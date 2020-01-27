package indv.jstengel.ezxml.core

import SimpleWrapper.ElemWrapper
import scala.annotation.tailrec
import scala.xml.{Elem, Node}


case class XmlPathList (paths : List[XmlPath]) extends WalkableXmlPath {

    private def traverse(traversalFunction : XmlPath => OptionalPath): OptionalPath = {
        OptionalPath(paths.flatMap{ path =>
            traversalFunction(path).oPath match {
                case Some(path : XmlPath)     => Some(List(path))
                case Some(list : XmlPathList) => Some(list.paths)
                case _                        => None
            }
        }.flatten)
    }

    override def \~ (childNodeName : String) : OptionalPath =
        traverse(_ \~ childNodeName)

    override def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
        traverse(_ \~ (childNodeName, predicate))

    override def \\~ (childNodeName: String): OptionalPath =
        traverse(_ \\~ childNodeName)

    override def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
        traverse(_ \\~ (childNodeName, predicate))

    override def toString : String = s"XmlPathList(\n ${paths.mkString(",\n")} \n)"
    
    /**
     * iterate over all paths of the current depth and cluster them into lists,
     * where each path has the exact same parent elem
     * @param unsorted
     * @return
     */
    private def clusterByParent(unsorted: List[XmlPath]): List[List[XmlPath]] = {
        val currentHead :: currentTail = unsorted
        currentTail.foldLeft(List(List(currentHead))){ (listsWithSameParent, path) =>
            var wasPut = false
            val merged = listsWithSameParent.map{
                case list @ head :: _ if head.parentElem   == path.parentElem &&
                                         head.ancestorPath == path.ancestorPath =>
                    wasPut = true
                    path :: list
                case list => list
            }
            /* has the path been put into a list where all other elems have the same parent? */
            if (wasPut) merged else List(path) :: listsWithSameParent
        }
    }
    
    /**
     * merge all paths inside the list under the same parent elem
     * @param clusteredLists
     * @return either single element List with the top level elem or a List of parent paths propagating the change
     *         (depending on the fact if there is a depth-level above or not)
     */
    private def mergePathsUnderParent (clusteredLists : List[List[XmlPath]]): List[Either[XmlPath, Elem]] = {
        clusteredLists.map(list => {
            val Elem(pre, lbl, att, meta, children @ _*) = list.head.parentElem
            val ancestorTarget = Elem(pre,
                                      lbl,
                                      att,
                                      meta,
                                      true,
                                      children.zipWithIndex.map{
                                          case (e : Elem, i) =>
                                              list.find(_.targetIndex == i)
                                                  .map(_.targetElem)
                                                  .getOrElse(e)
                                          case (n, _)        => n
                                      } : _*)
            list.head
                .ancestorPath
                .map{ case XmlPath(pe, _, ti, pd, ap) => Left(XmlPath(pe, ancestorTarget, ti, pd, ap)) }
                .getOrElse(Right(ancestorTarget))
        })
    }
    
    /**
     * merges all paths into an eventual top parent elem
     * @param pathsOnCurrentDepth
     * @param nextPaths
     * @return
     */
    @tailrec private def bubbleUpwards (pathsOnCurrentDepth : List[XmlPath], nextPaths : List[List[XmlPath]]) : Elem = {
        val ancestorPathsWithPropagatedChange = (clusterByParent _ andThen mergePathsUnderParent)(pathsOnCurrentDepth)
        ancestorPathsWithPropagatedChange.head match {
            case Left(path)    =>
                /* since the head is Left, all elements will be Left. Therefore, the List can be flattened */
                val ancestorPaths = ancestorPathsWithPropagatedChange.map{ guaranteedLeft =>
                    val Left(xmlPath) = guaranteedLeft
                    xmlPath
                }
                if ( nextPaths.nonEmpty && nextPaths.head.head.pathDepth == path.pathDepth )
                    bubbleUpwards(ancestorPaths ::: nextPaths.head, nextPaths.tail)
                else
                    bubbleUpwards(ancestorPaths, nextPaths)
            case Right(result) => result
        }
    }
    
    /**
     * partitions the input List into a Sets of paths with the same height
     * @param in a List[XmlPath] that is sorted via XmlPath.pathDepth (must be sorted from min to max depth)
     * @param partitioned a List[ List[XmlPath] ], where each inner List only contains XmlPaths with the same depth
     * @return partitioned, when there is no more path inside the input list
     */
    @tailrec private def part(in: List[XmlPath], partitioned: List[List[XmlPath]]): List[List[XmlPath]] = in match {
        case Nil                                                               => partitioned
        case head :: tail if partitioned.head.head.pathDepth == head.pathDepth =>
            part(tail, (head :: partitioned.head) :: partitioned.tail)
        case head :: tail                                                      => part(tail, List(head) :: partitioned)
    }
    
    // fürs verändern werden alle pfade solange hoch gebubbelt, bis man beim gleichen ancestor ankommt
    // dort werden die bisherigen änderungen gemerget und dann einfach weiter hoch-gebubbelt
    /**
     *
     * @param f
     * @return
     */
    private def changeTargets (f: Elem => Elem) : Option[Elem] = {
        if (paths.isEmpty)
            None
        else {
            val pathsWithChangedTarget = paths.map{ case XmlPath(pe, te, ti, pd, ap) =>
                XmlPath(pe, f(te), ti, pd, ap)
            }
            val presorted = pathsWithChangedTarget.sortWith{case (p1, p2) => p1.pathDepth < p2.pathDepth}
            val partition = part(presorted.tail, List(List(presorted.head)))
            Some(bubbleUpwards(partition.head, partition.tail))
        }
    }
    
    

    override def addChildren (children : Node*) : Option[Elem] = changeTargets(_.addChildren(children: _*))

    override def deleteChildren (predicate: Node => Boolean): Option[Elem] = changeTargets(_.deleteChildren(predicate))

    override def filterChildren (predicate: Node => Boolean): Option[Elem] = changeTargets(_.filterChildren(predicate))

    override def mapChildren (f: Node => Node): Option[Elem] = changeTargets(_.mapChildren(f))

    override def flatMapChildren (f: Node => Option[Node]): Option[Elem] = changeTargets(_.flatMapChildren(f))

    override def transformTarget (f: Elem => Elem): Option[Elem] = changeTargets(_.transformRoot(f))

}
