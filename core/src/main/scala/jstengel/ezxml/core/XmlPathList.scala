package jstengel.ezxml.core

import jstengel.ezxml.core.SimpleWrapper.ElemWrapper

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


/**
 * This class holds multiple [[XmlPath]]s in a list and in that way represents a splitting search path, created
 * by the functions [[WalkableXmlPath.\~]] or [[WalkableXmlPath.\\~]]
 * @param paths all [[XmlPath]]s that were selected to previous use of [[WalkableXmlPath.\~]] or [[WalkableXmlPath.\\~]]
 */
case class XmlPathList (paths : List[XmlPath]) extends WalkableXmlPath {
    
    /**
     * traverses every path inside [[paths]] with the same method of traversal
     * @param traversalFunction this function is either [[WalkableXmlPath.\~]] or [[WalkableXmlPath.\\~]]
     * @return a new OptionalPath, with the WalkableXmlPath targeted by [[WalkableXmlPath.\~]] and
     *         [[WalkableXmlPath.\\~]]
     */
    private def traverse(traversalFunction : XmlPath => OptionalPath): OptionalPath = {
        OptionalPath(paths.flatMap{ path =>
            traversalFunction(path).oPath match {
                case Some(path : XmlPath)     => Some(List(path))
                case Some(list : XmlPathList) => Some(list.paths)
                case _                        => None
            }
        }.flatten)
    }

    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName : String) : OptionalPath =
        traverse(_ \~ childNodeName)

    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
        traverse(_ \~ (childNodeName, predicate))

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName: String): OptionalPath =
        traverse(_ \\~ childNodeName)

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
        traverse(_ \\~ (childNodeName, predicate))

    override def toString : String = s"XmlPathList(\n ${paths.mkString(",\n")} \n)"
    
    /**
     * iterate over all paths of the current depth and cluster them into lists,
     * where each path has the exact same parent elem
     * @param unsorted a list with all XmlPaths on the same depth
     * @return a List of List of [[XmlPath]], where every path in the inner List has the same ancestors and is therefor
     *         able to merge in the next step under the same parent elem.
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
     * merge all paths inside the list under the same parent elem and then apply f to each path that was a previous
     * member of [[paths]]
     * @param clusteredLists the output of [[clusterByParent]]
     * @param f the function that changes the xml elems along the way.
     * @return either single element List with the top level elem or a List of parent paths propagating the change
     *         (depending on the fact if there is a depth-level above or not)
     *         The result is only ever of one sub type of [[Either]]. So, either a List with only [[Left[XmlPath] ]] or
     *         a List with only one [[Right[Elem] ]]
     */
    private def mergeAndChangePaths (clusteredLists : List[List[XmlPath]],
                                     f              : Elem => Elem): List[Either[XmlPath, Elem]] =
        clusteredLists.map( list => {
            val Elem(pre, lbl, att, meta, children @ _*) = list.head.parentElem
            val ancestorTarget = Elem(pre,
                                      lbl,
                                      att,
                                      meta,
                                      true,
                                      children.zipWithIndex.map{
                                          case (e : Elem, i) =>
                                              list.find(_.targetIndex == i) /* find the matching elem to e in the clustered Elems */
                                                  .map{
                                                      case XmlPath(_, te, _, _, _, true) => f(te)
                                                      case path => path.targetElem
                                                  }
                                                  .getOrElse(e)
                                          case (n, _)        => n
                                      } : _*)
            list.head
                .ancestorPath
                .map{ case XmlPath(pe, _, ti, pd, ap, _) =>
                    /* when we apply a change to the final targets, the parents value for "changesTarget" might still
                       be true, because it was a target during the searching process. This value now interferes with
                       the merging process and leads to unwanted changes, therefor it is ignored */
                    Left(XmlPath(pe, ancestorTarget, ti, pd, ap))
                }
                .getOrElse(Right(ancestorTarget))
        })
    
    /**
     * merges all paths into an eventual top parent elem
     * Merges the paths level by level into
     * @param pathsOnCurrentDepth This List was a former inner List of nextPaths, and contains [[XmlPath]]s of the
     *                            currently deepest level. The function f will be applied to this list first, before
     *                            they are merged among themselves and past to the along to the next height.
     * @param nextPaths a List of List of XmlPath, where each inner List only holds XmlPath with the same depth
     * @param f the function that changes the xml elems along the way.
     * @return the original root node, where all targeted [[Elem]]s have been changed according to f
     */
    @tailrec private def bubbleUpwards (pathsOnCurrentDepth : List[XmlPath],
                                        nextPaths : List[List[XmlPath]],
                                        f : Elem => Elem) : Elem = {
        val clusteredParents        = clusterByParent(pathsOnCurrentDepth)
        val ancestorPathsWithChange = mergeAndChangePaths(clusteredParents, f)
        ancestorPathsWithChange.head match {
            case Left(path)    =>
                /* since the head is Left, all elements will be Left. Therefore, the List can be flattened */
                val ancestorPathsWithChanges = ancestorPathsWithChange.map{ guaranteedLeft =>
                    val Left(xmlPath) = guaranteedLeft
                    xmlPath
                }
                if ( nextPaths.nonEmpty && nextPaths.head.head.pathDepth == path.pathDepth ) {
                    val (duplicatePaths, filteredNextPaths) =
                        nextPaths.head.partition(next => ancestorPathsWithChanges.exists(_.hasSameTarget(next)))
                    val changingAncestors = ancestorPathsWithChanges.map(aP => {
                        if (duplicatePaths.exists(_.hasSameTarget(aP)))
                            aP.copy(changesTarget = true)
                        else
                            aP
                    })
                    bubbleUpwards(changingAncestors ::: filteredNextPaths, nextPaths.tail, f)
                } else
                    bubbleUpwards(ancestorPathsWithChanges, nextPaths, f)
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
        case head :: tail                                                      =>
            part(tail, List(head) :: partitioned)
    }
    
    /**
     * Changes every target of the XmlPaths inside [[paths]] and then bubbles these changes upwards the xml structure.
     * @param f the function that will be applied to every target in [[paths]]
     * @return a completely new xml-structure, where the desired changes have been made
     */
    private def changeTargets (f: Elem => Elem) : Option[Elem] = {
        if (paths.isEmpty)
            None
        else {
            /* sorting the list with smallest depth first, because the partitioning reverses this orientation */
            val presorted = paths.sortWith{case (p1, p2) => p1.pathDepth < p2.pathDepth}
            val partition = part(presorted.tail, List(List(presorted.head)))
            Some(bubbleUpwards(partition.head, partition.tail, f))
        }
    }

    /** @see [[WalkableXmlPath.addChildren]] */
    override def addChildren (children : Node*) : Option[Elem] = changeTargets(_.addChildren(children: _*))

    /** @see [[WalkableXmlPath.deleteChildren]] */
    override def deleteChildren (predicate: Node => Boolean): Option[Elem] = changeTargets(_.deleteChildren(predicate))

    /** @see [[WalkableXmlPath.filterChildren]] */
    override def filterChildren (predicate: Node => Boolean): Option[Elem] = changeTargets(_.filterChildren(predicate))

    /** @see [[WalkableXmlPath.mapChildren]] */
    override def mapChildren (f: Node => Node): Option[Elem] = changeTargets(_.mapChildren(f))

    /** @see [[WalkableXmlPath.flatMapChildren]] */
    override def flatMapChildren (f: Node => IterableOnce[Node]): Option[Elem] = changeTargets(_.flatMapChildren(f))

    /** @see [[WalkableXmlPath.transformTarget]] */
    override def transformTarget (f: Elem => Elem): Option[Elem] = changeTargets(f)

    /** @see [[WalkableXmlPath.transformTargetRoot]] */
    override def transformTargetRoot (f: Elem => Elem): Option[Elem] = changeTargets(_.transformRoot(f))

}
