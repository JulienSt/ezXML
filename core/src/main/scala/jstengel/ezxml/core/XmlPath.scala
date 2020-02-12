package jstengel.ezxml.core

import SimpleWrapper.{ElemWrapper, NodeWrapper}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


case class XmlPath (parentElem   : Elem,
                    targetElem   : Elem,
                    targetIndex  : Int,
                    pathDepth    : Int,
                    ancestorPath : Option[XmlPath] = None) extends WalkableXmlPath {
    
    /** See [[WalkableXmlPath]] */
    override def \~ (childNodeName : String) : OptionalPath =
        OptionalPath(XmlPath \~ (targetElem, childNodeName, ancestorPath = Some(this)))
    
    /** See [[WalkableXmlPath]] */
    override def \~ (childNodeName : String, predicate : Elem => Boolean) : OptionalPath =
        OptionalPath(XmlPath \~ (targetElem, childNodeName, predicate, ancestorPath = Some(this)))
    
    /** See [[WalkableXmlPath]] */
    override def \\~ (childNodeName : String) : OptionalPath =
        OptionalPath(XmlPath \\~ (targetElem, childNodeName, ancestorPath = Some(this)))
    
    /** See [[WalkableXmlPath]] */
    override def \\~ (childNodeName : String, predicate : Elem => Boolean) : OptionalPath =
        OptionalPath(XmlPath \\~ (targetElem, childNodeName, predicate, ancestorPath = Some(this)))
    
    /**
     *
     * @param changedTarget
     * @param next
     * @return
     */
    @tailrec private def aggregateChange (changedTarget : Node, next : XmlPath) : Elem = {
        val Elem(pre, lbl, att, meta, children @ _*) = next.parentElem
        val aggregation                              = Elem(pre, lbl, att, meta, true, children.zipWithIndex.map{
            case (_ : Elem, i) if next.targetIndex == i => changedTarget
            case (n, _) => n
        } : _*)
        next.ancestorPath match {
            case Some(aPath) => aggregateChange(aggregation, aPath)
            case None        => aggregation
        }
    }
    
    /** See [[WalkableXmlPath]] */
    override def addChildren (children : Node*) : Option[Elem] =
        Some(aggregateChange(targetElem.addChildren(children : _*), this))
    
    /** See [[WalkableXmlPath]] */
    override def deleteChildren (predicate : Node => Boolean) : Option[Elem] =
        Some(aggregateChange(targetElem.deleteChildren(predicate), this))
    
    /** See [[WalkableXmlPath]] */
    override def filterChildren (predicate : Node => Boolean) : Option[Elem] =
        Some(aggregateChange(targetElem.filterChildren(predicate), this))
    
    /** See [[WalkableXmlPath]] */
    override def mapChildren (f : Node => Node) : Option[Elem] =
        Some(aggregateChange(targetElem.mapChildren(f), this))
    
    /** See [[WalkableXmlPath]] */
    override def flatMapChildren (f : Node => Option[Node]) : Option[Elem] =
        Some(aggregateChange(targetElem.flatMapChildren(f), this))
    
    /** See [[WalkableXmlPath]] */
    override def transformTarget (f : Elem => Elem) : Option[Elem] =
        Some(aggregateChange(targetElem.transformRoot(f), this))
    
    /**
     *
     * @return
     */
    def completePath : String = {
        @tailrec def aggregatePath (path : String, next : XmlPath) : String = {
            val currentSubPath = s"${ next.parentElem.root.toPrettyXMLString } \\ $path"
            next.ancestorPath match {
                case Some(aPath) => aggregatePath(currentSubPath, aPath)
                case None        => currentSubPath
            }
        }
        aggregatePath("", this)
    }
    
    override def toString : String = {
        s"XmlPath(${ targetElem.toPrettyXMLString }, $completePath, $targetIndex)"
    }
}


object XmlPath {
    
    val matchesLabel : String => Node => Boolean = childLabel => {
        case Elem(_, label, _, _, _*) => label == childLabel
        case _                        => false
    }
    
    def extractPath (parent : Elem,
                     target : String,
                     predicate : Elem => Boolean,
                     ancestorPath : Option[XmlPath]) : PartialFunction[(Node, Int), XmlPath] = {
        case (e : Elem, i) if e.hasCorrectLabel(target) && predicate(e) =>
            XmlPath(parent,
                    e,
                    i,
                    ancestorPath.map(_.pathDepth + 1).getOrElse(1),
                    ancestorPath)
    }
    
    val default : PartialFunction[(Node, Int), Option[XmlPath]] = {case _ => None}
    
    def \~ (parent : Elem,
            target : String,
            predicate : Elem => Boolean = _ => true,
            ancestorPath : Option[XmlPath] = None) : OptionalPath =
        OptionalPath(parent.child
                           .zipWithIndex
                           .flatMap{
                               extractPath(parent, target, predicate, ancestorPath).andThen(Some(_)) orElse
                               default
                           }
                           .toList)
    
    def \\~ (parent : Elem,
             target : String,
             predicate : Elem => Boolean = _ => true,
             ancestorPath : Option[XmlPath] = None) : OptionalPath = {
        
        def splitChildren (parent : Elem,
                           target : String,
                           ancestorPath : Option[XmlPath]) : (List[XmlPath], List[XmlPath]) =
            parent.child
                  .zipWithIndex
                  .partitionMap{
                      extractPath(parent, target, predicate, ancestorPath).andThen(Left(_)) orElse
                      extractPath(parent, "_", _ => true, ancestorPath).andThen(path => Right(Some(path))) orElse
                      default.andThen(Right(_))
                  } match {
                      case (foundPaths, bridgePaths) =>
                          val found = foundPaths.toList
                          (found, found ::: bridgePaths.flatten.toList)
                  }
        
        @tailrec
        def aggregate (foundPaths : List[XmlPath], bridgePaths : List[XmlPath]) : List[XmlPath] = {
            if ( bridgePaths.isEmpty )
                foundPaths
            else {
                val (founds, bridges) = bridgePaths.map(path => splitChildren(path.targetElem, target, Some(path)))
                                                   .fold(Nil, Nil){
                                                       case ((oldLeft, oldRight), (newLeft, newRight)) =>
                                                           (newLeft ::: oldLeft, newRight ::: oldRight)
                                                   }
                aggregate(founds ::: foundPaths, bridges)
            }
        }
        
        val (foundPaths, bridgePaths) = splitChildren(parent, target, ancestorPath)
        OptionalPath(aggregate(foundPaths, bridgePaths))
    }
    
    // move from level to level with "_" and collect all children which label matches the given string
    //    def \\
    
}
