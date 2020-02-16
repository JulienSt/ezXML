package jstengel.ezxml.core

import jstengel.ezxml.core.SimpleWrapper.{ElemWrapper, NodeWrapper}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

/**
 * This class represents the most basic form of a WalkableXmlPath.
 * All the other implementations of WalkableXmlPath are using this class to achieve their ways in traversing a
 * xml-structure
 * @param parentElem the original parent Elem of the target
 * @param targetElem the Elem, that was either directly searched for by [[WalkableXmlPath.\~]],
 *                   or [[WalkableXmlPath.\\~]] or is a bridge to the actual target of those functions
 * @param targetIndex the index for the targetElem, when seen as a child in [[Elem.child]]
 * @param pathDepth the depth of targetElem in the xml-structure. The higher the number the deeper the level
 * @param ancestorPath the path leading to this [[XmlPath]] inside the overall xml-structure
 */
case class XmlPath (parentElem    : Elem,
                    targetElem    : Elem,
                    targetIndex   : Int,
                    pathDepth     : Int,
                    ancestorPath  : Option[XmlPath] = None,
                    changesTarget : Boolean = false) extends WalkableXmlPath {
    
    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName : String) : OptionalPath =
        OptionalPath(XmlPath \~ (targetElem, childNodeName, ancestorPath = Some(this)))

    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName : String, predicate : Elem => Boolean) : OptionalPath =
        OptionalPath(XmlPath \~ (targetElem, childNodeName, predicate, ancestorPath = Some(this)))

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName : String) : OptionalPath =
        OptionalPath(XmlPath \\~ (targetElem, childNodeName, ancestorPath = Some(this)))

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName : String, predicate : Elem => Boolean) : OptionalPath =
        OptionalPath(XmlPath \\~ (targetElem, childNodeName, predicate, ancestorPath = Some(this)))
    
    /**
     * This function aggregates the change made to this [[XmlPath]] into the depth-level above and then bubbles the
     * change until it reaches the root and a newly changed xml-structure is created
     * @param changedTarget the target that was changed by some outside function
     * @param next the XmlPath to which the change will be passed on.
     *             This will be the [[ancestorPath]] in most cases, but to make this function tail recursive, it was
     *             included as parameter
     * @return the xml-structure complete from the root with the changes made to the target of this XmlPath
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

    /** @see [[WalkableXmlPath.addChildren]] */
    override def addChildren (children : Node*) : Option[Elem] =
        Some(aggregateChange(targetElem.addChildren(children : _*), this))

    /** @see [[WalkableXmlPath.deleteChildren]] */
    override def deleteChildren (predicate : Node => Boolean) : Option[Elem] =
        Some(aggregateChange(targetElem.deleteChildren(predicate), this))

    /** @see [[WalkableXmlPath.filterChildren]] */
    override def filterChildren (predicate : Node => Boolean) : Option[Elem] =
        Some(aggregateChange(targetElem.filterChildren(predicate), this))

    /** @see [[WalkableXmlPath.mapChildren]] */
    override def mapChildren (f : Node => Node) : Option[Elem] =
        Some(aggregateChange(targetElem.mapChildren(f), this))

    /** @see [[WalkableXmlPath.flatMapChildren]] */
    override def flatMapChildren (f : Node => IterableOnce[Node]) : Option[Elem] =
        Some(aggregateChange(targetElem.flatMapChildren(f), this))

    /** @see [[WalkableXmlPath.transformTarget]] */
    override def transformTarget (f : Elem => Elem) : Option[Elem] =
        Some(aggregateChange(f(targetElem), this))
    
    /** @see [[WalkableXmlPath.transformTargetRoot]] */
    override def transformTargetRoot (f : Elem => Elem) : Option[Elem] =
        Some(aggregateChange(targetElem.transformRoot(f), this))
    
    /**
     * this is a helpful function to determine, if two XmlPaths target the same original Elem.
     * @param other the [[XmlPath]] "this" will be compared to, to determine if both want to change the same target
     * @return true, if both want to change the same target, false if that is not the case
     */
    def hasSameTarget (other : XmlPath): Boolean =
        ancestorPath == other.ancestorPath && targetIndex == other.targetIndex
    
    /**
     * @return a string representation of the complete [[ancestorPath]]
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
    
    override def toString : String = s"XmlPath(${ targetElem.toPrettyXMLString }, $completePath, $targetIndex)"

}


object XmlPath {

    /**
     * creates a partial function with which a [[XmlPath]] is created out of a Node and former Index of said node
     * @param parent the parent of the node that will be converted to a XmlPath
     * @param target the name of the target [[Elem]]
     * @param predicate this function has to be true for the input node of the partial function, to be defined for the
     *                  node
     * @param ancestorPath this will be the [[XmlPath.ancestorPath]] of the created XmlPath
     * @return a partial function for a Tuple2[Node, Int] with the node that might be converted to a XmlPath and the
     *         index of the node, when seen as a child of "parent"
     */
    def extractPath (parent : Elem,
                     target : String,
                     predicate : Elem => Boolean,
                     ancestorPath : Option[XmlPath]) : PartialFunction[(Node, Int), XmlPath] = {
        case (e : Elem, i) if e.hasCorrectLabel(target) && predicate(e) =>
            XmlPath(parent,
                    e,
                    i,
                    ancestorPath.map(_.pathDepth + 1).getOrElse(1),
                    ancestorPath,
                    changesTarget = true)
    }

    /* */
    private val default : PartialFunction[(Node, Int), Option[XmlPath]] = {case _ => None}

    /**
     * This is the actual implementation of [[XmlPath.\~]].
     * This function checks the children of "parent" for matching target and predicate.
     * Each child of parent fitting the defined parameters will be converted to a XmlPath and then included in the
     * resulting [[OptionalPath]]
     * @see [[WalkableXmlPath.\~]] for more information
     * @param parent children of this [[Elem]] will be checked for the defined parameters
     * @param target a child has to have a label matching this parameter to be included in the final result.
     *               Alternatively "_" can be used as input, so that only the predicate decides, which nodes will be
     *               included
     * @param predicate this function has to be true for the child to be included in the final result
     * @param ancestorPath  this will be the [[XmlPath.ancestorPath]] of the potentially created XmlPath
     * @return a [[OptionalPath]] containing all children of "parent" as [[XmlPath]], which fit the defined search
     *         parameters
     */
    def \~ (parent       : Elem,
            target       : String,
            predicate    : Elem => Boolean = _ => true,
            ancestorPath : Option[XmlPath] = None) : OptionalPath =
        OptionalPath(parent.child
                           .zipWithIndex
                           .flatMap{
                               extractPath(parent, target, predicate, ancestorPath).andThen(Some(_)) orElse
                               default
                           }
                           .toList)
    
    /**
     * This is the actual implementation of [[XmlPath.\\~]].
     * This function checks the children and children of children of "parent" for matching target and predicate.
     * Each child and children's child of parent fitting the defined parameters will be converted to a XmlPath and then
     * included in the resulting [[OptionalPath]]
     * @see [[WalkableXmlPath.\\~]] for more information
     * @param parent children and children of children of this [[Elem]] will be checked for the defined parameters
     * @param target a child has to have a label matching this parameter to be included in the final result.
     *               Alternatively "_" can be used as input, so that only the predicate decides, which nodes will be
     *               included
     * @param predicate this function has to be true for the child to be included in the final result
     * @param ancestorPath  this will be the [[XmlPath.ancestorPath]] of the potentially created XmlPath of the most
     *                      shallow depth. All further ancestorPaths for deeper levels will be calculated automatically.
     * @return a [[OptionalPath]] containing all children of "parent" as [[XmlPath]], which fit the defined search
     *         parameters
     */
    def \\~ (parent       : Elem,
             target       : String,
             predicate    : Elem => Boolean = _ => true,
             ancestorPath : Option[XmlPath] = None) : OptionalPath = {
    
        /**
         * Given the [[Elem]] parent, all children of parent will be converted into a XmlPath, that either fits
         * the defined search parameters of [[target]] and [[predicate]] or doesn't fit these parameters.
         * The ones fitting these parameters are accumulated into a List.
         * The ones that don't fit these parameters, but still have children, that could fit these parameters are
         * accumulated into another list.
         * The ones that neither fit the parameters, nor have any children are then discarded from further search
         * @param parent the children of this elem will be sorted into fitting and not fitting
         * @param ancestorPath this will be the [[XmlPath.ancestorPath]] of the resulting [[XmlPath]]
         * @return a Tuple2 of two List[XmlPath], where all [[XmlPath]] of the List in [[Tuple2._1]] fit the defined
         *         search parameters, and all [[XmlPath]] of the List in [[Tuple2._2]] could potentially hold
         *         further children fitting the search parameters
         */
        def splitChildren (parent       : Elem,
                           ancestorPath : Option[XmlPath]) : (List[XmlPath], List[XmlPath]) =
            parent.child
                  .zipWithIndex
                  .partitionMap{
                      extractPath(parent, target, predicate, ancestorPath)
                          .andThen(Left(_)) orElse
                      extractPath(parent, "_", e => e.child.nonEmpty, ancestorPath)
                          .andThen(path => Right(Some(path))) orElse
                      default
                          .andThen(Right(_))
                  } match {
                      case (foundPaths, bridgePaths) =>
                          val found = foundPaths.toList
                          (found, found ::: bridgePaths.flatten.toList)
                  }
    
        /**
         * This function aggregates all the paths fitting the defined search parameters, while discarding all other
         * paths step by step.
         * @param foundPaths a List of paths fitting the defined search parameters of [[target]] and [[predicate]]
         * @param bridgePaths a List of paths, that could potentially hold children firring the search parameter.
         *                    Hence the name bridgePaths, as they are a potential bridge to the actually searched for
         *                    paths
         * @return a List of all Paths matching the defined search parameters of [[target]] and [[predicate]]
         */
        @tailrec def aggregate (foundPaths : List[XmlPath], bridgePaths : List[XmlPath]) : List[XmlPath] = {
            if ( bridgePaths.isEmpty )
                foundPaths
            else {
                val (founds, bridges) = bridgePaths.map( path => splitChildren(path.targetElem, Some(path)))
                                                   .fold(Nil, Nil){
                                                       case ((oldLeft, oldRight), (newLeft, newRight)) =>
                                                           (newLeft ::: oldLeft, newRight ::: oldRight)
                                                   }
                aggregate(founds ::: foundPaths, founds ::: bridges)
            }
        }
        
        val (foundPaths, bridgePaths) = splitChildren(parent, ancestorPath)
        OptionalPath(aggregate(foundPaths, bridgePaths))
    }
    
}
