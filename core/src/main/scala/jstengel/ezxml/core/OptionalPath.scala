package jstengel.ezxml.core

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


/**
 * This class represents a possible successful or failed search with the functions [[WalkableXmlPath.\~]] and
 * [[WalkableXmlPath.\\~]]
 * By using this middleman, a program using this library doesn't automatically fail when using a wrong search path
 * @param oPath holds Some[WalkableXmlPath], if the current searches where successful and None, if that wasn't the case
 */
case class OptionalPath (oPath : Option[WalkableXmlPath]) extends WalkableXmlPath {

    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName : String) : OptionalPath = oPath match {
        case Some(connector) => connector \~ childNodeName
        case None => this
    }

    /** @see [[WalkableXmlPath.\~]] */
    override def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \~ (childNodeName, predicate)
        case None => this
    }

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName: String): OptionalPath = oPath match {
        case Some(connector) => connector \\~ childNodeName
        case None => this
    }

    /** @see [[WalkableXmlPath.\\~]] */
    override def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \\~ (childNodeName, predicate)
        case None => this
    }

    /** @see [[WalkableXmlPath.addChildren]] */
    override def addChildren (children : Node*) : Option[Elem] = oPath.flatMap(_.addChildren(children: _*))

    /** @see [[WalkableXmlPath.deleteChildren]] */
    override def deleteChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.deleteChildren(predicate))

    /** @see [[WalkableXmlPath.filterChildren]] */
    override def filterChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.filterChildren(predicate))

    /** @see [[WalkableXmlPath.mapChildren]] */
    override def mapChildren (f: Node => Node): Option[Elem] = oPath.flatMap(_.mapChildren(f))

    /** @see [[WalkableXmlPath.flatMapChildren]] */
    override def flatMapChildren (f: Node => IterableOnce[Node]): Option[Elem] = oPath.flatMap(_.flatMapChildren(f))
    
    /** @see [[WalkableXmlPath.transformTarget]] */
    override def transformTarget (f: Elem => Elem): Option[Elem] = oPath.flatMap(_.transformTarget(f))
    
    /** @see [[WalkableXmlPath.transformTargetRoot]] */
    override def transformTargetRoot (f: Elem => Elem): Option[Elem] = oPath.flatMap(_.transformTarget(f))
    

}

object OptionalPath {
    
    /**
     * This apply method creates the tightest wrapping inside [[OptionalPath]] for a [[WalkableXmlPath]].
     *  - It flattens OptionalPath(OptionalPath(...)) until, there is no more nesting and only the outer most
     *    OptionalPath remains
     *  - OptionalPath(None) will remain the same reference
     *  - any other [[WalkableXmlPath]] will be wrapped by only one [[OptionalPath]]
     * @param connector a walkable path that will be wrapped with OptionalPath
     * @return tightest wrapping inside [[OptionalPath]] for a [[WalkableXmlPath]]
     */
    @tailrec def apply (connector : WalkableXmlPath) : OptionalPath = connector match {
        case OptionalPath(Some(connector)) => OptionalPath(connector)
        case oc @ OptionalPath(None)       => oc
        case walkableXmlConnector          => OptionalPath(Some(walkableXmlConnector))
    }
    
    /**
     * This function translates the different results of traversing a xml structure with [[WalkableXmlPath.\~]] and
     * [[WalkableXmlPath.\\~]] into a equivalent [[OptionalPath]], that can then be used to further search the
     * remaining xml structure
     * @param pathList a List of [[XmlPath]], that is the result of traversing a [[WalkableXmlPath]]
     * @return best fitting [[OptionalPath]] for pathList
     */
    def apply (pathList : List[XmlPath]) : OptionalPath = pathList match {
        case Nil         => OptionalPath(None)
        case head :: Nil => OptionalPath(head)
        case _           => OptionalPath(XmlPathList(pathList))
    }
    
}
