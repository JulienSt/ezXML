package jstengel.ezxml.core

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


/**
 * todo
 * Represents
 * @param oPath
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

}

object OptionalPath {
    
    /**
     * todo
     * @param connector
     * @return
     */
    @tailrec def apply (connector : WalkableXmlPath) : OptionalPath = connector match {
        case OptionalPath(Some(connector)) => OptionalPath(connector)
        case oc @ OptionalPath(None)       => oc
        case walkableXmlConnector          => OptionalPath(Some(walkableXmlConnector))
    }
    
    /**
     * todo
     * @param pathList
     * @return
     */
    def apply (pathList : List[XmlPath]) : OptionalPath = pathList match {
        case Nil         => OptionalPath(None)
        case head :: Nil => OptionalPath(head)
        case _           => OptionalPath(XmlPathList(pathList))
    }
    
}
