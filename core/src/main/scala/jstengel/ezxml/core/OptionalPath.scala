package jstengel.ezxml.core

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


case class OptionalPath (oPath : Option[WalkableXmlPath]) extends WalkableXmlPath {
    
    override def \~ (childNodeName : String) : OptionalPath = oPath match {
        case Some(connector) => connector \~ childNodeName
        case None => this
    }
    override def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \~ (childNodeName, predicate)
        case None => this
    }

    override def \\~ (childNodeName: String): OptionalPath = oPath match {
        case Some(connector) => connector \\~ childNodeName
        case None => this
    }
    override def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \\~ (childNodeName, predicate)
        case None => this
    }
    
    override def addChildren (children : Node*) : Option[Elem] = oPath.flatMap(_.addChildren(children: _*))
    override def deleteChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.deleteChildren(predicate))
    override def filterChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.filterChildren(predicate))
    override def mapChildren (f: Node => Node): Option[Elem] = oPath.flatMap(_.mapChildren(f))
    override def flatMapChildren (f: Node => Option[Node]): Option[Elem] = oPath.flatMap(_.flatMapChildren(f))
    override def transformTarget (f: Elem => Elem): Option[Elem] = oPath.flatMap(_.transformTarget(f))

}

object OptionalPath {
    
    @tailrec def apply (connector : WalkableXmlPath) : OptionalPath = connector match {
        case OptionalPath(Some(connector)) => OptionalPath(connector)
        case oc @ OptionalPath(None)       => oc
        case walkableXmlConnector          => OptionalPath(Some(walkableXmlConnector))
    }
    
    def apply (pathList : List[XmlPath]) : OptionalPath = pathList match {
        case Nil         => OptionalPath(None)
        case head :: Nil => OptionalPath(head)
        case _           => OptionalPath(XmlPathList(pathList))
    }
    
}
