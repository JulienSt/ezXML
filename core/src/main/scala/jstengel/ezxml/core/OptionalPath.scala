package jstengel.ezxml.core

import scala.annotation.tailrec
import scala.xml.{Elem, Node}


/**
 *
 * @param oPath
 */
case class OptionalPath (oPath : Option[WalkableXmlPath]) extends WalkableXmlPath {
    
    /**
     *
     * @param childNodeName
     * @return
     */
    override def \~ (childNodeName : String) : OptionalPath = oPath match {
        case Some(connector) => connector \~ childNodeName
        case None => this
    }
    
    /**
     *
     * @param childNodeName
     * @param predicate
     * @return
     */
    override def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \~ (childNodeName, predicate)
        case None => this
    }
    
    /**
     *
     * @param childNodeName
     * @return
     */
    override def \\~ (childNodeName: String): OptionalPath = oPath match {
        case Some(connector) => connector \\~ childNodeName
        case None => this
    }
    
    /**
     *
     * @param childNodeName
     * @param predicate
     * @return
     */
    override def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath = oPath match {
        case Some(connector) => connector \\~ (childNodeName, predicate)
        case None => this
    }
    
    /**
     *
     * @param children the nodes that will be added to the children of the elems at the end of the underlying path
     * @return the overall structure with a child added to all the elems,
     *         that where pointed to with the underlying Path
     */
    override def addChildren (children : Node*) : Option[Elem] = oPath.flatMap(_.addChildren(children: _*))
    
    /**
     *
     * @param predicate
     * @return
     */
    override def deleteChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.deleteChildren(predicate))
    
    /**
     *
     * @param predicate
     * @return
     */
    override def filterChildren (predicate: Node => Boolean): Option[Elem] = oPath.flatMap(_.filterChildren(predicate))
    
    /**
     *
     * @param f
     * @return
     */
    override def mapChildren (f: Node => Node): Option[Elem] = oPath.flatMap(_.mapChildren(f))
    
    /**
     *
     * @param f
     * @return
     */
    override def flatMapChildren (f: Node => Option[Node]): Option[Elem] = oPath.flatMap(_.flatMapChildren(f))
    
    /**
     *
     * @param f
     * @return
     */
    override def transformTarget (f: Elem => Elem): Option[Elem] = oPath.flatMap(_.transformTarget(f))

}

object OptionalPath {
    
    /**
     *
     * @param connector
     * @return
     */
    @tailrec def apply (connector : WalkableXmlPath) : OptionalPath = connector match {
        case OptionalPath(Some(connector)) => OptionalPath(connector)
        case oc @ OptionalPath(None)       => oc
        case walkableXmlConnector          => OptionalPath(Some(walkableXmlConnector))
    }
    
    /**
     *
     * @param pathList
     * @return
     */
    def apply (pathList : List[XmlPath]) : OptionalPath = pathList match {
        case Nil         => OptionalPath(None)
        case head :: Nil => OptionalPath(head)
        case _           => OptionalPath(XmlPathList(pathList))
    }
    
}
