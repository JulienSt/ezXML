package jstengel.ezxml.core

import scala.xml.{Elem, Node}


trait WalkableXmlPath {
    
    /**
     * navigates a path similar to the method "\" from [[scala.xml.NodeSeq]]
     * but instead of just returning the elems at the end of the path without information the parent elems,
     * this method returns a complete path with all information to the parent xml-structure,
     * to change elems along the way.
     * @param childNodeName
     * @return
     */
    def \~ (childNodeName : String) : OptionalPath
    def \~ (childNodeName : String, predicate: Elem => Boolean) : OptionalPath

    def \\~ (childNodeName : String) : OptionalPath
    def \\~ (childNodeName : String, predicate: Elem => Boolean) : OptionalPath

    /**
     * adds Children to the elem where the underlying path is pointing and then returns the complete structure
     * @param children the nodes that will be added to the children of the elems at the end of the underlying path
     * @return the overall structure with a child added to all the elems,
     *         that where pointed to with the underlying Path
     */
    def addChildren (children : Node*) : Option[Elem]

    def filterChildren (predicate: Node => Boolean) : Option[Elem]

    def deleteChildren (predicate: Node => Boolean): Option[Elem]

    def mapChildren (f: Node => Node) : Option[Elem]

    def flatMapChildren (f: Node => Option[Node]) : Option[Elem]

    /* changes only the targets, without the children
     *  child nodes can not even be accessed with the given function */
    def transformTarget (f: Elem => Elem): Option[Elem]

}