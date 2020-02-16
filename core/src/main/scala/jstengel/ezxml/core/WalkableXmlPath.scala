package jstengel.ezxml.core

import scala.xml.{Elem, Node}


trait WalkableXmlPath {
    
    /**
     * navigates a path similar to the method "\" from [[scala.xml.Node]], while retaining the
     * tree information of the parent structure.
     * So, instead of just returning the elems at the end of the path without information of the parent elems,
     * this method returns a complete path with all information to the parent xml-structure.
     * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
     * @param childNodeName the name of the child that is searched for
     * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
     */
    def \~ (childNodeName : String) : OptionalPath

    /**
     * navigates a path similar to the method "\" from [[scala.xml.Node]], while retaining the
     * tree information of the parent structure.
     * So, instead of just returning the elems at the end of the path without information of the parent elems,
     * this method returns a complete path with all information to the parent xml-structure.
     * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
     * @param childNodeName the name of the child that is searched for
     * @param predicate filtering function for the searched element. Only includes child node that returns true
     *                  for the predicate
     * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
     */
    def \~ (childNodeName : String, predicate: Elem => Boolean) : OptionalPath

    /**
     * navigates a path similar to the method "\\" from [[scala.xml.Node]], while retaining the
     * tree information of the parent structure.
     * So, instead of just returning the elems at the end of the path without information of the parent elems,
     * this method returns a complete path with all information to the parent xml-structure.
     * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
     * @param childNodeName the name of the child that is searched for
     * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
     */
    def \\~ (childNodeName : String) : OptionalPath

    /**
     * navigates a path similar to the method "\\" from [[scala.xml.Node]], while retaining the
     * tree information of the parent structure.
     * So, instead of just returning the elems at the end of the path without information of the parent elems,
     * this method returns a complete path with all information to the parent xml-structure.
     * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
     * @param childNodeName the name of the child that is searched for
     * @param predicate filtering function for the searched element. Only includes child node that returns true
     *                  for the predicate
     * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
     */
    def \\~ (childNodeName : String, predicate: Elem => Boolean) : OptionalPath

    /**
     * adds Children to the elem where the underlying path is pointing and then returns the complete structure
     * @param children the nodes that will be added to the children of the elems at the end of the underlying path
     * @return the overall structure with a child added to all the elems,
     *         that where pointed to with the underlying Path
     */
    def addChildren (children : Node*) : Option[Elem]

    /**
     * Filters through the children of the root of elem and then returns the root node with these filtered children.
     * Only direct children will be filtered. Children of children will be ignored
     * @param predicate the function with which the children will be filtered.
     *                  If the child node passes the predicate with true, it will be kept in the return value
     * @return the root element with the children of the targets filtered
     */
    def filterChildren (predicate: Node => Boolean) : Option[Elem]

    /**
     * Deletes children from the root of elem. and returns a new [[Elem]] without these deleted children.
     * Only direct children will be deleted. Children of children will be ignored
     * @param predicate the function with which the children will be selected for deletion.
     *                  If the child node passes the predicate with true, it will be gone in the return value
     * @return the root element without the deleted children of the targets
     */
    def deleteChildren (predicate: Node => Boolean): Option[Elem]

    /**
     * This method maps all the children of the root node and then returns the root with these children.
     * @param f this function is used during the mapping process to transform one node to another
     * @return root with all the children of targets transformed
     */
    def mapChildren (f: Node => Node) : Option[Elem]

    /**
     * This method flat maps all the children of the root node and then returns the
     * root with these transformed children.
     * @param f this function is used during the mapping process to transform one node to a
     *          IterableOnce[Node], which will then be flattened
     * @return the root node where the children of targets are flat mapped
     */
    def flatMapChildren (f: Node => IterableOnce[Node]) : Option[Elem]

    /**
     * changes the targets, with the children
     * child nodes can be accessed with the given function
     * @param f the function that transforms the target node
     * @return the transformed targets
     */
    def transformTarget (f: Elem => Elem): Option[Elem]
    
    /**
     * changes only the targets, without the children
     * child nodes can not even be accessed with the given function
     * @param f the function that transforms the target node
     * @return the transformed targets with all the previous children
     */
    def transformTargetRoot (f: Elem => Elem): Option[Elem]

}
