package jstengel.ezxml.core

import scala.xml.{Attribute, Elem, Node, NodeSeq, Null, PrettyPrinter, Text}

object SimpleWrapper {

    private[this] val pp = new PrettyPrinter(400, 4)
    
    implicit class ElemWrapper (elem : Elem) {
        
        /** returns just elem without any children */
        def root: Elem = {
            val Elem(pre, lbl, att, meta, _*) = elem
            Elem(pre, lbl, att, meta, true, Seq(): _*)
        }
    
        /**
         * Adds an [[Attribute]] without prefix to elem. If an attribute with the same key is already present, the value
         * and prefix will be overwritten.
         * @param key the key of the attribute
         * @param value the value of the attribute
         * @return elem with a newly set attribute
         */
        def setAttribute (key : String, value : String) : Elem = {
            elem % Attribute(key, Text(value), Null)
        }
        
        /**
         * Adds an [[Attribute]] with prefix to elem. If an attribute with the same key is already present, the value
         * and prefix will be overwritten.
         * @param pre the prefix to the attribute
         * @param key the key of the attribute
         * @param value the value of the attribute
         * @return elem with a newly set attribute
         */
        def setAttWithPre (pre: String, key : String, value : String) : Elem = {
            elem % Attribute(pre, key, Text(value), Null)
        }
    
        /**
         * Adds multiple [[Attribute]]s to elem at the same time or updates the values of existing keys.
         * These Attributes are without prefix
         * @note make sure to have distinct keys for each pair, as they would overwrite each other
         * @param pairs attributes without prefix as [[Tuple2]] varargs, with the order key -> Attribute
         * @return elem, where the given pairs are now attributes
         */
        def setAttributes (pairs : (String, String)*) : Elem =
            elem % pairs.tail.foldLeft({
                val (key, value) = pairs.head
                Attribute(key, Text(value), Null)
            }){ case (previous, (key, value)) => Attribute(key, Text(value), previous) }
    
        /**
         * Adds multiple [[Attribute]]s to elem at the same time or updates the values of existing keys.
         * These attributes are with prefix
         * @note make sure to have distinct keys for each pair, as they would overwrite each other
         * @param triples attributes with prefix as [[Tuple3]] varargs, with the order prefix -> key -> Attribute
         * @return elem, where the given triples are now attributes
         */
        def setAttsWithPre (triples : (String, String, String)*) : Elem =
            elem % triples.tail.foldLeft({
                val (pre, key, value) = triples.head
                Attribute(pre, key, Text(value), Null)
            }){ case (previous, (pre, key, value)) => Attribute(pre, key, Text(value), previous) }
    
        /**
         * Filters through the children of the root of elem and then returns the root node with these filtered children.
         * Only direct children will be filtered. Children of children will be ignored
         * @param predicate the function with which the children will be filtered.
         *                  If the child node passes the predicate with true, it will be kept in the return value
         * @return the root element with filtered children
         */
        def filterChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filter(predicate) : _*)
        }
    
        /**
         * Deletes children from the root of elem. and returns a new [[Elem]] without these deleted children.
         * Only direct children will be deleted. Children of children will be ignored
         * @param predicate the function with which the children will be selected for deletion.
         *                  If the child node passes the predicate with true, it will be gone in the return value
         * @return the root element without the deleted children
         */
        def deleteChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filterNot(predicate) : _*)
        }
    
        /**
         * adds children to the root node of elem and then returns the root node with these added children
         * @param nodes the nodes that will be added to the other children of elem
         * @return elem with all the newly added nodes
         */
        def addChildren (nodes : Node*) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children ++ nodes : _*)
        }
    
        /**
         * This method maps all the children of the root node and then returns the root with these children.
         * @param transform this function is used during the mapping process to transform one node to another
         * @return elem with all the children transformed
         */
        def mapChildren (transform: Node => Node) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.map(transform) : _*)
        }
    
        /**
         * This method flat maps all the children of the root node and then returns the
         * root with these transformed children.
         * @param transform this function is used during the mapping process to transform one node to a
         *                  IterableOnce[Node], which will then be flattened
         * @return the root node where the children are flat mapped
         */
        def flatMapChildren (transform: Node => IterableOnce[Node]) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.flatMap(transform) : _*)
        }
        
        /**
         * changes only the root, without the children.
         * child nodes can not even be accessed with the given function
         * @param f the function that transforms the root node of elem
         * @return the transformed root node of elem with all the previous children of elem
         */
        def transformRoot (f: Elem => Elem): Elem = {
            val Elem(pre, lbl, att, meta, _*) = f(root)
            Elem(pre, lbl, att, meta, true, elem.child : _*)
        }

        /**
         * navigates a path similar to the method "\" from [[scala.xml.Node]], while retaining the
         * tree information of the parent structure.
         * So, instead of just returning the elems at the end of the path without information of the parent elems,
         * this method returns a complete path with all information to the parent xml-structure.
         * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
         * @param childNodeName the name of the child that is searched for
         * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
         */
        def \~ (childNodeName: String): OptionalPath = XmlPath \~ (elem, childNodeName)

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
        def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \~ (elem, childNodeName, predicate)

        /**
         * navigates a path similar to the method "\\" from [[scala.xml.Node]], while retaining the
         * tree information of the parent structure.
         * So, instead of just returning the elems at the end of the path without information of the parent elems,
         * this method returns a complete path with all information to the parent xml-structure.
         * The resulting [[OptionalPath]] can then be used to transform the entire xml-structure.
         * @param childNodeName the name of the child that is searched for
         * @return an [[OptionalPath]], that can be used to create a changed Xml-Structure.
         */
        def \\~ (childNodeName: String): OptionalPath = XmlPath \\~ (elem, childNodeName)

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
        def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \\~ (elem, childNodeName, predicate)
    
        /**
         * checks if elem has the same label as the given string.
         * If the given label si "_", than it is true by default.
         * @param label the label that will be compared to elem.label
         * @return true if elem.label == label or the given string is of value "_"
         */
        private[core] def hasCorrectLabel(label: String): Boolean = elem.label == label || label == "_"

    }
    
    implicit class NodeWrapper (node: Node) {
        
        // todo insert method to transform node names (with or without uri) and key names
        
        def toPrettyXMLString : String = pp.format(node)
    }
    
    implicit class NodeSeqWrapper(seq: NodeSeq) {
        def toPrettyXMLString : String = seq.map(pp.format(_)).mkString("\n")
    }
}
