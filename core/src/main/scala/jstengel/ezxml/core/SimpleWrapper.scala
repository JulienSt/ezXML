package jstengel.ezxml.core

import scala.xml.{Attribute, Elem, MetaData, Node, NodeSeq, Null, PrettyPrinter, Text}


object SimpleWrapper {
    private[this] val pp = new PrettyPrinter(400, 4)
    
    implicit class ElemWrapper (elem : Elem) {
        
        /** returns just elem without any children */
        def root: Elem = {
            val Elem(pre, lbl, att, meta, _*) = elem
            Elem(pre, lbl, att, meta, true, Seq(): _*)
        }
    
        /**
         *
         * @param key
         * @param value
         * @return
         */
        def setAttribute (key : String, value : String) : Elem = {
            elem % Attribute(key, Text(value), Null)
        }
        
        /**
         *
         * @param pre
         * @param key
         * @param value
         * @return
         */
        def setAttWithPre (pre: String, key : String, value : String) : Elem = {
            elem % Attribute(pre, key, Text(value), Null)
        }
    
        /**
         *
         * @param pairs
         * @return
         */
        def setAttributes (pairs : (String, String)*) : Elem = {
            elem % convertPairsToAttributes (pairs:_*)
        }
    
        /**
         *
         * @param triples
         * @return
         */
        def setAttsWithPre (triples : (String, String, String)*) : Elem = {
            elem % convertTriplesToAttributes(triples: _*)
        }
    
        /**
         *
         * @param predicate
         * @return
         */
        def filterChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filter(predicate) : _*)
        }
    
        /**
         *
         * @param predicate
         * @return
         */
        def deleteChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filterNot(predicate) : _*)
        }
    
        /**
         *
         * @param seq
         * @return
         */
        def addChildren (seq : Node*) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children ++ seq : _*)
        }
    
        /**
         *
         * @param transform
         * @return
         */
        def mapChildren (transform: Node => Node) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.map(transform) : _*)
        }
    
        /**
         *
         * @param transform
         * @return
         */
        def flatMapChildren (transform: Node => Option[Node]) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.flatMap(transform) : _*)
        }
        
        /* changes only the root, without the children
         *  child nodes can not even be accessed with the given function */
        def transformRoot (f: Elem => Elem): Elem = {
            val Elem(pre, lbl, att, meta, _*) = f(root)
            Elem(pre, lbl, att, meta, true, elem.child : _*)
        }
    
        /**
         *
         * @param childNodeName
         * @return
         */
        def \~ (childNodeName: String): OptionalPath = XmlPath \~ (elem, childNodeName)
    
        /**
         *
         * @param childNodeName
         * @param predicate
         * @return
         */
        def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \~ (elem, childNodeName, predicate)
    
        /**
         *
         * @param childNodeName
         * @return
         */
        def \\~ (childNodeName: String): OptionalPath = XmlPath \\~ (elem, childNodeName)
    
        /**
         *
         * @param childNodeName
         * @param predicate
         * @return
         */
        def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \\~ (elem, childNodeName, predicate)
    
        /**
         *
         * @param label
         * @return
         */
        def hasCorrectLabel(label: String): Boolean = elem.label == label || label == "_"
    
        /**
         *
         * @param triples
         * @return
         */
        private def convertTriplesToAttributes (triples : (String, String, String)*) = {
            triples.tail.foldLeft({
                val (pre, key, value) = triples.head
                Attribute(pre, key, Text(value), Null)
            }){ case (previous, (pre, key, value)) => Attribute(pre, key, Text(value), previous) }
        }
    
        /**
         *
         * @param pairs
         * @return
         */
        private def convertPairsToAttributes (pairs : (String, String)*) = {
            pairs.tail.foldLeft({
                val (key, value) = pairs.head
                Attribute(key, Text(value), Null)
            }){ case (previous, (key, value)) => Attribute(key, Text(value), previous) }
        }
        
    }
    
    implicit class NodeWrapper (node: Node) {
        
        // todo insert method to transform node names (with or without uri) and key names
        
        def toPrettyXMLString : String = pp.format(node)
    }
    
    implicit class NodeSeqWrapper(seq: NodeSeq) {
        def toPrettyXMLString : String = seq.map(pp.format(_)).mkString("\n")
    }
}
