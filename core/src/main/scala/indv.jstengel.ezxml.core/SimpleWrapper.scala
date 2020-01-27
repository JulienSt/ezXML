package indv.jstengel.ezxml.core


import scala.xml.{Attribute, Elem, MetaData, Node, NodeSeq, Null, PrettyPrinter, Text}


object SimpleWrapper {
    private[this] val pp = new PrettyPrinter(400, 4)
    
    implicit class ElemWrapper (elem : Elem) {
        
        /** returns just elem without any children */
        def root: Elem = {
            val Elem(pre, lbl, att, meta, _*) = elem
            Elem(pre, lbl, att, meta, true, Seq(): _*)
        }
        
        // adds multiples without normalizing, the end result therefore contains duplicates
        def addAttribute(key : String, value : String) : Elem = {
            elem.copy(attributes = MetaData.concatenate(elem.attributes, Attribute(key, Text(value), Null)))
        }
        def addAttWithPre(pre: String, key : String, value : String) : Elem = {
            elem.copy(attributes = MetaData.concatenate(elem.attributes, Attribute(pre, key, Text(value), Null)))
        }
        def addAttributes(pairs : (String, String)*) : Elem = {
            elem.copy(attributes = MetaData.concatenate(elem.attributes, convertPairsToAttributes(pairs: _*)))
        }
        def addAttsWithPre(triples : (String, String, String)*) : Elem = {
            elem.copy(attributes = MetaData.concatenate(elem.attributes, convertTriplesToAttributes(triples: _*)))
        }
        
        def setAttribute (key : String, value : String) : Elem = {
            elem % Attribute(key, Text(value), Null)
        }
        def setAttWithPre (pre: String, key : String, value : String) : Elem = {
            elem % Attribute(pre, key, Text(value), Null)
        }
        def setAttributes (pairs : (String, String)*) : Elem = {
            elem % convertPairsToAttributes (pairs:_*)
        }
        def setAttsWithPre (triples : (String, String, String)*) : Elem = {
            elem % convertTriplesToAttributes(triples: _*)
        }
        
        def filterChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filter(predicate) : _*)
        }
        
        def deleteChildren (predicate: Node => Boolean) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.filterNot(predicate) : _*)
        }
        
        def addChildren (seq : Node*) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children ++ seq : _*)
        }
        
        def mapChildren (transform: Node => Node) : Elem = {
            val Elem(pre, lbl, att, meta, children @ _*) = elem
            Elem(pre, lbl, att, meta, true, children.map(transform) : _*)
        }
        
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
        
        def \~ (childNodeName: String): OptionalPath = XmlPath \~ (elem, childNodeName)
        def \~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \~ (elem, childNodeName, predicate)
        def \\~ (childNodeName: String): OptionalPath = XmlPath \\~ (elem, childNodeName)
        def \\~ (childNodeName: String, predicate: Elem => Boolean): OptionalPath =
            XmlPath \\~ (elem, childNodeName, predicate)
        
        def apply (createPath : ElemWrapper => OptionalPath): OptionalPath = createPath(this)
        
        
        def hasCorrectLabel(label: String): Boolean = elem.label == label || label == "_"
        
        
        private def convertTriplesToAttributes (triples : (String, String, String)*) = {
            triples.tail.foldLeft({
                                      val (pre, key, value) = triples.head
                                      Attribute(pre, key, Text(value), Null)
                                  }){ case (previous, (pre, key, value)) => Attribute(pre, key, Text(value), previous) }
        }
        
        private def convertPairsToAttributes (pairs : (String, String)*) = {
            pairs.tail.foldLeft({
                                    val (key, value) = pairs.head
                                    Attribute(key, Text(value), Null)
                                }){ case (previous, (key, value)) => Attribute(key, Text(value), previous) }
        }
        
    }
    
    implicit class NodeWrapper (node: Node) {
        def toPrettyXMLString : String = pp.format(node)
    }
    implicit class NodeSeqWrapper(seq: NodeSeq) {
        def toPrettyXMLString : String = seq.map(pp.format(_)).mkString("\n")
    }
    
    implicit class TextWrapper (text: Text) {
        def obj: String = text.text // todo check if this is really needed
        //  maybe in combination with the reflective type { def obj } ?
    }
}
