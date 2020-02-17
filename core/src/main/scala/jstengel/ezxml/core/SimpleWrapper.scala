package jstengel.ezxml.core


import scala.xml._

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
    
        /**
         * With this method all labels inside the entire structure of elem will be renamed
         *
         * @note be careful to not rename two different labels with the same replacement, as that
         *       will be an irreversible transformation, when you send the resulting [[Elem]] to
         *       a different party, which doesn't have the original [[elem]]
         * @note also of note is the fact, that an already prefixed label will not be replaced,
         *       if only the label matches.
         *       So, pre:Label will not be matched by "Label", it will be matched though by
         *       "_:Label", as will every other label of value "Label", regardless of prefix.
         *       If both are present, the most fitting replacement will be used.
         *       "pre:Label" over "_:Label" when prefix is given
         *       "Label" over "_:Label" when the target elem has no prefix
         * @param renamingPairs a list of string pairs (Tuple2[String, String]), where the first element always
         *                      describes the label that will be renamed and the corresponding second element is
         *                      the replacement label.
         *                      A colon denotes if a value has a prefix or not. E.g. :
         *                          "pre1:Test" -> "PreTest" will replace the prefixed label with an unprefixed label
         *                          "preA:A" -> "prefix:LabelA" will replace prfix and label
         *                          "C" -> "preC:C" adds a prefix to all labels of value "C"
         * @return the complete structure of [[elem]], where all labels are renamed according to the defined
         *         renamingPairs
         */
        def renameLabels(renamingPairs : (String, String)*): Elem = {
            val (newMap, keyList, temp) = prepareRenaming(renamingPairs)
            val res = (temp \\~ ("_", {
                case Elem(null, lbl, _, _, _*) => keyList.contains(lbl) || keyList.contains(s"_:$lbl")
                case Elem(pre, lbl, _, _, _*)  => keyList.contains(s"$pre:$lbl") || keyList.contains(s"_:$lbl")
                case _ => false
            })).transformTargetRoot {
                case Elem(null, lbl, att, meta, c @ _*) =>
                    val (newPre, newLbl) = newMap.getOrElse(lbl, newMap(s"_:$lbl"))
                    Elem(newPre, newLbl, att, meta, true, c: _*)
                case Elem(pre, lbl, att, meta, c @ _*)  =>
                    val (newPre, newLbl) = newMap.getOrElse(s"$pre:$lbl", newMap(s"_:$lbl"))
                    Elem(newPre, newLbl, att, meta, true, c: _*)
                case n => n
            }
            res.get.child.find(_.isInstanceOf[Elem]).get.asInstanceOf[Elem]
        }
    
        /**
         * Same as [[renameLabels]], but with all Attributes inside [[elem]].
         * Same rules concerning the prefixes apply as well
         * @param renamingPairs a list of string pairs (Tuple2[String, String]), where the first element always
         *                      describes the key that will be renamed and the corresponding second element is
         *                      the replacement key.
         * @return the complete structure of [[elem]], where all Attributes are renamed according to the defined
         *         renamingPairs
         */
        def renameAttributes(renamingPairs : (String, String)*): Elem = {
            val (newMap, keyList, temp) = prepareRenaming(renamingPairs)
            val res = (temp \\~ ("_", {
                case e : Elem => e.attributes.iterator.exists{
                    case PrefixedAttribute(pre, key, _, _) =>
                        keyList.contains(s"$pre:$key") || keyList.contains(s"_:$key")
                    case Attribute(key, _, _) =>
                        keyList.contains(key) || keyList.contains(s"_:$key")
                }
                case _ => false
            })).transformTargetRoot {
                case Elem(pre, lbl, att, meta, c @ _*) =>
                    val newAtt = att.map{
                        case PrefixedAttribute(pre, key, value, data) =>
                            val (newPre, newKey) = newMap.getOrElse(s"$pre:$key", newMap(s"_:$key"))
                            Attribute(Option(newPre), newKey, value, data)
                        case Attribute(key, value, data) =>
                            val (newPre, newKey) = newMap.getOrElse(key, newMap(s"_:$key"))
                            Attribute(Option(newPre), newKey, value, data)
                        case a => a
                    }.fold(Null)((soFar, attr) => soFar append attr)
                    Elem(pre, lbl, newAtt, meta, true, c: _*)
                case n => n
            }
            (res.get \ elem.label).head.asInstanceOf[Elem]
        }
    
        /**
         * preparation function for [[renameAttributes]] and [[renameLabels]]
         * @param pairList the input of either [[renameAttributes]] or [[renameLabels]]
         * @return a [[Tuple3]] with (conversion map, seq of keys for the conversion map, and a temp elem)
         */
        private def prepareRenaming(pairList: Seq[(String, String)]) : (Map[String, (String, String)],
                                                                        Seq[String],
                                                                        Elem) = {
            val map = pairList.map{ case (key, value) =>
                val split               = value.split(':')
                val prefixedReplacement = if ( split.length == 1 ) (null, value) else (split(0), split(1))
                (key, prefixedReplacement)
            }.toMap
            (map, pairList.map(_._1), <Temp>${elem}</Temp>)
        }
        
        def toPrettyXMLString : String = pp.format(elem)
        
    }
    
    implicit class NodeWrapper (node: Node) {
        def toPrettyXMLString : String = pp.format(node)
    }
    
    implicit class NodeSeqWrapper(seq: NodeSeq) {
        def toPrettyXMLString : String = seq.map(pp.format(_)).mkString("\n")
    }
}
