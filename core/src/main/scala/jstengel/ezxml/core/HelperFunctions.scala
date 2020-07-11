package jstengel.ezxml.core


import scala.xml.{Attribute, MetaData, Null, PrefixedAttribute, UnprefixedAttribute}


object HelperFunctions {
    
    def mergeAttributes(attributes: Attribute*) : MetaData =
        if (attributes.nonEmpty)
            attributes.reduce[Attribute]{
                case (previous, PrefixedAttribute(pre, key, value, _)) => Attribute(pre, key, value, previous)
                case (previous, UnprefixedAttribute(key, value, _)) => Attribute(key, value, previous)
            }
        else
            Null
}
