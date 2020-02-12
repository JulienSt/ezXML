package jstengel.ezxml.extension

import scala.xml.Elem


/**
 * todo description
 */
trait XmlClassTrait {
    def saveAsXml: Elem
}

/**
 * todo description
 */
trait XmlObjectTrait {
    def loadFromXML(elem: Elem): XmlClassTrait
}