package jstengel.ezxml.extension

import scala.xml.Elem


trait XmlClassTrait {
    def saveAsXml: Elem
}

trait XmlObjectTrait {
    def loadFromXML(elem: Elem): XmlClassTrait
}