package jstengel.ezxml.extension

import scala.xml.Elem


/**
 * todo description
 */
trait XmlClassTrait {
    def encode: Elem
}

/**
 * This trait signals the DeCoders and
 */
trait XmlObjectTrait {
    def decode(elem: Elem): XmlClassTrait
}