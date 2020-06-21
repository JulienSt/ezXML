package jstengel.ezxml.extension

import scala.xml.Elem


/**
 * This trait signals the encoding functions that there is an already written encoding function inside the target class
 * This leads to less reflections during runtime and avoids accidentally doubled functions
 */
trait XmlClassTrait {
    def encode(): Elem
}

/**
 * This trait signals the decoding functions that there is an already written decoding function in the companion object.
 * This leads to less reflections during runtime and avoids accidentally doubled functions
 */
trait XmlObjectTrait {
    def decode(elem: Elem): XmlClassTrait
}