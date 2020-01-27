package indv.jstengel.ezxml.extension


import scala.xml.{Elem, PrettyPrinter}

trait XMLable {
    def toXML : Elem

    def toXML (onCreated : () => Unit) : Elem = toXML
    
    def toXMLString : String = toXML.toString()
    
    def toPrettyXMLString (onCreated : () => Unit) : String = new PrettyPrinter(400, 4).format(toXML(onCreated))
}
