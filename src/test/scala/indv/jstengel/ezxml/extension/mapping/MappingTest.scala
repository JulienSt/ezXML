package indv.jstengel.ezxml.extension.mapping


import indv.jstengel.ezxml.extension.rt.{RTConverter, RTLoader}


object MappingTest extends App {

    class A(field: Int) {
        val substitution: Int = field * 2
    
        override def toString : String = s"A($field)"
    }

    val a = new A(23)
    
    val map = FieldMapping[A]("field" -> "substitution")
    
    val xml = RTConverter.convertToXML(a, FieldMappings(map))
    
    println(a)
    println(xml)
    println(RTLoader.load[A](xml))
    
    
}
