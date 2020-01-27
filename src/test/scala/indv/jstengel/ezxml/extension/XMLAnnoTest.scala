package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.macros.Xml

object XMLAnnoTest extends App{
    
    object Stuff
    @Xml class Bla(val i: Int)
    
    @Xml case class Blupp()
    
    @Xml class Hey()
    object Hey
    
    @Xml class Stuff()
    object anotherObject
    
}
