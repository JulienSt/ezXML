package indv.jstengel.ezxml.extension.mapping


import scala.annotation.implicitNotFound


object NotNothingObject {
    
    // source :
    // https://riptutorial.com/scala/example/21134/preventing-inferring-nothing
    
    @implicitNotFound("Nothing was inferred")
    sealed trait NotNothing[-T]
    
    implicit object notNothing extends NotNothing[Any]
    //We do not want Nothing to be inferred, so make an ambiguous implicit
    implicit object `\nThis error appeared, because someone forgot the type parameters to a function or class\n`
        extends NotNothing[Nothing]
    
}
