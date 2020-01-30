# ezXML
This project aims to make working with xml in scala simpler by building on the standard xml library:
https://github.com/scala/scala-xml

Although the standard library has a lot to offer and is better than most xml libraries for other languages,
I personally think it can be unwieldy once you want to try more advanced stuff 
(like changing child elements that satisfy a certain predicate).

My main goal therefore is the simplification of these problems.

---

 #### Simple Example:

 ```scala
import indv.jstengel.ezxml.core.SimpleWrapper.ElemWrapper
import scala.xml.Elem

val elem: Elem =
      <A>
          <B value="test1">
              <C/>
          </B>
          <B value="test2">
              <C/>
          </B>
          <B value="test3">
              <C/>
          </B>
      </A>
 
val res = elem \~ ("B", _ \@ "value" == "test2") \~ "C" addChildren <D/>
println(res)
 ```
 ##### Output:
  ```scala
  Some(<A>
           <B value="test1">
               <C/>
           </B>
           <B value="test2">
               <C>
                   <D/>
               </C>
           </B>
           <B value="test3">
               <C/>
           </B>
       </A>)
  ```

The core part of the library simple tries to add a little extra functionality like shown in the example.
To read up on all the other things one can do with this library, click here:

- todo: link to core wiki
- todo: link to parsing wiki

---
#### How to integrate this Library
since I am currently neither able nor willing to put this library up on maven central, we have to make do with 

---

#### One other fun example