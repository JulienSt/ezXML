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
import jstengel.ezxml.core.SimpleWrapper.ElemWrapper
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
\\ where "\~" is similar to "\" from the standard library and can be given a predicate.
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
                   <D/>  // <- notice that the elem was only added here, as dictated by the specified predicate
                         //    _ \@ "value" == "test2"
               </C>
           </B>
           <B value="test3">
               <C/>
           </B>
       </A>)
  ```

---
#### How to integrate this Library

Complete dependencies for just the core:
  ```
  // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  libraryDependencies += "com.github.julienst" % "ezxml-core_2.13" % "0.6.1"
  ```
Complete dependencies if the extension is used:
  ```
  // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  // https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.1"

  libraryDependencies += "com.github.julienst" % "ezxml-core_2.13" % "0.6.1"
  libraryDependencies += "com.github.julienst" % "ezxml-extension_2.13" % "0.6.1"

  scalacOptions += "-Ymacro-annotations"
  ```
  This Library is also compatible with Scala.js and could be used to communicate between Server and Client.

---

#### One other fun example
  ```scala
  import jstengel.ezxml.extension.{ElemWrapper, ObjWrapper}
  import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
          
  class CurriedVarArgs[A, B](val a: A*)(val b: B*) {
      override def equals (obj : Any) : Boolean = obj match {
          case other: CurriedVarArgs[A, B] =>
              other.a.zip(a).forall(p => p._1 == p._2) && other.b.zip(b).forall(p => p._1 == p._2)
      }
  }
  
  val a = new CurriedVarArgs("String1", "test2", "String3", "test4")(1, 2, 3, 4, 5, 6)
  
  val encoded = a.xml // with this the object is automatically encoded into a xml element
  println(encoded.toPrettyXMLString)
  val decoded = encoded.obj[CurriedVarArgs[String, Int]].get // with this the xml object can be decoded
  println(a == decoded)
  ```
 ##### Output:
  ```scala
  <jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int]>
      <a:scala.collection.immutable.Seq[java.lang.String]>
          <java.lang.String value="String1"/>
          <java.lang.String value="test2"/>
          <java.lang.String value="String3"/>
          <java.lang.String value="test4"/>
      </a:scala.collection.immutable.Seq[java.lang.String]>
      <b:scala.collection.immutable.Seq[java.lang.Integer]>
          <java.lang.Integer value="1"/>
          <java.lang.Integer value="2"/>
          <java.lang.Integer value="3"/>
          <java.lang.Integer value="4"/>
          <java.lang.Integer value="5"/>
          <java.lang.Integer value="6"/>
      </b:scala.collection.immutable.Seq[java.lang.Integer]>
  </jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int]>
  true
```
As you can see all objects can be saved and loaded without specifying how to decode or encode the object.
But this is obviously a little bit unwieldy, when one decides to send this xml structure.
For this, the following is of help:

  ```scala
  import jstengel.ezxml.core.SimpleWrapper.ElemWrapper

  val map =
      Map("jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int]" -> "Class[String,Int]",
          "a:scala.collection.immutable.Seq[java.lang.String]" -> "SeqA",
          "java.lang.String" -> "String",
          "b:scala.collection.immutable.Seq[java.lang.Integer]" -> "SeqB",
          "java.lang.Integer" -> "Int")

  val mapped = encoded.renameLabels(map.toSeq:_*) // the map can be used to rename everything in the xml output
  println(mapped.toPrettyXMLString) 

  val reverseMap = map.map(_.swap) // obviously this map is reversable
  val decoded2 = mapped.renameLabels(reverseMap.toSeq:_*).obj[CurriedVarArgs[String, Int]].get
  println(a == decoded2)

  ```
 ##### Output:
  ```scala
  <Class[String,Int]>
      <SeqA>
          <String value="String1"/>
          <String value="test2"/>
          <String value="String3"/>
          <String value="test4"/>
      </SeqA>
      <SeqB>
          <Int value="1"/>
          <Int value="2"/>
          <Int value="3"/>
          <Int value="4"/>
          <Int value="5"/>
          <Int value="6"/>
      </SeqB>
  </Class[String,Int]>
  true
```

The map that was used to shrink the original can also be encoded and decoded

  ```scala
  println(reverseMap.xml.toPrettyXMLString)
  println(reverseMap.xml.obj[Map[String, String]].get)
  ```
 ##### Output:
  ```scala
  <scala.collection.immutable.Map[java.lang.String,java.lang.String]>
      <scala.Tuple2[java.lang.String,java.lang.String] java.lang.String:_2="a:scala.collection.immutable.Seq[java.lang.String]" java.lang.String:_1="SeqA"/>
      <scala.Tuple2[java.lang.String,java.lang.String] java.lang.String:_2="java.lang.Integer" java.lang.String:_1="Int"/>
      <scala.Tuple2[java.lang.String,java.lang.String] java.lang.String:_2="b:scala.collection.immutable.Seq[java.lang.Integer]" java.lang.String:_1="SeqB"/>
      <scala.Tuple2[java.lang.String,java.lang.String] java.lang.String:_2="jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int]" java.lang.String:_1="Class[String,Int]"/>
      <scala.Tuple2[java.lang.String,java.lang.String] java.lang.String:_2="java.lang.String" java.lang.String:_1="String"/>
  </scala.collection.immutable.Map[java.lang.String,java.lang.String]>
  HashMap(
      SeqA -> a:scala.collection.immutable.Seq[java.lang.String],
      Int -> java.lang.Integer,
      SeqB -> b:scala.collection.immutable.Seq[java.lang.Integer],
      Class[String,Int] -> jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int], 
      String -> java.lang.String
  )
  ```

The given examples are all runtime encodings and decodings and are therefore a little bit slow. But not to worry, 
Compile time variant can also be found under 
  
  ```scala
  jstengel.ezxml.extension.ct.CtEncoder
  jstengel.ezxml.extension.ct.CtDecoder
  ```


The core part of the library simply tries to add a little extra functionality like shown in the first example.
The extension part is concerned with encoding and decoding of arbitrary objects and type.
To read up on all the other things one can do with this library, check out the code in

- core/src/test/scala/jstengel/ezxml/core for the core capabilities
- src/test/scala/jstengel/ezxml/extension for the extension capabilities

(The wiki is currently under construction, but once that is finished, I will link the main topics here as well)
