package jstengel.ezxml.extension.rt

import jstengel.ezxml.extension.RTWrappers.{DecodingWrapper, EncodingWrapper}
import org.scalatest.flatspec.AnyFlatSpec

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait BasicRtLoadTest { this: AnyFlatSpec =>
    def test[A](a: A)(implicit tt : TypeTag[A], ct : ClassTag[A]): Unit = {
        val xml = a.xml
        s"\n$a as ${tt.tpe}" should s" load with $xml at RT" in {assert(a == xml.obj[A].get)}
    }
    def testArray[A](a: Array[A])(implicit tt : TypeTag[Array[A]], ct : ClassTag[Array[A]]): Unit = {
        s"\n$a" should s" load with ${a.xml} " in {
            assert(a.xml.obj[Array[A]].get.zipWithIndex.forall(p => p._1 == a(p._2)))
        }
    }
}
