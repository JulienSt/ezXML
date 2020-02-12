package jstengel.ezxml.extension.rt

import scala.language.reflectiveCalls
import scala.reflect.ClassTag
import scala.xml.{Attribute, Elem, Null, Text, TopScope}
import jstengel.ezxml.core.SimpleWrapper.ElemWrapper
import RuntimeReflectHelper.{NullFlag, arrayType, asArrayType, classTagOf, createStringRepresentation, getType, getTypeFromString, getTypeParams, isConstructorMissing, isSimpleType, iterableType, tagOf}
import jstengel.ezxml.extension.XmlClassTrait
import jstengel.ezxml.extension.mapping.FieldMapping.FieldMappings
import jstengel.ezxml.extension.XmlClassTrait


// https://medium.com/@sinisalouc/overcoming-type-erasure-in-scala-8f2422070d20
object RtEncoder {

    import scala.reflect.{runtime => rt}
    import rt.{universe => ru}
    import ru._
    private implicit val rm: Mirror = rt.currentMirror
    
    private val savableType = typeOf[XmlClassTrait]
    
    private[extension]
    def convertToXML[A] (a           : A,
                         mappings    : FieldMappings = Seq(),
                         pre         : String = null,
                         isRecursive : Boolean = false)
                        (implicit tt : TypeTag[A], ct : ClassTag[A]) : Elem = {
    
        val ttType    = getType(a)
        val typeParams = getTypeParams(ttType)
        val className = createStringRepresentation(ttType)(typeParams)
        
        if (ttType <:< savableType) {
            val Elem(_, l, att, s, c @ _*) = a.asInstanceOf[XmlClassTrait].saveAsXml
            Elem(pre, l, att, s, true, c: _*)
        }
        
        else if (isSimpleType(ttType)) {
            Elem(pre, className, Null, TopScope, true, Seq(): _*) % Attribute("value", Text(a.toString), Null)
        }
        
        else if (ttType <:< arrayType) {
            val tParams = getTypeParams(tt.tpe)
            val tParam = tParams.head
            val tagParam = tagOf(tParam)
            val arr = asArrayType(a)(tagParam, classTagOf(tParam)).map { e =>
                convertToXML(e, mappings)(tagParam, classTagOf(tParam))
            }.toIndexedSeq
            Elem(pre, createStringRepresentation(ttType)(tParams), Null, TopScope, true, arr: _*)
        }
        
        else if ((ttType <:< iterableType || className.startsWith("scala.collection.")) &&
                 !isRecursive &&
                 (isConstructorMissing(ttType) || ttType.typeSymbol.isAbstract)) {
            val iterator = a.asInstanceOf[IterableOnce[Any]].iterator
            val arrayToElemSeq: (Any => Elem) => Seq[Elem] = aToXml => iterator.map(aToXml).toSeq
            val (newClassName: String, seq: Seq[Elem]) = typeParams match {
                case Nil              =>
                    val asList = List.from(iterator)
                    val listHead = asList.headOption
                    val isIterableRecursive = listHead.contains(a)
                    if (isIterableRecursive)
                        asList.map(e => convertToXML(e, mappings, isRecursive = isIterableRecursive))
                    else if (listHead.nonEmpty)
                        (className, asList.map(e => convertToXML(e, mappings)))
                    else
                        (className, Nil)
                case typeParam :: Nil if typeParam.typeSymbol.fullName.startsWith("scala.Tuple") =>
                    val asList = List.from(iterator)
                    val tupleType =
                        asList.headOption
                              .map(h => getType(h)(tagOf(typeParam), ClassTag(h.getClass), rm))
                              .getOrElse(typeParam)
                    val newClassName = className.takeWhile(_ != '[') +
                                       "[" +
                                       createStringRepresentation(tupleType)() +
                                       "]"
                    val seq = asList.map(e => convertToXML(e, mappings)(tagOf(tupleType), ClassTag(e.getClass)))
                    (newClassName, seq)
                case typeParam :: Nil =>
                    (className, arrayToElemSeq(e => convertToXML(e, mappings)
                                                    (tagOf(typeParam), ClassTag(e.getClass))))
                case paramList =>
                    val n = paramList.length
                    // works for something like Map[Int, String], not something like List[(Int, String)]
                    val parameterizedTupleName = s"scala.Tuple$n" + className.dropWhile(_ != '[')
                    val tupleType = getTypeFromString(parameterizedTupleName)
                    (className, arrayToElemSeq(e => convertToXML(e, mappings)
                                                    (tagOf(tupleType), ClassTag(e.getClass))))
            }
            Elem(pre, newClassName, Null, TopScope, true, seq: _*)
        }
        
        else {
            val reflectedObj  = rm.reflect(a)
            val reflectedType = reflectedObj.symbol.typeSignature
            val constructor = reflectedType.members
                                           .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                                           .get
            constructor
                .paramLists
                .flatten
                .foldLeft(Elem(pre, className, Null, TopScope, true, Seq() : _*)){
                    case (elem @ Elem(pre, l, att, s, c @ _*), p) =>
                        val fieldName  = p.name
                        val fieldNameStr = fieldName.toString
                        
                        val member = reflectedType.member(
                            TermName(mappings.getSubstituteName(fieldNameStr)(tagOf(constructor.returnType)))
                        )
                        
                        val fieldValue  = try
                                              reflectedObj.reflectField(member.asTerm).get
                                          catch {
                                              case _: ScalaReflectionException => try {
                                                  reflectedObj.reflectMethod(member.asMethod).apply()
                                              } catch {
                                                  case r: ScalaReflectionException =>
                                                      val explanation = ScalaReflectionException(
                                                          "A field couldn't be accessed through reflection."+
                                                          "\nMake sure every Field you want to access " +
                                                          "is actually publicly accessible.\n" +
                                                          "If You don't want to change the privacy of a field,\n" +
                                                          "look into jstengel.ezxml.extension" +
                                                          ".mapping.FieldMapping.FieldMappings")
                                                      explanation.setStackTrace(Array.concat(
                                                          explanation.getStackTrace,
                                                          r.getStackTrace).drop(6)
                                                      )
                                                      throw explanation
                                              }
                                          }
                                 
                        val valueType = try
                                            rm.reflect(fieldValue).symbol.toType
                                        catch {
                                            case _ : ScalaReflectionException => member.typeSignature.resultType
                                            case _ : NullPointerException     => member.typeSignature.resultType
                                        }
                                 
                        val valueTypeName = valueType.typeSymbol.fullName

                        val fieldType = try
                                            member.asMethod.returnType /* otherwise you get [=> A] for [A] */
                                        catch {
                                            case _ : ScalaReflectionException => member.typeSignature
                                        }

                        val newLabel = if (fieldType.typeSymbol.isAbstract && l.contains(fieldType.toString))
                                           l.replace(fieldType.typeSymbol.fullName, valueTypeName)
                                       else
                                           l
                                 
                        if(fieldValue == null)
                            elem addAttWithPre (valueTypeName, fieldNameStr, NullFlag)
                        else if (isSimpleType(valueType)) {
                            Elem(pre, newLabel, att, s, true, c:_*).addAttWithPre(valueTypeName,
                                                                                  fieldNameStr,
                                                                                  fieldValue.toString)
                        } else {
                            val savedType = if (!fieldType.typeSymbol.isAbstract)
                                                fieldType
                                            else
                                                valueType
                            Elem(pre, newLabel, att, s, true, c ++
                                                              convertToXML(fieldValue, mappings, fieldNameStr)
                                                                          (tagOf(savedType),
                                                                           ClassTag(fieldValue.getClass)) : _*)
                        }
                }
        }
    }
    
}
