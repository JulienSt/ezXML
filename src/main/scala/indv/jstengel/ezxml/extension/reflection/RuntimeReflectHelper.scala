package indv.jstengel.ezxml.extension.reflection


import indv.jstengel.ezxml.extension.StringTypeTree

import scala.collection.IterableOnce
import scala.reflect.{ClassTag, api}
import scala.reflect.runtime.universe.{
    MethodMirror,
    MethodSymbol,
    Mirror,
    Symbol,
    Type,
    TypeRef,
    TypeTag,
    internal,
    SingleType,
    typeOf}
import internal.typeRef


//private[xml]
private[ezxml] object RuntimeReflectHelper {
    
    private val IntType = typeOf[Int]
    private val DoubleType = typeOf[Double]
    private val LongType = typeOf[Long]
    private val FloatType = typeOf[Float]
    private val ShortType = typeOf[Short]
    private val ByteType = typeOf[Byte]
    private val BooleanType = typeOf[Boolean]
    private val CharType = typeOf[Char]
    private val StringType = typeOf[String]
    private val JIntType = typeOf[java.lang.Integer]
    private val JDoubleType = typeOf[java.lang.Double]
    private val JLongType = typeOf[java.lang.Long]
    private val JFloatType = typeOf[java.lang.Float]
    private val JShortType = typeOf[java.lang.Short]
    private val JByteType = typeOf[java.lang.Byte]
    private val JBooleanType = typeOf[java.lang.Boolean]
    private val JCharType = typeOf[java.lang.Character]
    
    def stringToSimpleValue[A] (toBeConverted : String) : PartialFunction[TypeTag[A], A] = {
        case TypeTag(t) if t <:< IntType      => toBeConverted.toInt.asInstanceOf[A]
        case TypeTag(t) if t <:< DoubleType   => toBeConverted.toDouble.asInstanceOf[A]
        case TypeTag(t) if t <:< LongType     => toBeConverted.toLong.asInstanceOf[A]
        case TypeTag(t) if t <:< FloatType    => toBeConverted.toFloat.asInstanceOf[A]
        case TypeTag(t) if t <:< ShortType    => toBeConverted.toShort.asInstanceOf[A]
        case TypeTag(t) if t <:< ByteType     => toBeConverted.toByte.asInstanceOf[A]
        case TypeTag(t) if t <:< BooleanType  => toBeConverted.toBoolean.asInstanceOf[A]
        case TypeTag(t) if t <:< CharType     => toBeConverted.head.asInstanceOf[A]
        case TypeTag(t) if t <:< StringType   => toBeConverted.asInstanceOf[A]
        case TypeTag(t) if t <:< JIntType     => toBeConverted.toInt.asInstanceOf[A]
        case TypeTag(t) if t <:< JDoubleType  => toBeConverted.toDouble.asInstanceOf[A]
        case TypeTag(t) if t <:< JLongType    => toBeConverted.toLong.asInstanceOf[A]
        case TypeTag(t) if t <:< JFloatType   => toBeConverted.toFloat.asInstanceOf[A]
        case TypeTag(t) if t <:< JShortType   => toBeConverted.toShort.asInstanceOf[A]
        case TypeTag(t) if t <:< JByteType    => toBeConverted.toByte.asInstanceOf[A]
        case TypeTag(t) if t <:< JBooleanType => toBeConverted.toBoolean.asInstanceOf[A]
        case TypeTag(t) if t <:< JCharType    => toBeConverted.head.asInstanceOf[A]
    }
    
    val iterableType : Type = typeOf[IterableOnce[_]]
    val arrayType    : Type = typeOf[Array[_]]
    val productType  : Type = typeOf[scala.Product]
    val listType     : Type = typeOf[List[_]]
    val seqType      : Type = typeOf[Seq[_]]
    private val listSymbol = listType.typeSymbol
    private val seqSymbol = seqType.typeSymbol
    val minimumBaseClasses = List(typeOf[Any].typeSymbol)
    
    def isSimpleType(tpe: Type): Boolean = tpe <:< typeOf[String] || tpe <:< typeOf[AnyVal] || tpe <:< typeOf[Number]
    
    def classTagOf[T](tpe: Type)(implicit rm: Mirror): ClassTag[T] = ClassTag(rm.runtimeClass(tpe))
    
    def asArrayType[A, T](a: A)(implicit tt : TypeTag[T], ct: ClassTag[T]): Array[T] =
        Array[T](a.asInstanceOf[Array[T]].toSeq: _*)
    
    /* https://stackoverflow.com/questions/27887386/get-a-typetag-from-a-type */
    def tagOf(tpe : Type)(implicit rm: Mirror) : TypeTag[Any] = TypeTag(rm, new api.TypeCreator {
        def apply[U <: api.Universe with Singleton] (m : api.Mirror[U]) : U#Type = tpe.asInstanceOf[U#Type]
    })
    
    def getTypeParams (tpe: Type): List[Type] = tpe match {
        case TypeRef(_, _, args) => args
        case _ => List()
    }
    
    def companionMethodExtraction(companionSymbol: Symbol, methodName: String)
                                              (implicit rm: Mirror): PartialFunction[Symbol, MethodMirror] = {
        case m : MethodSymbol if m.name.toString == methodName =>
            rm.reflect(rm.reflectModule(companionSymbol.asModule).instance).reflectMethod(m.asMethod)
    }
    
    /* with type Params in name */
    def getTypeFromString(s: String)(implicit rm: Mirror): Type = StringTypeTree.typeFromString(s)
    
    
    def getType[A](a: A)(implicit tt : TypeTag[A], ct : ClassTag[A], rm: Mirror): Type = {
    
        /* tpe <:< _ does not work for the colonColonType at this point, because sometimes it is missing it's tparam */
        def isColonColonType(tpe: Type): Boolean =
            tpe.typeSymbol.asClass.baseClasses.head.asClass.fullName == "scala.collection.immutable.$colon$colon"
    
        def isRepeatedType(tpe: Type): Boolean = tpe.baseClasses.head.name.toString == "<repeated>"
    
        def findLowestSuperClassOfIterable(it: Iterator[Any])(implicit rm: Mirror): Type = {
            it.map(e => rm.reflect(e).symbol.asClass.baseClasses)
              .iterator
              .reduce((l1, l2) => l1.filter(x => l2.contains(x)))
              .head
              .asClass
              .toType
        }
        
        def createListType(l: Any): Type = {
            val iterable = l.asInstanceOf[::[_]]
            val ListParam = findLowestSuperClassOfIterable(iterable.iterator)
            if (isColonColonType(ListParam))
                typeRef(listType, listSymbol, List(createListType(iterable.asInstanceOf[::[::[_]]].flatten)))
            else
                typeRef(listType, listSymbol, List(ListParam))
        }
        
        def createSeqType(l: Any): Type = {
            val iterable = l.asInstanceOf[IterableOnce[_]]
            val ListParam = findLowestSuperClassOfIterable(iterable.iterator)
            if (isColonColonType(ListParam))
                typeRef(seqType, seqSymbol, List(createListType(iterable.asInstanceOf[Seq[::[_]]].flatten)))
            else
                typeRef(seqType, seqSymbol, List(ListParam))
        }
        
        val tpe = tt.tpe
        val reflectedSymbol = rm.reflect(a).symbol
        
        if(reflectedSymbol.isModuleClass)
            reflectedSymbol.typeSignature
        else if ( tpe.typeSymbol.isAbstract && !( tpe <:< iterableType ) ) {
            val rtType          = reflectedSymbol.asClass.toType
            if ( reflectedSymbol.typeParams.nonEmpty && !( tpe.baseClasses == minimumBaseClasses ) )
                rtType.baseClasses
                      .withFilter(_.asClass
                                   .knownDirectSubclasses
                                   .contains(reflectedSymbol))
                      .withFilter(_.asClass.toType <:< tpe.erasure)
                      .map{ s =>
                          val tParamNames          = getTypeParams(s.asClass.toType).map(_.typeSymbol.name)
                          val paramMap             = tParamNames.zip(getTypeParams(tpe)).toMap
                          val reflectedTParamNames = getTypeParams(reflectedSymbol.toType).map(_.typeSymbol.name)
                          reflectedTParamNames.map(paramMap(_))
                      }
                      .headOption
                      .map{ head =>
                          reflectedSymbol.toType match {
                              case TypeRef(t, s, _) => typeRef(t, s, head)
                          }
                      }
                      .getOrElse(rtType)
            else if ( isRepeatedType(tpe) )
                     createSeqType(a)
            else if ( isColonColonType(rtType) )
                     createListType(a)
            else
                rtType
        }
        else if ( isRepeatedType(tpe) )
            createSeqType(a)
        else if ( isColonColonType(tpe) )
            createListType(a)
        else
            tpe
    }

    def createStringRepresentation(t: Type)(typeParams: List[Type] = getTypeParams(t)): String =
        if (typeParams.isEmpty)
            t.typeSymbol.fullName
        else
            s"${t.typeSymbol.fullName}[${typeParams.map(t => createStringRepresentation(t)()).mkString(",")}]"
    
}
