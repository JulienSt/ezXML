package indv.jstengel.ezxml.extension.rt


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
    typeOf}
import internal.typeRef


/**
 * Everything inside this object is used to aid in runtime reflections and minimize duplicate code between loading and
 * converting objects.
 */
private[rt] object RuntimeReflectHelper {
    
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
    
    /**
     * @param toBeConverted the String that will be converted to another simple Type
     * @tparam A the type the String will be converted into
     * @return a partial function that converts the string value of toBeConverted to a value of Type A
     *         by accepting a TypeTag[A]
     */
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
    
    /**
     * a type is considered simple (in this library at least), when it is a sub type of AnyVal, Number, or String
     * and a value of that type can therefor easily be extracted from a String
     * @param tpe the type that will be checked
     * @return true, if the type is a sub type of AnyVal, Number, or String. false if that is not the case
     */
    def isSimpleType(tpe: Type): Boolean = tpe <:< typeOf[String] || tpe <:< typeOf[AnyVal] || tpe <:< typeOf[Number]
    
    /**
     * this is a convenient method to reduce boilerplate and communicate intention
     * it converts a Type into a ClassTag
     * @param tpe the type which will be converted
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @tparam T the type param for the resulting ClassTag
     * @return a ClassTag based upon the input type
     */
    def classTagOf[T](tpe: Type)(implicit rm: Mirror): ClassTag[T] = ClassTag(rm.runtimeClass(tpe))
    
    /**
     * converts a basic array Type to a proper (scala) array. Since a lot of information is lost a runtime about a type
     * this function restores that information, so that it can be used to accurately convert the type of an array as
     * String
     * @param a an Array/java Array (something like int[]), that is to be converted to Array[T] (Array[int])
     * @param tt TypeTag for Type A | theoretically this is provided implicitly, but since the implicit
     *           values for this are often wrong, all uses of this function give this parameter explicitly
     * @param ct ClassTag for Type A | theoretically this is provided implicitly, but since the implicit
     *           values for this are often wrong, all uses of this function give this parameter explicitly
     * @tparam A the type of Array (something like int[])
     * @tparam T the type parameter for the resulting array
     * @return an Array[T] with all the elements inside a
     */
    def asArrayType[A, T](a: A)(implicit tt : TypeTag[T], ct: ClassTag[T]): Array[T] =
        Array[T](a.asInstanceOf[Array[T]].toSeq: _*)
    
    /**
     * This function returns the correct TypeTag for a type based upon the runtime mirror of the universe
     * https://stackoverflow.com/questions/27887386/get-a-typetag-from-a-type
     * @param tpe the type for which the TypeTag will be extracted
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return The TypeTag corresponding to tpe
     */
    def tagOf(tpe : Type)(implicit rm: Mirror) : TypeTag[Any] = TypeTag(rm, new api.TypeCreator {
        def apply[U <: api.Universe with Singleton] (m : api.Mirror[U]) : U#Type = tpe.asInstanceOf[U#Type]
    })
    
    /**
     * @param tpe the type for which the type parameters will be extracted
     * @return All TypeParameters as a List corresponding to tpe
     */
    def getTypeParams (tpe: Type): List[Type] = tpe match {
        case TypeRef(_, _, args) => args
        case _ => List()
    }
    
    /**
     * this method created a partial function that reflects a method inside a companion object.
     * the resulting partial function that is to be used in conjunction with collectFirst
     * @param companionSymbol the Symbol for the companion object from which the the method will be reflected
     * @param methodName the name of the reflected method
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return a partial function Symbol => MethodMirror, that is defined for a MethodSymbol with its name equal to
     *         methodName
     */
    def companionMethodExtraction(companionSymbol: Symbol, methodName: String)
                                              (implicit rm: Mirror): PartialFunction[Symbol, MethodMirror] = {
        case m : MethodSymbol if m.name.toString == methodName =>
            rm.reflect(rm.reflectModule(companionSymbol.asModule).instance).reflectMethod(m.asMethod)
    }
    
    /**
     * Reflects a type based upon a given string. This String has to include all necessary information about that type,
     * including the complete path and all type parameters.
     * The output of createStringRepresentation is most likely the input for this function
     * @param s the string representation for a type
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return The correct Type, with all type params based upon the input string
     */
    def getTypeFromString(s: String)(implicit rm: Mirror): Type = StringTypeTree.typeFromString(s)
    
    /**
     * reflects the type of a given value with accurate type parameters included
     * Since a lot of information is lost when only looking at tparams, this function recovers the remaining information
     * through runtime reflection
     * @param a the value from which the type will be reflected
     * @param tt TypeTag for Type A | this is provided implicitly
     * @param ct ClassTag for Type A | this is provided implicitly
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @tparam A the type parameter corresponding to value a
     * @return
     */
    def getType[A](a: A)(implicit tt : TypeTag[A], ct : ClassTag[A], rm: Mirror): Type = {
    
        /* tpe <:< _ does not work for the colonColonType at this point, because sometimes it is missing it's tparam */
        def isColonColonType(tpe: Type): Boolean =
            tpe.typeSymbol.asClass.baseClasses.head.asClass.fullName == "scala.collection.immutable.$colon$colon"
    
        /**
         * var args are annotated with "<repeated>" and need to be understood as iterable/seq
         * therefor method allows to check on that easily.
         * @param tpe the type of a parameter
         * @return true, if the given type is a vararg
         *         false otherwise
         */
        def isRepeatedType(tpe: Type): Boolean = tpe.baseClasses.head.name.toString == "<repeated>"
    
        /**
         * to find the best fitting type param for an iterable it, all types of the elements inside it are reflected.
         * these types are then filtered, to get the best fitting base class that will represent the type param of it
         * @param it the iterable, that will be searched through
         * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
         * @return the greatest common demeanor of all types inside it
         */
        def findLowestSuperClassOfIterable(it: Iterator[Any])(implicit rm: Mirror): Type = {
            it.map(e => rm.reflect(e).symbol.asClass.baseClasses)
              .iterator
              .reduce((l1, l2) => l1.filter(x => l2.contains(x)))
              .head
              .asClass
              .toType
        }
    
        /**
         * is used to turn :: into List
         * @param l a value that represents a list in the form of ::
         * @return the Type of List with tparams reflected from l
         */
        def createListType(l: Any): Type = {
            val iterable = l.asInstanceOf[::[_]]
            val ListParam = findLowestSuperClassOfIterable(iterable.iterator)
            if (isColonColonType(ListParam))
                typeRef(listType, listSymbol, List(createListType(iterable.asInstanceOf[::[::[_]]].flatten)))
            else
                typeRef(listType, listSymbol, List(ListParam))
        }
    
        /**
         * is used to turn var args into Seq
         * @param l a value that represents a var arg
         * @return the Type of Seq with tparams reflected from l
         */
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
            val returnType = reflectedSymbol.asClass.toType
            if ( reflectedSymbol.typeParams.nonEmpty && tpe.baseClasses != minimumBaseClasses )
                returnType.baseClasses
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
                          .getOrElse(returnType)
            else if ( isRepeatedType(tpe) )
                     createSeqType(a)
            else if ( isColonColonType(returnType) )
                     createListType(a)
            else
                returnType
        }
        else if ( isRepeatedType(tpe) )
            createSeqType(a)
        else if ( isColonColonType(tpe) )
            createListType(a)
        else
            tpe
    }
    
    /**
     * creates a string representation for a given type, such that it can be loaded through RTLoader.load,
     * CTLoader.obj, or getTypeFromString
     * @param t the type that will be converted to a string
     * @param typeParams the type params that will be included in the string representation of t
     * @return a String representation for type t in the for of t[typeParams]
     */
    def createStringRepresentation(t: Type)(typeParams: List[Type] = getTypeParams(t)): String =
        if (typeParams.isEmpty)
            t.typeSymbol.fullName
        else
            s"${t.typeSymbol.fullName}[${typeParams.map(t => createStringRepresentation(t)()).mkString(",")}]"
    
}
