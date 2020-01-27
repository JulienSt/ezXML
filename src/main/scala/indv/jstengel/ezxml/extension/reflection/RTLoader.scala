package indv.jstengel.ezxml.extension.reflection


import RuntimeReflectHelper._
import indv.jstengel.ezxml.extension.StringTypeTree

import scala.reflect.ClassTag
import scala.xml.{Elem, PrefixedAttribute, Text}


object RTLoader {
    
    import scala.reflect.{runtime => rt}
    import rt.{universe => ru}
    import ru._
    private implicit val rm : ru.Mirror = rt.currentMirror
    
    /**
     * Creates a constructor from the given elem through reflections and then calls said constructor to reconstruct
     * the object that is saved inside the elem
     * @param elem xml-elem containing an object created object.xml
     * @param tt same as paramType
     * @tparam A the object inside elem will be loaded as this type. Make sure to supply the correct type
     * @return Either a correctly loaded object from the given elem of type A or
     *         throws a [[ClassCastException]] containing a message on how to correct the Error
     */
    @throws[ClassCastException]
    private[ezxml] def load[A] (elem : Elem)(implicit tt : TypeTag[A], ct : ClassTag[A]) : A = {
        val loadedType   = getTypeFromString(elem.label)
        val loadedSymbol = loadedType.typeSymbol
    
//        import app.xml.XMLWrapper.NodeWrapper
//        println(elem.toPrettyXMLString)
//        println(loadedType)
    
        if ( loadedSymbol.isModuleClass )
            rm.reflectModule(loadedSymbol.owner.typeSignature.member(loadedSymbol.name.toTermName).asModule).instance
    
        else if ( isCreatedThroughObject(loadedType, loadedSymbol) )
            rm.reflectModule(loadedSymbol.asClass.companion.asModule).instance
    
        else if ( isSimpleType(loadedType) )
            stringToSimpleValue(elem \@ "value")(tagOf(loadedType))

        else if ( loadedType <:< iterableType )
                 loadIterable(elem, loadedType)
    
        else if ( loadedType <:< productType )
            loadProduct(elem, loadedType)
    
        else if ( loadedType <:< arrayType ) {
            val tParam = getTypeParams(tt.tpe).head
            val a      = Array(loadChildren(elem, tParam) : _*)
            asArrayType(a)(tagOf(tParam), classTagOf(tParam))
        }
        else
            loadClass(elem, loadedType)
        }.asInstanceOf[A]
    
    private def loadChildren[A] (elem : Elem, tParam: Type) = {
        elem.child.map{ case elem : Elem => load(elem)(tagOf(tParam), classTagOf(tParam)) }
    }
    
    private def isCreatedThroughObject[A] (loadedType : Type, loadedSymbol : Symbol) = {
        loadedSymbol.asClass.companion.typeSignature.members.nonEmpty && loadedType.members.isEmpty
    }
    
    @inline private def loadIterable[A] (elem : Elem, tpe : Type) = {
        val loadedElems = loadChildren(elem, getTypeFromString(elem.label))
        val companionSymbol  = tpe.typeSymbol.asClass.companion
        val companionMembers = companionSymbol.typeSignature.members
        companionMembers.collectFirst(companionMethodExtraction(companionSymbol, "from"))
                        .map(_(loadedElems))
                        .getOrElse{
                            val f = companionMembers.collectFirst(companionMethodExtraction(companionSymbol, "apply"))
                                                    .get
                            f(loadedElems: _*)
                        }
    }
    
    @inline private def loadProduct[A] (elem : Elem, tpe : Type) = {
        val companionSymbol  =
            try {
                getTypeFromString(elem.label).typeSymbol.asClass.companion
            } catch {
                case _: ScalaReflectionException => tpe.typeSymbol.asClass.companion
            }
        val apply = companionSymbol.typeSignature
                                   .members
                                   .collectFirst(companionMethodExtraction(companionSymbol, "apply"))
                                   .get
        val loadedParams = extractParamsForFunctionFromElem(apply, elem)
        apply(loadedParams.flatten:_*)
    }
    
    @inline private def loadClass (elem : Elem, tpe : Type) = {
        val (constructor, returnType) = loadConstructor(elem.label)
        if ( tpe <:< returnType || returnType.baseClasses == tpe.baseClasses &&
                                   getTypeParams(returnType).forall(_.baseClasses == minimumBaseClasses) ) {
            val loadedParams = extractParamsForFunctionFromElem(constructor, elem)
            constructor(loadedParams.flatten:_*)
        }
        else
            throw createException(tpe, returnType)
    }
    
    @inline private def loadConstructor (className : String) : (MethodMirror, Type) = {
        val classSymbol = getTypeFromString(className).typeSymbol.asClass
        classSymbol.typeSignature
                   .members
                   .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                   .map{ m => (rm.reflectClass(classSymbol).reflectConstructor(m.asMethod), m.returnType) }
                   .get
    }
    
    private def extractParamsForFunctionFromElem (f : ru.MethodMirror, elem : Elem): List[List[Any]] = {
        f.symbol
         .paramLists
         .map(
             _.map{ param =>
                 val paramName = param.name.toString
                 elem.attributes
                     .collectFirst{
                         case PrefixedAttribute(pre, key, Text(value), _) if key.startsWith(paramName) =>
                             stringToSimpleValue(value)(tagOf(StringTypeTree.typeFromString(pre)))
                     }
                     .getOrElse{
                         elem.child
                             .collectFirst{ case c : Elem if c.prefix == paramName =>
                                 val tpe = getTypeFromString(c.label)
                                 load(c)(tagOf(tpe), classTagOf(tpe))
                             }
                             .get
                     }
             })
    }
    
    /**
     * internal function only used in [[load]]
     * @param tpe the passed through type from the type parameter of load
     * @param returnType type of the object stored inside the xml-elem
     * @return a ClassCastException with the correct message depending on the tpe
     */
    private def createException(tpe : Type, returnType : Type): ClassCastException = {
        val e = new ClassCastException (
            if ( tpe.toString == "Nothing" )
                s"\n\tThere was no type parameter provided." +
                s"\n\tThe elem you tried to load holds an object of type: " +
                s"\n\t\t[$returnType]" +
                s"\n\tTry to provide that type as a type parameter. " +
                s"\n\tIf you wanted to load something different, " +
                s"change the elem and make sure to provide the correct type parameter." +
                s"\n\n\t(Also, don't provide the Nothing-Type. " +
                s"That would lead to this error again.)\n"
            else
                s"\n\tThe xml-structure you provided contained: \n\t\t [$returnType] " +
                s"\n\tThe type-parameter you provided is: \n\t\t [$tpe]." +
                s"\n\tSadly, you can not load [$returnType] into an object of [$tpe]" +
                s"\n\tMake sure to load the correct xml-structure!" +
                s"\n\tEither change the type parameter or load a different xml element\n"
        )
        e.setStackTrace(e.getStackTrace.drop(3))
        e
    }
}