package org.encalmo.utils

import scala.quoted.*

object OpaqueTypeUtils {

  /** Check if a type is an opaque type and return the underlying upper bound type if it is. */
  object TypeReprIsOpaqueType {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Option[quotes.reflect.TypeRepr]] = {
      import quotes.reflect.*
      if (tpe.typeSymbol.flags.is(Flags.Opaque))
      then
        Some {
          val bases = tpe.baseClasses
          val underlyingClass = bases.find { s =>
            s != defn.AnyClass && s != defn.MatchableClass && s != defn.ObjectClass
          }
          underlyingClass match {
            case Some(superSym) =>
              val bound = tpe.baseType(superSym)
              Some(bound)

            case None =>
              findBaseTypeFromUnapply(tpe)
                .orElse(
                  findBaseTypeFromApply(tpe)
                )
          }
        }
      else None

    }
  }

  def findBaseTypeFromUnapply(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    // 1. Get the companion object symbol and its type reference
    val sym = tpe.typeSymbol
    for {
      companionSym <-
        if sym.companionModule.isNoSymbol
        then
          // Look inside the scope where the type is defined (the owner)
          // and find the object (Module) with the exact same name.
          sym.owner.declarations.find(s => s.name == sym.name && s.flags.is(Flags.Module))
        else Some(sym.companionModule)
      companionTpe = companionSym.termRef
      unapplyMethods = companionSym.declaredMethod("unapply")
      // 2. Find the 'unapply' method
      filteredUnapplyMethods = unapplyMethods.filter(method =>
        method.paramSymss.size == 1
          && method.paramSymss.head.size == 1
          && {
            val methodSignature = companionTpe.memberType(method)
            methodSignature match {
              // accept only methods with a single parameter of the same type as the opaque type
              case mt: MethodType => mt.paramTypes.head =:= tpe
              case _              => false
            }
          }
      )
      unapplyMethod <- filteredUnapplyMethods.headOption
      // 3. Get the method signature
      methodSignature = companionTpe.memberType(unapplyMethod)
      // 4. Extract the return type (e.g., Option[Document] or Some[Document])
      returnType <- methodSignature match {
        case MethodType(_, _, res) => Some(res)
        case _                     => None
      }
      // 5. Unwrap the Option[_] or Some[_] to get the inner type using AppliedType
      // .widen is used in case it returns Some[Document] instead of Option[Document]
      underlyingType <- returnType.widen.dealias match
        // Matches Option[X] or Some[X] and captures X as innerType
        case AppliedType(tycon, List(innerType))
            if tycon.typeSymbol == defn.OptionClass || returnType <:< TypeRepr.of[Option[Any]] =>
          Some(innerType)
        case other if !(other =:= TypeRepr.of[Boolean]) => Some(other)
        case _                                          => None
    } yield underlyingType
  }

  def findBaseTypeFromApply(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    // 1. Get the companion object symbol and its type reference
    val sym = tpe.typeSymbol
    for {
      companionSym <-
        if sym.companionModule.isNoSymbol
        then
          // Look inside the scope where the type is defined (the owner)
          // and find the object (Module) with the exact same name.
          sym.owner.declarations.find(s => s.name == sym.name && s.flags.is(Flags.Module))
        else Some(sym.companionModule)
      companionTpe = companionSym.termRef
      applyMethods = companionSym.declaredMethod("apply")
      // 2. Find the 'apply' method
      filteredApplyMethods = applyMethods.filter(method =>
        val methodSignature = companionTpe.memberType(method)
        methodSignature match {
          // accept only methods returning the same type as the opaque type
          case MethodType(_, _, res) => res =:= tpe
          case other                 => false
        }
      )
      applyMethod <- filteredApplyMethods.headOption
      // 3. Get the method signature
      methodSignature = companionTpe.memberType(applyMethod)
      // 4. Inspect the Implementation Body (Deep Reflection)
      // This requires the compiler to have access to the source AST (same project)
      underlyingType <- applyMethod.tree match {
        case DefDef(_, _, _, Some(rhs)) =>
          // We look for the function call or constructor usage
          rhs match {
            case Apply(Select(New(typeTree), _), _) =>
              // Case: new XYZ(...)
              Some(typeTree.tpe)

            case Apply(Select(ident, "apply"), _) =>
              // Case: XYZ.apply(...) - typical for case classes
              // ident.tpe will point to the XYZ object type
              Some(ident.tpe.widen)

            case Apply(TypeApply(Select(ident, "apply"), typeArgs), _) =>
              Some(ident.tpe.appliedTo(typeArgs.map(_.tpe)))

            case ident @ Ident(identifier) =>
              Some(ident.tpe.widen)

            case other =>
              // Could not parse method body structure
              None
          }

        case tree @ DefDef(_, _, _, None) =>
          // Cannot access source code/tree for this method
          None

        case other =>
          // Cannot access source code/tree for this method
          None
      }
    } yield underlyingType
  }

  /** Visit an opaque type and apply a dedicated function when the type is an opaque type or when the type is not an
    * opaque type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visit(using
      cache: StatementsCache
  )(
      label: String,
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionWhenOpaqueType: (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term
      ) => Unit,
      functionOtherwise: (cache.quotes.reflect.TypeRepr, String, cache.quotes.reflect.Term) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes

    TypeUtils
      .underlyingTypeRepr(tpe)
      .match {
        case Left((tpe)) => functionWhenOpaqueType(tpe, label, valueTerm)
        case _           => functionOtherwise(tpe, label, valueTerm)
      }
  }

  /** Visit an opaque type and apply a dedicated function when the type is an opaque type or when the type is not an
    * opaque type without the value term.
    *
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visitTermless(using
      cache: StatementsCache
  )(
      label: String,
      tpe: cache.quotes.reflect.TypeRepr,
      functionWhenOpaqueType: (
          cache.quotes.reflect.TypeRepr,
          String
      ) => Unit,
      functionOtherwise: (cache.quotes.reflect.TypeRepr, String) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes

    TypeUtils
      .underlyingTypeRepr(tpe)
      .match {
        case Left((tpe)) => functionWhenOpaqueType(tpe, label)
        case _           => functionOtherwise(tpe, label)
      }
  }

}
