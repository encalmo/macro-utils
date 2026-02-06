package org.encalmo.utils

import scala.quoted.*
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import org.encalmo.utils.AnnotationUtils.computeInfo

object EnumUtils {

  enum EnumType {
    case Value
    case Class
    case Java
    case Unknown
  }

  /** Check if a type is a Scala3 enum or a sealed abstract type, or a Java enum. */
  object TypeReprIsEnum {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
      import quotes.reflect.*
      val flags = tpe.dealias.typeSymbol.flags
      (flags.is(Flags.Sealed) && (flags.is(Flags.Trait) || flags.is(Flags.Abstract)))
      || (flags.is(Flags.JavaDefined) && flags.is(Flags.Enum))
    }
  }

  /** Check if a type is an enum or a sealed abstract type. */
  def isEnumOrSealedADT[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Sealed)
    && (
      TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Trait)
        || TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Abstract)
    )
  }

  /** Check if a type is a Java enum. */
  def isJavaEnum[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    val sym = TypeRepr.of[A].dealias.typeSymbol
    sym.flags.is(Flags.JavaDefined) && sym.flags.is(Flags.Enum)
  }

  def hasEnumCaseClasses(using cache: StatementsCache)(tpe: cache.quotes.reflect.TypeRepr): Boolean = {
    findEnumCases(tpe).exists(_._2 == EnumType.Class)
  }

  def findEnumCases(using
      cache: StatementsCache
  )(tpe: cache.quotes.reflect.TypeRepr): List[(cache.quotes.reflect.Symbol, EnumType)] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val enumType = tpe.dealias
    val enumSymbol = enumType.typeSymbol
    val enumCompanion = enumSymbol.companionModule

    def enumCases: List[Symbol] = {
      if enumSymbol.flags.is(Flags.JavaDefined)
        && enumSymbol.flags.is(Flags.Enum)
      then
        enumSymbol.companionModule.moduleClass.declarations
          .filter { member => member.flags.is(Flags.Enum) }
      else if enumSymbol.flags.is(Flags.Sealed)
        && (enumSymbol.flags.is(Flags.Enum) || enumSymbol.flags.is(Flags.Trait))
      then enumSymbol.children
      else Nil
    }

    enumCases.map { enumCase =>
      val enumType =
        if enumCompanion.declaredType(enumCase.name).nonEmpty then EnumType.Class
        else if enumCompanion.declaredField(enumCase.name).exists then EnumType.Value
        else if enumCase.flags.is(Flags.JavaDefined) && enumCase.flags.is(Flags.Enum) then EnumType.Java
        else EnumType.Unknown
      (enumCase, enumType)
    }
  }

  /** Visit an enum, create a pattern match stattement of enum cases.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def transformToMatchTerm(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionWhenCaseValue: (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term,
          Set[AnnotationInfo]
      ) => cache.quotes.reflect.Term,
      functionWhenCaseClass: (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term,
          Set[AnnotationInfo]
      ) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val enumType = tpe.dealias
    val enumSymbol = enumType.typeSymbol
    val enumCompanion = enumSymbol.companionModule

    def enumCases: List[Symbol] = {
      if enumSymbol.flags.is(Flags.JavaDefined)
        && enumSymbol.flags.is(Flags.Enum)
      then
        enumSymbol.companionModule.moduleClass.declarations
          .filter { member => member.flags.is(Flags.Enum) }
      else if enumSymbol.flags.is(Flags.Sealed)
        && (enumSymbol.flags.is(Flags.Enum) || enumSymbol.flags.is(Flags.Trait))
      then enumSymbol.children
      else Nil
    }

    if !enumCases.isEmpty
    then {

      val matchCaseDefs = enumCases
        .map { enumCase =>
          val isEnumCaseValue = enumCompanion.declaredField(enumCase.name).exists
          val isEnumCaseType = enumCompanion.declaredType(enumCase.name).nonEmpty
          val isJavaEnumCase = enumCase.flags.is(Flags.JavaDefined) && enumCase.flags.is(Flags.Enum)

          val enumCaseSymbol =
            if isEnumCaseType
            then enumCompanion.declaredType(enumCase.name).headOption.get
            else if isEnumCaseValue
            then enumCompanion.declaredField(enumCase.name)
            else enumCase

          val tpe =
            if isEnumCaseType
            then TypeSelect(Ref(enumCompanion), enumCase.name).tpe
            else if isEnumCaseValue
            then Select(Ref(enumCompanion), enumCase).tpe
            else enumCase.typeRef

          val bindSym = Symbol.newBind(
            Symbol.spliceOwner,
            TypeNameUtils.valueNameOf(tpe),
            Flags.EmptyFlags,
            tpe
          )

          val matchCasePattern =
            if isEnumCaseType
            then {
              val typeCheckPattern = Typed(Wildcard(), TypeSelect(Ref(enumCompanion), enumCase.name))
              Bind(bindSym, typeCheckPattern)
            } else if enumCompanion.declaredField(enumCase.name).exists
            then Ref(enumCase)
            else if isJavaEnumCase
            then Ref(enumCase)
            else {
              val typeCheckPattern = Typed(Wildcard(), TypeTree.of(using enumCase.typeRef.asType))
              Bind(bindSym, typeCheckPattern)
            }

          val matchCaseBody =
            tpe.asType match {
              case '[t] =>
                if (enumCase.isTerm)
                then
                  functionWhenCaseValue(
                    tpe,
                    enumCase.name,
                    Ref(enumCase),
                    enumCaseSymbol.annotations.computeInfo
                  )
                else
                  functionWhenCaseClass(
                    tpe,
                    enumCase.name,
                    Ref(bindSym),
                    enumCaseSymbol.annotations.computeInfo
                  )
            }

          CaseDef(matchCasePattern, None, matchCaseBody)
        }

      Match(valueTerm, matchCaseDefs)
    } else report.errorAndAbort(s"The type ${tpe.show} is not an enum or sealed ADT")
  }

}
