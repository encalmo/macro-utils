package org.encalmo.utils

import scala.quoted.*

object AnnotationUtils {

  case class AnnotationInfo(name: String, params: Map[String, Any]) {
    override def toString: String =
      s"@${TypeNameUtils.shortBaseName(name)}(${params.map { case (k, v) => s"$k=$v" }.mkString(", ")})"
  }

  def annotationsOf(using cache: StatementsCache)(tpe: cache.quotes.reflect.TypeRepr): Set[AnnotationInfo] = {
    given cache.quotes.type = cache.quotes
    tpe.typeSymbol.annotations.computeInfo
  }

  def getFieldAnnotations[A: Type](
      fieldName: String
  )(using Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    TypeRepr
      .of[A]
      .typeSymbol
      .primaryConstructor
      .paramSymss
      .flatten
      .find(_.name == fieldName)
      .getOrElse(
        report.errorAndAbort(
          s"Field '$fieldName' not found in ${TypeRepr.of[A].typeSymbol.name}"
        )
      )
      .annotations
  }

  def computeFieldAnnotations(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      fieldName: String
  ): Set[AnnotationInfo] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    tpe.typeSymbol.primaryConstructor.paramSymss.flatten
      .find(_.name == fieldName)
      .getOrElse(
        report.errorAndAbort(
          s"Field '$fieldName' not found in ${tpe.typeSymbol.name}"
        )
      )
      .annotations
      .computeInfo
  }

  def getValueAnnotations(using
      cache: StatementsCache
  )(valueTerm: cache.quotes.reflect.Term): Set[AnnotationInfo] = {
    import cache.quotes.reflect.*
    // STEP 1: Unwrap the 'Inlined' wrapper to get the actual variable reference
    val term = valueTerm match {
      case Inlined(_, _, t) => t
      case t                => t
    }
    val sym = term.symbol
    sym.annotations.computeInfo
      ++ {
        if (term.symbol.isDefDef)
        then sym.owner.fieldMember(sym.name).annotations
        else Nil
      }.computeInfo
  }

  extension (annotations: Set[AnnotationInfo]) {

    def exists[Annotation <: scala.annotation.StaticAnnotation: Type](using cache: StatementsCache): Boolean =
      import cache.quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations.exists(_.name == name)

    def getTermOrDefault[Annotation <: scala.annotation.StaticAnnotation: Type](using
        cache: StatementsCache
    )(parameter: String, defaultValue: cache.quotes.reflect.Term): cache.quotes.reflect.Term =
      import cache.quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(
          _.params
            .get(parameter)
            .map {
              case value: String =>
                Literal(StringConstant(value))
              case value: Int =>
                Literal(IntConstant(value))
              case value: Boolean =>
                Literal(BooleanConstant(value))
              case value: Double =>
                Literal(DoubleConstant(value))
              case value: Float =>
                Literal(FloatConstant(value))
              case value: Long =>
                Literal(LongConstant(value))
              case value: Short =>
                Literal(ShortConstant(value))
              case value: Byte =>
                Literal(ByteConstant(value))
              case value: Char =>
                Literal(CharConstant(value))
              case value =>
                report.errorAndAbort(s"Unsupported value type: ${value.getClass.getName}")
            }
        )
        .getOrElse(defaultValue)

    def getStringOrDefault[Annotation <: scala.annotation.StaticAnnotation: Type](using
        cache: StatementsCache
    )(parameter: String, defaultValue: String): String =
      import cache.quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(
          _.params
            .get(parameter)
            .map {
              case value: String => value
              case value         =>
                report.errorAndAbort(s"Expected String but got ${value.getClass.getName} for parameter '$parameter'")
            }
        )
        .getOrElse(defaultValue)

    def getTerm[Annotation <: scala.annotation.StaticAnnotation: Type](using
        cache: StatementsCache
    )(parameter: String): Option[cache.quotes.reflect.Term] =
      import cache.quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(_.params.get(parameter).map {
          case value: String =>
            Literal(StringConstant(value))
          case value: Int =>
            Literal(IntConstant(value))
          case value: Boolean =>
            Literal(BooleanConstant(value))
          case value: Double =>
            Literal(DoubleConstant(value))
          case value: Float =>
            Literal(FloatConstant(value))
          case value: Long =>
            Literal(LongConstant(value))
          case value: Short =>
            Literal(ShortConstant(value))
          case value: Byte =>
            Literal(ByteConstant(value))
          case value: Char =>
            Literal(CharConstant(value))
          case value =>
            report.errorAndAbort(s"Unsupported value type: ${value.getClass.getName}")
        })

    def getString[Annotation <: scala.annotation.StaticAnnotation: Type](using
        cache: StatementsCache
    )(parameter: String): Option[String] =
      import cache.quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(_.params.get(parameter).map {
          case value: String => value
          case value         =>
            report.errorAndAbort(s"Expected String but got ${value.getClass.getName} for parameter '$parameter'")
        })
  }

  extension (using cache: StatementsCache)(annotations: List[cache.quotes.reflect.Term]) {

    def computeInfo: Set[AnnotationInfo] =
      import cache.quotes.reflect.*

      annotations.map { term =>
        val name = term.tpe.dealias.show(using Printer.TypeReprCode)
        val paramsNames = term.tpe.typeSymbol.caseFields.map(_.name)
        val paramsValues = term match {
          case Apply(Select(New(TypeIdent(_)), "<init>"), params) =>
            params.collect { case Literal(constant) => constant.value }
          case _ => Nil
        }
        val params = paramsNames.zip(paramsValues).toMap
        AnnotationInfo(name, params)
      }.toSet
  }

}
