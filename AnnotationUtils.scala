package org.encalmo.utils

import scala.quoted.*

object AnnotationUtils {

  case class AnnotationInfo(name: String, params: Map[String, Any]) {
    override def toString: String =
      s"@${TypeNameUtils.shortBaseName(name)}(${params.map { case (k, v) => s"$k=$v" }.mkString(", ")})"
  }

  def annotationsOf[A: Type](using Quotes): Set[AnnotationInfo] = {
    import quotes.reflect.*
    TypeRepr
      .of[A]
      .typeSymbol
      .annotations
      .computeInfo
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

  def getValueAnnotations[A: Type](value: Expr[A])(using Quotes): Set[AnnotationInfo] = {
    import quotes.reflect.*
    // STEP 1: Unwrap the 'Inlined' wrapper to get the actual variable reference
    val term = value.asTerm match {
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
    def exists[Annotation <: scala.annotation.StaticAnnotation: Type](using Quotes): Boolean =
      import quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations.exists(_.name == name)

    def getOrDefault[Annotation <: scala.annotation.StaticAnnotation: Type, T: ToExpr](using
        Quotes
    )(parameter: String, defaultValue: Expr[T]): Expr[T] =
      import quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(_.params.get(parameter).map(value => Expr(value.asInstanceOf[T])))
        .getOrElse(defaultValue)

    def get[Annotation <: scala.annotation.StaticAnnotation: Type, T: ToExpr](using
        Quotes
    )(parameter: String): Option[Expr[T]] =
      import quotes.reflect.*
      val name = TypeRepr.of[Annotation].show(using Printer.TypeReprCode)
      annotations
        .find(_.name == name)
        .flatMap(_.params.get(parameter).map(value => Expr(value.asInstanceOf[T])))
  }

  extension (using Quotes)(annotations: List[quotes.reflect.Term]) {

    def computeInfo: Set[AnnotationInfo] =
      import quotes.reflect.*
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

    /** Check if contains an annotation of the given type. */
    def exists[Annotation <: scala.annotation.StaticAnnotation: Type]: Boolean =
      annotations.exists(
        _.tpe =:= quotes.reflect.TypeRepr.of[Annotation]
      )

    /** Get the value of an annotation of the given type, or return the default value if annotation not exists.
      */
    def getOrDefault[
        Annotation <: scala.annotation.StaticAnnotation: Type,
        T: Type: ToExpr
    ](
        parameter: String,
        defaultValue: Expr[T]
    ): Expr[T] =
      import quotes.reflect.*
      annotations
        .find(_.tpe =:= quotes.reflect.TypeRepr.of[Annotation])
        .flatMap { term =>
          val paramIndex = term.tpe.typeSymbol.caseFields.indexWhere(_.name == parameter)
          term match {
            case Apply(_, params) =>
              params(paramIndex) match {
                case Literal(constant) => Some(Expr(constant.value.asInstanceOf[T]))
              }
            case _ => None
          }
        }
        .getOrElse(defaultValue)

    /** Get the value of an annotation of the given type. */
    def get[
        Annotation <: scala.annotation.StaticAnnotation: Type,
        T: Type: ToExpr
    ](
        parameter: String
    ): Option[Expr[T]] =
      import quotes.reflect.*
      annotations
        .find(_.tpe =:= quotes.reflect.TypeRepr.of[Annotation])
        .flatMap { term =>
          val paramIndex = term.tpe.typeSymbol.caseFields.indexWhere(_.name == parameter)
          term match {
            case Apply(_, params) =>
              params(paramIndex) match {
                case Literal(constant) => Some(Expr(constant.value.asInstanceOf[T]))
              }
            case _ => None
          }
        }

  }

}
