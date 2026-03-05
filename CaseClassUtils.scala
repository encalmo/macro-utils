package org.encalmo.utils

import scala.quoted.*
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import scala.deriving.Mirror

object CaseClassUtils {

  object TypeReprIsCaseClass {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
      import quotes.reflect.*
      tpe.dealias.typeSymbol.isClassDef && tpe.dealias.typeSymbol.flags.is(Flags.Case)
    }
  }

  /** Check if a type is a case class. */
  def isCaseClass[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias.typeSymbol.isClassDef
    && TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Case)
  }

  /** Visit a case class and apply a function to each field using a statements cache.
    *
    * @param tpe
    * @param valueTerm
    * @param functionOnField
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visit(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnField: (
          cache.quotes.reflect.TypeRepr, // type of the field
          String, // name of the field
          cache.quotes.reflect.Term, // value of the field
          Set[AnnotationInfo] // annotations
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    {
      val parentTpe = TypeUtils.underlyingTypeRepr(tpe) match {
        case Left(tpe)  => tpe
        case Right(tpe) => tpe
      }
      parentTpe.typeSymbol.caseFields
        .map { caseField =>
          val fieldTpe = valueTerm.tpe.memberType(caseField)
          functionOnField.apply(
            fieldTpe,
            caseField.name,
            Select(valueTerm, caseField),
            AnnotationUtils.computeFieldAnnotations(parentTpe, caseField.name)
          )
        }
    }
  }

  /** Visit a case class and apply a function to each field using a statements cache without the value term.
    *
    * @param tpe
    *   the type of the case class
    * @param functionOnField
    *   the function to apply to each field
    * @param cache
    *   implicit statements cache to store the generated code
    * @return
    *   Unit
    */
  def visitTermless(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      functionOnField: (
          cache.quotes.reflect.TypeRepr, // type of the field
          String, // name of the field
          Set[AnnotationInfo] // annotations
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    {
      val parentTpe = TypeUtils.underlyingTypeRepr(tpe) match {
        case Left(tpe)  => tpe
        case Right(tpe) => tpe
      }
      parentTpe.typeSymbol.caseFields
        .map { caseField =>
          val fieldTpe = tpe.memberType(caseField)
          functionOnField.apply(
            fieldTpe,
            caseField.name,
            AnnotationUtils.computeFieldAnnotations(parentTpe, caseField.name)
          )
        }
    }
  }

  /** Create an instance of a case class using its primary constructor.
    *
    * @param args
    *   The arguments to pass to the constructor.
    * @return
    *   An instance of the case class.
    */
  def createInstanceUsingConstructor[R <: Product: Type](using Quotes)(args: List[quotes.reflect.Term]): Expr[R] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[R].dealias
    val sym = tpe.typeSymbol
    if (!sym.isClassDef || !sym.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${tpe.show} must be a Case Class")
    }

    val primaryConstr = sym.primaryConstructor
    if (primaryConstr.isNoSymbol) {
      report.errorAndAbort(s"Could not find primary constructor for ${tpe.show}")
    }

    // Create 'new Class' using the raw Symbol
    val newT = New(TypeTree.ref(sym))
    // Select the constructor
    val con = Select(newT, primaryConstr)
    // Check if T has type arguments (e.g. Box[String])
    val typedCon = tpe match {
      case AppliedType(_, typeArgs) =>
        // If T = Box[String], we apply [String] here
        val typeTrees = typeArgs.map(arg => TypeTree.of(using arg.asType))
        TypeApply(con, typeTrees)
      case _ =>
        con
    }
    val applied = Apply(typedCon, args)

    applied.asExprOf[R]
  }

  /** Create an instance of a case class using a tuple.
    *
    * @param tuple
    *   The tuple to pass to the constructor.
    * @return
    *   An instance of the case class.
    */
  def createInstanceFromTuple[R <: Product: Type](using Quotes)(tuple: Expr[Tuple]): Expr[R] = {
    Expr
      .summon[Mirror.ProductOf[R]]
      .map(mirror => '{ ${ mirror }.fromProduct(${ tuple }) })
      .get
  }

}
