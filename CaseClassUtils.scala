package org.encalmo.utils

import scala.quoted.*
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import scala.deriving.Mirror

object CaseClassUtils {

  /** Check if a type is a case class. */
  def isCaseClass[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias.typeSymbol.isClassDef
    && TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Case)
  }

  /** Transform a case class into a list of output values returned by the provided function. Filter out returned values
    * that are None.
    *
    * @param value
    *   The instance of the case class to transform.
    * @param function
    *   The function to apply to each field.
    * @return
    *   A list of output values.
    */

  def transformToListOfExpr[In: Type, Out: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (
          Expr[String],
          Expr[A],
          Set[AnnotationInfo]
      ) => Option[Expr[Out]]
  )(using quotes: Quotes): List[Expr[Out]] = {
    import quotes.reflect.*
    val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
      case Left(tpe)  => tpe
      case Right(tpe) => tpe
    }
    parentTpe.asType match {
      case '[p] =>
        parentTpe.typeSymbol.caseFields
          .map { caseField =>
            val tpe = parentTpe.memberType(caseField).dealias
            tpe.asType match {
              case '[t] =>
                functionExpr.apply[t](
                  Expr(caseField.name),
                  Select(valueExpr.asTerm, caseField).asExprOf[t],
                  AnnotationUtils.computeFieldAnnotations[p](caseField.name)
                )
            }
          }
          .collect { case Some(expr) => expr }
    }
  }

  /** Transform a case class into a list of output values returned by the provided function. Filter out returned values
    * that are None.
    *
    * @param value
    *   The instance of the case class to transform.
    * @param function
    *   The function to apply to each field.
    * @return
    *   A list of output values.
    */
  def transformToList[In: Type, Out](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (
          Expr[String],
          Expr[A],
          Set[AnnotationInfo]
      ) => Option[Out]
  )(using quotes: Quotes): List[Out] = {
    import quotes.reflect.*
    val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
      case Left(tpe)  => tpe
      case Right(tpe) => tpe
    }
    parentTpe.asType match {
      case '[p] =>
        parentTpe.typeSymbol.caseFields
          .map { caseField =>
            val tpe = parentTpe.memberType(caseField).dealias
            tpe.asType match {
              case '[t] =>
                functionExpr.apply[t](
                  Expr(caseField.name),
                  Select(valueExpr.asTerm, caseField).asExprOf[t],
                  AnnotationUtils.computeFieldAnnotations[p](caseField.name)
                )
            }
          }
          .collect { case Some(expr) => expr }
    }
  }

  /** Transform a case class into an expression of tuple of output values returned by the provided function. Filter out
    * returned values that are None.
    *
    * @param value
    *   The instance of the case class to transform.
    * @param function
    *   The function to apply to each field.
    * @return
    *   A list of output values.
    */

  def transformToExprOfTuple[In: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (
          Expr[String],
          Expr[A],
          Set[AnnotationInfo]
      ) => Option[Expr[Any]]
  )(using quotes: Quotes): Expr[Tuple] = {
    import quotes.reflect.*
    val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
      case Left(tpe)  => tpe
      case Right(tpe) => tpe
    }
    parentTpe.asType match {
      case '[p] =>
        def transform(caseFields: List[Symbol], result: Expr[Tuple]): Expr[Tuple] = {
          caseFields match {
            case Nil                         => result
            case caseField :: caseFieldsTail =>
              val tpe = parentTpe.memberType(caseField).dealias
              tpe.asType match {
                case '[t] =>
                  functionExpr
                    .apply[t](
                      Expr(caseField.name),
                      Select(valueExpr.asTerm, caseField).asExprOf[t],
                      AnnotationUtils.computeFieldAnnotations[p](caseField.name)
                    )
                    .match {
                      case Some(expr) =>
                        expr.asTerm.tpe.asType match {
                          case '[e] =>
                            transform(caseFieldsTail, '{ ${ expr.asExprOf[e] } *: ${ result } })
                        }
                      case None => transform(caseFieldsTail, result)
                    }
              }
          }
        }

        transform(parentTpe.typeSymbol.caseFields.reverse, '{ EmptyTuple })
    }
  }

  /** Transform a case class into a tuple of output values returned by the provided function. Filter out returned values
    * that are None.
    *
    * @param value
    *   The instance of the case class to transform.
    * @param function
    *   The function to apply to each field.
    * @return
    *   A list of output values.
    */

  def transformToTuple[In: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (
          Expr[String],
          Expr[A],
          Set[AnnotationInfo]
      ) => Option[Any]
  )(using quotes: Quotes): Tuple = {
    import quotes.reflect.*
    val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
      case Left(tpe)  => tpe
      case Right(tpe) => tpe
    }
    parentTpe.asType match {
      case '[p] =>
        def transform(caseFields: List[Symbol], result: Tuple): Tuple = {
          caseFields match {
            case Nil                         => result
            case caseField :: caseFieldsTail =>
              val tpe = parentTpe.memberType(caseField).dealias
              tpe.asType match {
                case '[t] =>
                  functionExpr
                    .apply[t](
                      Expr(caseField.name),
                      Select(valueExpr.asTerm, caseField).asExprOf[t],
                      AnnotationUtils.computeFieldAnnotations[p](caseField.name)
                    )
                    .match {
                      case Some(value) => transform(caseFieldsTail, value *: result)
                      case None        => transform(caseFieldsTail, result)
                    }
              }
          }
        }

        transform(parentTpe.typeSymbol.caseFields.reverse, EmptyTuple)
    }
  }

  /** Visit a case class and apply a function to each field, then collect the results into a block of unit.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def collect[In: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (
          Expr[String],
          Expr[A],
          Set[AnnotationInfo]
      ) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*
    Expr.block(
      {
        val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
          case Left(tpe)  => tpe
          case Right(tpe) => tpe
        }
        parentTpe.asType match {
          case '[p] =>
            parentTpe.typeSymbol.caseFields
              .map { caseField =>
                val tpe = parentTpe.memberType(caseField).dealias
                tpe.asType match {
                  case '[t] =>
                    functionExpr.apply[t](
                      Expr(caseField.name),
                      Select(valueExpr.asTerm, caseField).asExprOf[t],
                      AnnotationUtils.computeFieldAnnotations[p](caseField.name)
                    )
                }
              }
        }
      },
      '{}
    )
  }

  /** Visit a case class and apply a function to each field using a statements cache.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visit[In: Type](using
      cache: StatementsCache
  )(
      valueTerm: cache.quotes.reflect.Term,
      functionExpr: [A: Type] => (
          cache.quotes.reflect.TypeRepr,
          String, // name
          cache.quotes.reflect.Term, // value
          Set[AnnotationInfo] // annotations
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    {
      val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
        case Left(tpe)  => tpe
        case Right(tpe) => tpe
      }
      parentTpe.asType match {
        case '[p] =>
          parentTpe.typeSymbol.caseFields
            .map { caseField =>
              val tpe = parentTpe.memberType(caseField).dealias
              tpe.asType match {
                case '[t] =>
                  functionExpr.apply[t](
                    tpe,
                    caseField.name,
                    Select(valueTerm, caseField),
                    AnnotationUtils.computeFieldAnnotationsFromTpe(parentTpe, caseField.name)
                  )
              }
            }
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
