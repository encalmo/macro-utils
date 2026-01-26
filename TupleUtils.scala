

package org.encalmo.utils

import scala.quoted.*

object TupleUtils {

  def isTuple[A: Type](using Quotes): Boolean =
    Type.of[A] match {
      case '[head *: tail]        => true
      case '[scala.EmptyTuple]    => true
      case '[scala.NonEmptyTuple] => true
      case _                      => false
    }

  def isNamedTuple[A: Type](using Quotes): Boolean =
    Type.of[A] match {
      case '[NamedTuple.AnyNamedTuple] => true
      case _                           => false
    }

  /** Visit a named tuple.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def visit[In: Type](using
      quotes: Quotes
  )(
      label: Option[Expr[String]],
      valueExpr: Expr[In],
      functionWhenTupleExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Expr[Int]
      ) => Expr[Any],
      functionWhenNamedTupleExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Expr[Int]
      ) => Expr[Any],
      onStart: Expr[Unit],
      onEnd: Expr[Unit]
  ): Expr[Unit] = {
    import quotes.reflect.*

    Type.of[In] match {
      case '[head *: tail] =>
        '{
          ${ onStart }
          ${ visitTuple[In, head, tail](label, valueExpr, functionWhenTupleExpr, Expr(0)) }
          ${ onEnd }
        }

      case '[NamedTuple.AnyNamedTuple] =>
        '{
          ${ onStart }
          ${
            TypeRepr.of[In].dealias match {
              case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>
                Expr.block(
                  nameTypeList
                    .zip(valueTypeList)
                    .zipWithIndex
                    .map { case ((name, valueType), index) =>
                      valueType.asType match {
                        case '[value] =>
                          functionWhenNamedTupleExpr.apply[value](
                            Some(Expr(TypeNameUtils.shortBaseName(name.show(using Printer.TypeReprShortCode)))),
                            '{ $valueExpr.asInstanceOf[Product].productElement(${ Expr(index) }).asInstanceOf[value] },
                            Expr(index)
                          )
                      }
                    },
                  '{}
                )
              case _ => '{}
            }
          }
          ${ onEnd }
        }

      case '[scala.EmptyTuple] =>
        '{
          ${ onStart }
          ${ onEnd }
        }

      case '[scala.NonEmptyTuple] =>
        '{
          ${ onStart }
          ${ onEnd }
        }

      case _ =>
        '{}
    }
  }

  private def visitTuple[In: Type, head: Type, tail <: Tuple: Type](using
      quotes: Quotes
  )(
      label: Option[Expr[String]],
      valueExpr: Expr[In],
      functionExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Expr[Int]
      ) => Expr[Any],
      n: Expr[Int] = Expr(0)
  ): Expr[Unit] = {
    import quotes.reflect.*
    '{
      ${
        functionExpr.apply[head](
          label,
          '{ $valueExpr.asInstanceOf[Product].productElement(${ n }).asInstanceOf[head] },
          n
        )
      }

      ${
        Type.of[tail] match {
          case '[head2 *: tail2] =>
            visitTuple[In, head2, tail2](
              label = label,
              valueExpr = valueExpr,
              functionExpr = functionExpr,
              n = '{ ${ n } + 1 }
            )

          case '[scala.EmptyTuple] =>
            '{}
        }
      }

    }
  }
}
