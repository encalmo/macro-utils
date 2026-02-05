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

  /** Visit a named tuple and collect the results into a block of unit.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def collect[In: Type](using
      quotes: Quotes
  )(
      label: Option[Expr[String]],
      valueExpr: Expr[In],
      functionWhenTupleExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any],
      functionWhenNamedTupleExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any],
      onStart: Expr[Unit],
      onEnd: Expr[Unit]
  ): Expr[Unit] = {
    import quotes.reflect.*

    Type.of[In] match {
      case '[head *: tail] =>
        '{
          ${ onStart }
          ${ collectTuple[In, head, tail](label, valueExpr, functionWhenTupleExpr, 0) }
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
                            index
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

  private def collectTuple[In: Type, head: Type, tail <: Tuple: Type](using
      quotes: Quotes
  )(
      label: Option[Expr[String]],
      valueExpr: Expr[In],
      functionExpr: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any],
      n: Int
  ): Expr[Unit] = {
    import quotes.reflect.*
    '{
      ${
        functionExpr.apply[head](
          label,
          '{ $valueExpr.asInstanceOf[Product].productElement(${ Expr(n) }).asInstanceOf[head] },
          n
        )
      }

      ${
        Type.of[tail] match {
          case '[head2 *: tail2] =>
            collectTuple[In, head2, tail2](
              label = label,
              valueExpr = valueExpr,
              functionExpr = functionExpr,
              n = n + 1
            )

          case '[scala.EmptyTuple] =>
            '{}
        }
      }

    }
  }

  /** Visit a named tuple using a statements cache.
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
      label: Option[String],
      valueTerm: cache.quotes.reflect.Term,
      functionWhenTupleExpr: [A: Type] => (
          cache.quotes.reflect.TypeRepr,
          Option[String],
          cache.quotes.reflect.Term,
          Int
      ) => Unit,
      functionWhenNamedTupleExpr: [A: Type] => (
          cache.quotes.reflect.TypeRepr,
          Option[String],
          cache.quotes.reflect.Term,
          Int
      ) => Unit,
      onStart: cache.quotes.reflect.Term,
      onEnd: cache.quotes.reflect.Term
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    Type.of[In] match {

      case '[NamedTuple.AnyNamedTuple] =>
        cache.put(onStart)
        val productElementMethodSym = MethodUtils.findMethodByArity(TypeRepr.of[Product], "productElement", 1)
        TypeRepr.of[In].dealias match {
          case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>
            nameTypeList
              .zip(valueTypeList)
              .zipWithIndex
              .foreach { case ((nameTpe, valueTpe), index) =>
                valueTpe.asType match {
                  case '[value] =>
                    functionWhenNamedTupleExpr.apply[value](
                      valueTpe,
                      Some(TypeNameUtils.shortBaseName(nameTpe.show(using Printer.TypeReprShortCode))),
                      Apply(
                        Select(valueTerm.callAsInstanceOf[Product], productElementMethodSym),
                        List(Literal(IntConstant(index)))
                      ).callAsInstanceOf(Inferred(valueTpe)),
                      index
                    )
                }
              }
          case _ => ()
        }
        cache.put(onEnd)

      case '[head *: tail] =>
        val (headTpe, tailTpes) =
          TypeRepr.of[In] match {
            // Case 1: Standard Tuple2, Tuple3, etc.
            // (Union, Union) is represented as AppliedType(Tuple2, List(Union, Union))
            case AppliedType(tpe, args) if tpe.typeSymbol.name.startsWith("Tuple") =>
              (args.head, args.tail)

            // Case 2: Recursive Cons (*:) structure
            // head *: tail is AppliedType(*:, List(head, tail))
            case AppliedType(tycon, head :: tail :: Nil) if tycon.typeSymbol.name == "*:" =>
              (head, TypeUtils.tupleTypeToTypeList[tail])

            case _ =>
              (TypeRepr.of[head], TypeUtils.tupleTypeToTypeList[tail])
          }

        cache.put(onStart)
        visitTuple[In](headTpe, tailTpes, label, valueTerm, functionWhenTupleExpr, 0)
        cache.put(onEnd)

      case '[scala.EmptyTuple] =>
        cache.put(onStart)
        cache.put(onEnd)

      case '[scala.NonEmptyTuple] =>
        cache.put(onStart)
        cache.put(onEnd)

      case _ =>
        ()
    }
  }

  private def visitTuple[In: Type](using
      cache: StatementsCache
  )(
      headTpe: cache.quotes.reflect.TypeRepr,
      tailTpes: List[cache.quotes.reflect.TypeRepr],
      label: Option[String],
      valueTerm: cache.quotes.reflect.Term,
      functionExpr: [A: Type] => (
          cache.quotes.reflect.TypeRepr,
          Option[String],
          cache.quotes.reflect.Term,
          Int
      ) => Unit,
      n: Int
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    headTpe.asType match {
      case '[head] =>
        functionExpr.apply[head](
          headTpe,
          label,
          valueTerm
            .methodCall("productElement", List(Literal(IntConstant(n))))
            .callAsInstanceOf(cache.quotes.reflect.Inferred(headTpe)),
          n
        )
    }

    tailTpes match {
      case headTpe2 :: tailTpes2 =>
        visitTuple[In](
          headTpe2,
          tailTpes2,
          label,
          valueTerm,
          functionExpr,
          n + 1
        )
      case Nil =>
        ()
    }
  }

  def createTuple2(using
      cache: StatementsCache
  )(
      key: cache.quotes.reflect.Term,
      value: cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Resolve Symbols
    val tupleObjSym = Symbol.requiredModule("scala.Tuple2") // The object Tuple2
    val applySym = tupleObjSym
      .methodMember("apply")
      .headOption
      .getOrElse(report.errorAndAbort("Could not find apply method on Tuple2")) // The 'apply' method

    // 2. Prepare Types [K, V]
    // We need to widen types to avoid over-specific singleton types (e.g. "foo".type)
    val keyType = key.tpe.widen
    val valType = value.tpe.widen

    // 3. Construct the Call
    // AST: Tuple2.apply[KeyType, ValType](key, value)

    // A. Select 'apply'
    val sel = Select(Ref(tupleObjSym), applySym)

    // B. Apply Type Arguments [K, V]
    // Since 'apply' is generic: def apply[T1, T2](_1: T1, _2: T2): (T1, T2)
    val typedFun = TypeApply(sel, List(Inferred(keyType), Inferred(valType)))

    // C. Apply Value Arguments (key, value)
    Apply(typedFun, List(key, value))
  }

}
