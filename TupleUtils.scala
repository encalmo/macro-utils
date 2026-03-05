package org.encalmo.utils

import scala.quoted.*

object TupleUtils {

  object TypeReprIsTuple {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
      import quotes.reflect.*
      // Check if the type derives from the core scala.Tuple trait.
      // This covers:
      // 1. Standard Tuples: (A, B) -> Tuple2[A, B]
      // 2. Generic Tuples: A *: B *: EmptyTuple
      // 3. Type aliases pointing to tuples
      tpe.derivesFrom(Symbol.requiredClass("scala.Tuple"))
    }
  }

  def isTuple[A: Type](using Quotes): Boolean =
    Type.of[A] match {
      case '[head *: tail]        => true
      case '[scala.EmptyTuple]    => true
      case '[scala.NonEmptyTuple] => true
      case _                      => false
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
      functionOnItem: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any]
  ): Expr[Unit] = {
    import quotes.reflect.*

    Type.of[In] match {
      case '[head *: tail] =>
        collectTuple[In, head, tail](label, valueExpr, functionOnItem, 0)

      case _ =>
        '{}
    }
  }

  private def collectTuple[In: Type, head: Type, tail <: Tuple: Type](using
      quotes: Quotes
  )(
      label: Option[Expr[String]],
      valueExpr: Expr[In],
      functionOnItem: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any],
      n: Int
  ): Expr[Unit] = {
    import quotes.reflect.*
    '{
      ${
        functionOnItem.apply[head](
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
              functionOnItem = functionOnItem,
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
  def visit(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnItem: (
          cache.quotes.reflect.TypeRepr,
          cache.quotes.reflect.Term,
          Int
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    tpe.asType match {
      case '[head *: tail] =>
        val (headTpe, tailTpes) =
          tpe match {
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

        visitTuple(tpe, headTpe, tailTpes, valueTerm, functionOnItem, 0)

      case _ => ()
    }
  }

  private def visitTuple(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      headTpe: cache.quotes.reflect.TypeRepr,
      tailTpes: List[cache.quotes.reflect.TypeRepr],
      valueTerm: cache.quotes.reflect.Term,
      functionOnItem: (
          cache.quotes.reflect.TypeRepr,
          cache.quotes.reflect.Term,
          Int
      ) => Unit,
      n: Int
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    headTpe.asType match {
      case '[head] =>
        functionOnItem(
          headTpe,
          valueTerm
            .methodCall("productElement", List(Literal(IntConstant(n))))
            .callAsInstanceOf(cache.quotes.reflect.Inferred(headTpe)),
          n
        )
    }

    tailTpes match {
      case headTpe2 :: tailTpes2 =>
        visitTuple(
          tpe,
          headTpe2,
          tailTpes2,
          valueTerm,
          functionOnItem,
          n + 1
        )
      case Nil =>
        ()
    }
  }

  /** Visit a named tuple using a statements cache without the value term.
    *
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visitTermless(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      functionOnItem: (
          cache.quotes.reflect.TypeRepr,
          Int
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    tpe.asType match {
      case '[head *: tail] =>
        val (headTpe, tailTpes) =
          tpe match {
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

        visitTupleTermless(tpe, headTpe, tailTpes, functionOnItem, 0)

      case _ => ()
    }
  }

  private def visitTupleTermless(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      headTpe: cache.quotes.reflect.TypeRepr,
      tailTpes: List[cache.quotes.reflect.TypeRepr],
      functionOnItem: (
          cache.quotes.reflect.TypeRepr,
          Int
      ) => Unit,
      n: Int
  ): Unit = {
    given cache.quotes.type = cache.quotes

    headTpe.asType match {
      case '[head] =>
        functionOnItem(
          headTpe,
          n
        )
    }

    tailTpes match {
      case headTpe2 :: tailTpes2 =>
        visitTupleTermless(
          tpe,
          headTpe2,
          tailTpes2,
          functionOnItem,
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
