package org.encalmo.utils

import scala.quoted.*
import scala.deriving.Mirror

object TypeUtils {

  /** Simplify a type to its underlying type, use dealias and tree matching to handle opaque types. Returns a Left of an
    * upper bound of the opaque type if not Any, otherwise a Right containing dealiased type.
    */
  def underlyingTypeRepr[A: Type](using
      Quotes
  ): Either[quotes.reflect.TypeRepr, quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[A].dealias
    tpe match {
      case _: OrType => Right(tpe) // already a union type
      case _         =>
        val sym = tpe.typeSymbol
        (if (sym.flags.is(Flags.Opaque))
         then {
           val bases = tpe.baseClasses
           val underlyingClass = bases.find { s =>
             s != defn.AnyClass && s != defn.MatchableClass && s != defn.ObjectClass
           }
           underlyingClass match {
             case Some(superSym) =>
               val bound = tpe.baseType(superSym)
               Left(bound)

             case None =>
               Right(tpe)
           }
         } else Right(sym.typeRef))
    }
  }

  extension [A: Type](using
      Quotes
  )(either: Either[quotes.reflect.TypeRepr, quotes.reflect.TypeRepr]) {
    def tpe: quotes.reflect.TypeRepr = either match {
      case Left((tpe)) => tpe
      case Right(tpe)  => tpe
    }
  }

  inline def inspectUnion[A]: List[String] = ${ inspectUnionImpl[A] }
  def inspectUnionImpl[A: Type](using
      Quotes
  ): Expr[List[String]] = {
    Expr((inspectUnionType[A].getOrElse(Nil).map(_.show)).toList)
  }

  def inspectUnionType[A: Type](using
      Quotes
  ): Option[List[quotes.reflect.TypeRepr]] = {
    import quotes.reflect.*

    // Helper to recursively flatten "A | B | C" into List(A, B, C)
    def flatten(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
      case OrType(left, right) => flatten(left) ++ flatten(right)
      case other               => List(other)
    }

    val tpe = TypeRepr.of[A].dealias
    tpe match {
      case OrType(_, _) => Some(flatten(tpe).distinct)
      case _            => None
    }
  }

  inline def isConcrete[A]: Boolean = ${ isConcreteImpl[A] }
  def isConcreteImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    val symbol = underlyingTypeRepr[A].tpe.typeSymbol

    // A type is concrete if:
    // 1. It is a Class definition
    // 2. It is NOT Abstract
    // 3. It is NOT a Trait
    val isConcrete = symbol.isClassDef &&
      !symbol.flags.is(Flags.Abstract) &&
      !symbol.flags.is(Flags.Trait)

    Expr(isConcrete)
  }

  inline def isFullyConcrete[A]: Boolean = ${ isFullyConcreteImpl[A] }
  def isFullyConcreteImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    val tpe = underlyingTypeRepr[A].tpe

    // Helper to recursively check if a type contains any "holes" (Type Parameters)
    def isResolved(tr: TypeRepr): Boolean = tr match {
      // Case 1: Standard type (e.g., String, Int)
      case t: TypeRef =>
        !t.typeSymbol.isTypeParam // If it's a 'T', it fails

      // Case 2: Applied Type (e.g., List[String], Map[Int, T])
      case AppliedType(base, args) =>
        isResolved(base) && args.forall(isResolved)

      // Case 3: Other types (AndTypes, OrTypes, etc.)
      // You might want to handle specific edge cases here
      case _ => true
    }

    val symbol = tpe.typeSymbol

    // Combine: Must be a concrete class AND have no unresolved generics
    val isInstantiable = symbol.isClassDef &&
      !symbol.flags.is(Flags.Abstract) &&
      !symbol.flags.is(Flags.Trait)

    Expr(isInstantiable && isResolved(tpe))
  }

  inline def isString[A]: Boolean = ${ isStringImpl[A] }
  def isStringImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    import qctx.reflect.*
    Expr(underlyingTypeRepr[A].tpe =:= TypeRepr.of[String])

  inline def isBoolean[A]: Boolean = ${ isBooleanImpl[A] }
  def isBooleanImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    import qctx.reflect.*
    Expr(underlyingTypeRepr[A].tpe =:= TypeRepr.of[Boolean])

  inline def isNumeric[A]: Boolean = ${ isNumericImpl[A] }
  def isNumericImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    Expr(Expr.summon[Numeric[A]].isDefined)

  inline def isCaseClass[A]: Boolean = ${ isCaseClassImpl[A] }
  def isCaseClassImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val sym = TypeRepr.of[A].dealias.typeSymbol
    Expr(
      (sym.isClassDef && sym.flags.is(Flags.Case))
    )
  }

  inline def isProduct[A]: Boolean = ${ isProductImpl[A] }
  def isProductImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    import qctx.reflect.*
    Expr(underlyingTypeRepr[A].tpe <:< TypeRepr.of[Product])

  inline def isSum[A]: Boolean = ${ isSumImpl[A] }
  def isSumImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    Expr(Expr.summon[Mirror.SumOf[A]].isDefined)

  inline def isOption[A]: Boolean = ${ isOptionImpl[A] }
  def isOptionImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    Type.of[A] match {
      case '[Option[t]] => '{ true }
      case _            => '{ false }
    }

  inline def isCollection[A]: Boolean = ${ isCollectionImpl[A] }
  def isCollectionImpl[A: Type](using qctx: Quotes): Expr[Boolean] =
    import qctx.reflect.*
    Expr(underlyingTypeRepr[A].tpe <:< TypeRepr.of[Iterable])

  inline def isEnum[A]: Boolean = ${ isEnumImpl[A] }
  def isEnumImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[A].dealias.typeSymbol
    Expr(sym.flags.is(Flags.Enum))
  }

  inline def isSealedADT[A]: Boolean = ${ isSealedADTImpl[A] }
  def isSealedADTImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[A].dealias.typeSymbol
    Expr(
      if (
        sym.flags.is(Flags.Sealed)
        && !sym.flags.is(Flags.Enum)
        && (sym.flags.is(Flags.Trait) || sym.flags.is(Flags.Abstract))
      ) true
      else false
    )

  }

  inline def isUnion[A]: Boolean = ${ isUnionImpl[A] }
  def isUnionImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A].dealias match {
      case OrType(_, _) => true
      case _            => false
    })
  }

  inline def isOpaque[A]: Boolean = ${ isOpaqueImpl[A] }
  def isOpaqueImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Opaque))
  }

  inline def isTuple[A]: Boolean = ${ isTupleImpl[A] }
  def isTupleImpl[A: Type](using Quotes): Expr[Boolean] = {
    Expr(Type.of[A] match {
      case '[head *: tail]        => true
      case '[scala.EmptyTuple]    => true
      case '[scala.NonEmptyTuple] => true
      case _                      => false
    })
  }

  inline def isNonEmptyTuple[A]: Boolean = ${ isNonEmptyTupleImpl[A] }
  def isNonEmptyTupleImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(underlyingTypeRepr[A].tpe <:< TypeRepr.of[NonEmptyTuple])
  }

  inline def isNamedTuple[A]: Boolean = ${ isNamedTupleImpl[A] }
  def isNamedTupleImpl[A: Type](using Quotes): Expr[Boolean] = {
    Expr(Type.of[A] match {
      case '[NamedTuple.AnyNamedTuple] => true
      case _                           => false
    })
  }

  inline def isJavaRecord[A]: Boolean = ${ isJavaRecordImpl[A] }
  def isJavaRecordImpl[A: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A].dealias <:< TypeRepr.typeConstructorOf(classOf[java.lang.Record]))
  }

  def tupleTypeToNameList[T: Type](using Quotes): List[String] = {
    import quotes.reflect.*
    val consType = TypeRepr.of[*:].typeSymbol
    def extract(t: TypeRepr): List[String] = t.dealias match {
      case AppliedType(base, List(head, tail)) if base.typeSymbol == consType =>
        val headName = head match {
          case ConstantType(StringConstant(s)) => s
          case _ => report.errorAndAbort(s"Expected a String literal, found: ${head.show}")
        }
        headName :: extract(tail)

      case t if t =:= TypeRepr.of[EmptyTuple] =>
        Nil

      case other =>
        report.errorAndAbort(s"Unexpected type structure: ${other.show}")
    }
    extract(TypeRepr.of[T])
  }

  def extractTermsFromList[A: Type](listExpr: Expr[List[A]])(using Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*

    listExpr match {
      // CASE 1: Standard List(a, b, c) construction
      // 'Varargs' is a special extractor that handles the sequence arguments
      case '{ List(${ Varargs(args) }*) } =>
        args.toList.map(_.asTerm)

      // CASE 2: Explicit Cons chains: head :: tail
      // e.g., 1 :: 2 :: Nil
      case '{ ($head: A) :: ($tail: List[A]) } =>
        head.asTerm :: extractTermsFromList(tail)

      // CASE 3: Empty List (Nil or List.empty)
      case '{ Nil } | '{ List.empty } =>
        Nil

      // CASE 4: The expression is not a static literal (e.g., it's a variable reference 'val x = ...; macro(x)')
      case other =>
        report.errorAndAbort(s"Expected a static List literal, but got a dynamic expression: ${other.show}")
    }
  }

}
