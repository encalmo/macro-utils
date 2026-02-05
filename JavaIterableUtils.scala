package org.encalmo.utils

import scala.quoted.*

object JavaIterableUtils {

  object TypeReprIsJavaIterable {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*

      // 1. Widen and Dealias to get the "real" type
      // widening handles singleton types (val x = ...), dealias handles type aliases
      val param = tpe.widen.dealias

      // 2. Resolve Java Iterable Symbol
      val iterableSym = Symbol.requiredClass("java.lang.Iterable")

      // 3. Check Inheritance
      if (param.derivesFrom(iterableSym)) {
        // 4. Compute the base type (e.g., Iterable[String])
        val base = param.baseType(iterableSym)

        base match {
          case AppliedType(_, List(arg)) =>
            arg match {
              // FIX: Handle Wildcards (TypeBounds)
              // java.util.List.of(1) often appears as List[? <: Integer]
              // We extract the Upper Bound (hi) to get the concrete type.
              case TypeBounds(_, hi) => Some(hi)

              // Standard case: List[String]
              case concrete => Some(concrete)
            }

          // Edge case: Raw types (e.g. java.util.ArrayList without brackets)
          // We treat this as Iterable[Any]
          case _ =>
            Some(TypeRepr.of[Any])
        }
      } else {
        None
      }
    }
  }

  def buildIterableLoop(using
      cache: StatementsCache
  )(
      iteratorName: String,
      tpe: cache.quotes.reflect.TypeRepr, // The Item Type (e.g. Integer)
      target: cache.quotes.reflect.Term, // The Collection Term
      functionOnItem: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Resolve Symbols
    val javaIterableSym = Symbol.requiredClass("java.lang.Iterable")
    val javaIteratorSym = Symbol.requiredClass("java.util.Iterator")

    val iteratorMethod = javaIterableSym.methodMember("iterator").head
    val hasNextMethod = javaIteratorSym.methodMember("hasNext").head
    val nextMethod = javaIteratorSym.methodMember("next").head

    // 2. CAST the Target to Iterable[T]
    // This solves "denotation" errors and missing methods on private subclasses.
    // Generates: (target: java.lang.Iterable[T])
    val iterableType = javaIterableSym.typeRef.appliedTo(tpe)
    val typedTarget = Typed(target, Inferred(iterableType))

    // 3. Create Iterator Variable
    // val it: java.util.Iterator[T] = ...
    val iteratorType = javaIteratorSym.typeRef.appliedTo(tpe)

    // Select .iterator() from the CASTED target
    val iteratorCall = Apply(Select(typedTarget, iteratorMethod), Nil)

    val iteratorSym = Symbol.newVal(
      Symbol.spliceOwner,
      iteratorName + "Iterator",
      iteratorType,
      Flags.EmptyFlags,
      Symbol.noSymbol
    )
    val iteratorVal = ValDef(iteratorSym, Some(iteratorCall))
    val iteratorRef = Ref(iteratorSym)

    // 4. Loop Condition: it.hasNext()
    val condition = Apply(Select(iteratorRef, hasNextMethod), Nil)

    // 5. Loop Body
    val loopBody = {
      // val item = it.next()
      val nextCall = Apply(Select(iteratorRef, nextMethod), Nil)

      val itemSym = Symbol.newVal(
        Symbol.spliceOwner,
        iteratorName + "Item",
        tpe,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val itemVal = ValDef(itemSym, Some(nextCall))

      val userCode = functionOnItem(tpe, Ref(itemSym))

      Block(
        List(itemVal, userCode),
        Literal(UnitConstant())
      )
    }

    Block(
      List(iteratorVal),
      While(condition, loopBody)
    )
  }
}
