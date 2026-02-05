package org.encalmo.utils

import scala.quoted.*

object IterableUtils {

  object TypeReprIsIterable {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*
      val iterableSym = Symbol.requiredClass("scala.collection.Iterable")
      // FIX: Check if the type actually implements Iterable using 'derivesFrom'
      if (tpe.derivesFrom(iterableSym)) {
        // It is safe to call baseType now
        tpe.baseType(iterableSym) match {
          case AppliedType(_, List(arg)) => Some(arg)
          case _                         => Some(TypeRepr.of[Any]) // E.g. raw Iterable without type args
        }
      } else {
        // Not an Iterable
        None
      }
    }
  }

  def buildIterableLoop(using
      cache: StatementsCache
  )(
      iteratorName: String,
      tpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnItem: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val itemType = tpe
    val iterableType = TypeRepr.of[Iterable].appliedTo(itemType)

    // FIX 1: Explicitly cast the target to Iterable[A]
    // This ensures that 'iterator' is selected from a known trait,
    // solving the "does not have a denotation" error.
    val typedTarget = Typed(target, Inferred(iterableType))

    // 2. Resolve Standard Symbols
    val iterableClass = Symbol.requiredClass("scala.collection.Iterable")
    val iteratorClass = Symbol.requiredClass("scala.collection.Iterator")

    val iteratorMethodSym = iterableClass.methodMember("iterator").head
    val hasNextSym = iteratorClass.methodMember("hasNext").head
    val nextSym = iteratorClass.methodMember("next").head

    // 3. Create Iterator Variable
    // Select from the TYPED target, not the raw target
    val iteratorTerm = Select(typedTarget, iteratorMethodSym)

    val iteratorType = TypeRepr.of[Iterator].appliedTo(itemType)

    val iteratorSym = Symbol.newVal(
      Symbol.spliceOwner,
      iteratorName,
      iteratorType,
      Flags.Mutable,
      Symbol.noSymbol
    )
    val iteratorValDef = ValDef(iteratorSym, Some(iteratorTerm))
    val iteratorRef = Ref(iteratorSym)

    // 4. Build Loop Condition: it.hasNext
    val condition = Select(iteratorRef, hasNextSym)

    // 5. Build Loop Body
    val loopBody = {
      // it.next()
      val nextTerm = Apply(Select(iteratorRef, nextSym), Nil)

      val itemSym = Symbol.newVal(
        Symbol.spliceOwner,
        "x",
        itemType,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val itemValDef = ValDef(itemSym, Some(nextTerm))

      val userCode = functionOnItem(itemType, Ref(itemSym))

      Block(
        List(itemValDef, userCode),
        Literal(UnitConstant())
      )
    }

    // 6. Return Block
    val whileTerm = While(condition, loopBody)
    Block(List(iteratorValDef), whileTerm)
  }

  def buildIterableLoop2(using
      cache: StatementsCache
  )(
      iteratorName: String,
      tpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnItem: (
          cache.quotes.reflect.TypeRepr,
          cache.quotes.reflect.Term
      ) => cache.quotes.reflect.Term // Logic to apply to each 'item'
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Determine the Item Type (T)
    // We extract T from Iterable[T] to type the loop variable correctly.
    val iterableType = Symbol.requiredClass("scala.collection.Iterable")
    val itemType = tpe

    // --- Helper: Call a method, handling () vs no-args automatically ---
    def call(obj: Term, methodName: String): Term = {
      val typeSymbol = obj.tpe.typeSymbol
      val sym = typeSymbol
        .methodMember(methodName)
        .headOption
        .getOrElse(report.errorAndAbort(s"Could not find `$methodName` method on ${typeSymbol.name}"))

      val sel = Select(obj, sym)

      // Check if method expects empty parens: def foo() vs def foo
      if (sym.paramSymss.headOption.contains(Nil)) {
        Apply(sel, Nil) // foo()
      } else {
        sel // foo
      }
    }

    // 2. Create Iterator Variable: "val it = target.iterator"
    // We must bind this to a val, otherwise 'target.iterator' would reset every loop.
    val iteratorTerm = call(target, "iterator")
    val iteratorSym = Symbol.newVal(
      Symbol.spliceOwner,
      iteratorName,
      iteratorTerm.tpe.widen,
      Flags.EmptyFlags,
      Symbol.noSymbol
    )
    val iteratorValDef = ValDef(iteratorSym, Some(iteratorTerm))
    val iteratorRef = Ref(iteratorSym)

    // 3. Build Loop Condition: "it.hasNext"
    val condition = call(iteratorRef, "hasNext")
    val valueName = TypeNameUtils.valueNameOf(tpe)

    // 4. Build Loop Body: "{ val x = it.next(); onItem(x) }"
    val loopBody = {
      // A. Call next()
      val nextTerm = call(iteratorRef, "next")

      // B. Bind result to 'x' (so user logic can reference it safely)
      val itemSym = Symbol.newVal(
        Symbol.spliceOwner,
        valueName,
        itemType,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val itemValDef = ValDef(itemSym, Some(nextTerm))

      // C. Generate User Code
      val userCode = functionOnItem(itemType, Ref(itemSym))

      // D. Block(val x = ..., userCode, ())
      Block(
        List(itemValDef, userCode),
        Literal(UnitConstant())
      )
    }

    // 5. Build While Loop
    val whileTerm = While(condition, loopBody)

    // 6. Return Block(val it = ..., while(...))
    Block(List(iteratorValDef), whileTerm)
  }

  /** Create a static list from a list of terms. */
  def createStaticList(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      terms: List[cache.quotes.reflect.Term]
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val itemType = tpe

    // 1. Define Nil
    val nil: cache.quotes.reflect.Term =
      Ref(Symbol.requiredModule("scala.collection.immutable.Nil"))

    // 2. Define Cons Module (::)
    // The object name is encoded as "$colon$colon"
    val consModule = Symbol.requiredModule("scala.collection.immutable.$colon$colon")
    val applySym = consModule.methodMember("apply").head

    // 3. Fold Right using ::.apply[T](head, tail)
    terms.foldRight(nil) { (term, tail) =>
      // A. Select 'apply' from the companion object
      val sel = Select(Ref(consModule), applySym)

      // B. Apply Type Argument [T]
      // Generates: ::.apply[T]
      // This turns the PolyType into a MethodType
      val typedFun = TypeApply(sel, List(Inferred(itemType)))

      // C. Apply Value Arguments (head, tail)
      Apply(typedFun, List(term, tail))
    }
  }

}
