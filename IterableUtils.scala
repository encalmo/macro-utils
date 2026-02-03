package org.encalmo.utils

object IterableUtils {

  import scala.quoted.*

  def buildIterableLoop(using
      cache: StatementsCache
  )[A: Type](
      target: cache.quotes.reflect.Term,
      onItem: [A: Type] => cache.quotes.reflect.Term => cache.quotes.reflect.Term // Logic to apply to each 'item'
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Determine the Item Type (T)
    // We extract T from Iterable[T] to type the loop variable correctly.
    val iterableType = Symbol.requiredClass("scala.collection.Iterable")
    val itemType = TypeRepr.of[A]

    // --- Helper: Call a method, handling () vs no-args automatically ---
    def call(obj: Term, methodName: String): Term = {
      val sym = obj.tpe.typeSymbol.methodMember(methodName).head
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
      "it",
      iteratorTerm.tpe.widen,
      Flags.Mutable,
      Symbol.noSymbol
    )
    val iteratorValDef = ValDef(iteratorSym, Some(iteratorTerm))
    val iteratorRef = Ref(iteratorSym)

    // 3. Build Loop Condition: "it.hasNext"
    val condition = call(iteratorRef, "hasNext")

    // 4. Build Loop Body: "{ val x = it.next(); onItem(x) }"
    val loopBody = {
      // A. Call next()
      val nextTerm = call(iteratorRef, "next")

      // B. Bind result to 'x' (so user logic can reference it safely)
      val itemSym = Symbol.newVal(
        Symbol.spliceOwner,
        "x",
        itemType,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val itemValDef = ValDef(itemSym, Some(nextTerm))

      // C. Generate User Code
      val userCode =
        itemType.asType match {
          case '[t] =>
            onItem(Ref(itemSym))
        }

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
  def createStaticList[A: Type](using
      cache: StatementsCache
  )(
      terms: List[cache.quotes.reflect.Term]
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val itemType = TypeRepr.of[A]

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
