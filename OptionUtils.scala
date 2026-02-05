package org.encalmo.utils

import scala.quoted.*

object OptionUtils {

  object TypeReprIsOption {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*
      // 1. Resolve the Option symbol
      val optionSym = Symbol.requiredClass("scala.Option")
      // 2. Check if the type is a subtype of Option
      if (tpe.derivesFrom(optionSym)) {
        // 3. Upcast to Option[T]
        val base = tpe.baseType(optionSym)
        base match {
          // 4. Extract the inner type T
          case AppliedType(_, List(innerType)) =>
            Some(innerType)
          // Edge case: Raw Option or unexpected structure
          case _ =>
            Some(TypeRepr.of[Any])
        }
      } else {
        None
      }
    }
  }

  def buildMatchTerm(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnSome: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term,
      functionOnNone: cache.quotes.reflect.Term
  ): cache.quotes.reflect.Match = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val someClassSym = Symbol.requiredClass("scala.Some")
    val noneModuleSym = Symbol.requiredModule("scala.None")

    // -------------------------------------------------------------------
    // CASE 1: case temp: Some[T] => onSome(temp.value)
    // -------------------------------------------------------------------

    // A. Create type: Some[T]
    val someType = TypeRepr.of[Some].appliedTo(tpe)

    // B. Create Bind Symbol 'temp' of type Some[T]
    val argSym =
      Symbol.newBind(Symbol.spliceOwner, TypeNameUtils.valueNameOf(tpe), Flags.EmptyFlags, someType)

    // C. Create Pattern: temp @ (_ : Some[T])
    // This explicitly forces an 'isInstanceOf[Some[T]]' check.
    val typeCheckPattern = Typed(Wildcard(), Inferred(someType))
    val bindPattern = Bind(argSym, typeCheckPattern)

    // D. Create Body: temp.value
    // We access the 'value' accessor of the Some class.
    val valueSym = someClassSym.caseFields.head
    val extractedValue = Select(Ref(argSym), valueSym)

    val caseSome =
      tpe.asType match {
        case '[t] =>
          CaseDef(bindPattern, None, functionOnSome(tpe, extractedValue))
      }

    // -------------------------------------------------------------------
    // CASE 2: case None => onNone
    // -------------------------------------------------------------------

    // Standard object match pattern
    val caseNone = CaseDef(Ref(noneModuleSym), None, functionOnNone)

    // -------------------------------------------------------------------
    // 3. Build Match
    // -------------------------------------------------------------------
    Match(target, List(caseSome, caseNone))
  }

}
