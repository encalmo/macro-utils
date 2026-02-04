package org.encalmo.utils

import scala.quoted.*

object OptionUtils {

  def buildMatchTerm[A: Type](using
      cache: StatementsCache
  )(
      target: cache.quotes.reflect.Term,
      onSome: [B: Type] => cache.quotes.reflect.Term => cache.quotes.reflect.Term,
      onNone: cache.quotes.reflect.Term
  ): cache.quotes.reflect.Match = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val someClassSym = Symbol.requiredClass("scala.Some")
    val noneModuleSym = Symbol.requiredModule("scala.None")

    // 1. Determine Content Type (T) from Option[T]
    val contentType = TypeRepr.of[A]

    // -------------------------------------------------------------------
    // CASE 1: case temp: Some[T] => onSome(temp.value)
    // -------------------------------------------------------------------

    // A. Create type: Some[T]
    val someType = TypeRepr.of[Some].appliedTo(contentType)

    // B. Create Bind Symbol 'temp' of type Some[T]
    val argSym =
      Symbol.newBind(Symbol.spliceOwner, TypeNameUtils.valueNameOf[A], Flags.EmptyFlags, someType)

    // C. Create Pattern: temp @ (_ : Some[T])
    // This explicitly forces an 'isInstanceOf[Some[T]]' check.
    val typeCheckPattern = Typed(Wildcard(), Inferred(someType))
    val bindPattern = Bind(argSym, typeCheckPattern)

    // D. Create Body: temp.value
    // We access the 'value' accessor of the Some class.
    val valueSym = someClassSym.caseFields.head
    val extractedValue = Select(Ref(argSym), valueSym)

    val caseSome =
      contentType.asType match {
        case '[t] =>
          CaseDef(bindPattern, None, onSome[t](extractedValue))
      }

    // -------------------------------------------------------------------
    // CASE 2: case None => onNone
    // -------------------------------------------------------------------

    // Standard object match pattern
    val caseNone = CaseDef(Ref(noneModuleSym), None, onNone)

    // -------------------------------------------------------------------
    // 3. Build Match
    // -------------------------------------------------------------------
    Match(target, List(caseSome, caseNone))
  }

}
