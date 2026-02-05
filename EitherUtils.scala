package org.encalmo.utils

import scala.quoted.*

object EitherUtils {

  object TypeReprIsEither {
    def unapply(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] = {
      import quotes.reflect.*
      val eitherSym = Symbol.requiredClass("scala.util.Either")
      // 1. Check inheritance
      if (tpe.derivesFrom(eitherSym)) {
        // 2. Upcast to Either[L, R] to resolve type arguments safely
        val base = tpe.baseType(eitherSym)
        base match {
          case AppliedType(_, List(left, right)) =>
            Some((left, right))
          case _ =>
            None
        }
      } else {
        None
      }
    }
  }

  def buildMatchTerm(using
      cache: StatementsCache
  )(
      leftTpe: cache.quotes.reflect.TypeRepr,
      rightTpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnLeft: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term,
      functionOnRight: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Match = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Resolve Class Symbols
    val leftClassSym = Symbol.requiredClass("scala.util.Left")
    val rightClassSym = Symbol.requiredClass("scala.util.Right")

    // --- Helper to build a case: case temp: Class[L, R] => handler(temp.value) ---
    def buildCase(
        classSym: Symbol,
        tpeArgs: List[TypeRepr],
        handler: (TypeRepr, Term) => Term,
        handlerTpe: TypeRepr
    ): CaseDef = {
      // A. Construct the specific type: Left[L, R] or Right[L, R]
      val caseType = TypeIdent(classSym).tpe.appliedTo(tpeArgs)

      // B. Create Bind Symbol 'temp'
      val tempSym = Symbol.newBind(Symbol.spliceOwner, TypeNameUtils.valueNameOf(caseType), Flags.EmptyFlags, caseType)

      // C. Create Pattern: temp @ (_ : CaseType)
      val typeCheckPattern = Typed(Wildcard(), Inferred(caseType))
      val bindPattern = Bind(tempSym, typeCheckPattern)

      // D. Access the 'value' field
      // Both Left and Right are case classes with exactly one field named 'value'
      val valueSym = classSym.caseFields.head
      val extractedValue = Select(Ref(tempSym), valueSym)

      // E. Return the CaseDef
      CaseDef(bindPattern, None, handler(handlerTpe, extractedValue))
    }

    // 3. Build the two cases
    // Note: We must pass BOTH [L, R] to Left and Right types, not just one.
    // Left[String, Int] and Right[String, Int] are the valid subtypes of Either[String, Int].
    val typeArgs = List(leftTpe, rightTpe)
    val caseLeft = buildCase(leftClassSym, typeArgs, functionOnLeft, leftTpe)
    val caseRight = buildCase(rightClassSym, typeArgs, functionOnRight, rightTpe)

    // 4. Construct Match
    Match(target, List(caseLeft, caseRight))
  }
}
