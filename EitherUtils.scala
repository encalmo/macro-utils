package org.encalmo.utils

import scala.quoted.*

object EitherUtils {

  def buildMatchTerm[L: Type, R: Type](using
      cache: StatementsCache
  )(
      target: cache.quotes.reflect.Term,
      onLeft: [B: Type] => cache.quotes.reflect.Term => cache.quotes.reflect.Term,
      onRight: [C: Type] => cache.quotes.reflect.Term => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Match = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Resolve Class Symbols
    val leftClassSym = Symbol.requiredClass("scala.util.Left")
    val rightClassSym = Symbol.requiredClass("scala.util.Right")

    // 2. Extract Types L and R from Either[L, R]
    val leftTypeArg = TypeRepr.of[L]
    val rightTypeArg = TypeRepr.of[R]

    // --- Helper to build a case: case temp: Class[L, R] => handler(temp.value) ---
    def buildCase[T: Type](
        classSym: Symbol,
        tpeArgs: List[TypeRepr],
        handler: [T: Type] => Term => Term
    ): CaseDef = {
      // A. Construct the specific type: Left[L, R] or Right[L, R]
      val caseType = TypeIdent(classSym).tpe.appliedTo(tpeArgs)

      // B. Create Bind Symbol 'temp'
      val tempSym = Symbol.newBind(Symbol.spliceOwner, TypeNameUtils.valueNameOf[T], Flags.EmptyFlags, caseType)

      // C. Create Pattern: temp @ (_ : CaseType)
      val typeCheckPattern = Typed(Wildcard(), Inferred(caseType))
      val bindPattern = Bind(tempSym, typeCheckPattern)

      // D. Access the 'value' field
      // Both Left and Right are case classes with exactly one field named 'value'
      val valueSym = classSym.caseFields.head
      val extractedValue = Select(Ref(tempSym), valueSym)

      // E. Return the CaseDef
      CaseDef(bindPattern, None, handler[T](extractedValue))
    }

    // 3. Build the two cases
    // Note: We must pass BOTH [L, R] to Left and Right types, not just one.
    // Left[String, Int] and Right[String, Int] are the valid subtypes of Either[String, Int].
    val typeArgs = List(leftTypeArg, rightTypeArg)

    val caseLeft =
      leftTypeArg.asType match {
        case '[l] =>
          buildCase[l](leftClassSym, typeArgs, onLeft)
      }
    val caseRight =
      rightTypeArg.asType match {
        case '[r] =>
          buildCase[r](rightClassSym, typeArgs, onRight)
      }

    // 4. Construct Match
    Match(target, List(caseLeft, caseRight))
  }
}
