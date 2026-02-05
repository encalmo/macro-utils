package org.encalmo.utils

import scala.quoted.*

object MapUtils {

  object TypeReprIsMap {
    def unapply(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] = {
      import quotes.reflect.*
      // 1. Resolve the Map symbol
      // We use "scala.collection.Map" to cover both mutable and immutable maps
      val mapSym = Symbol.requiredClass("scala.collection.Map")
      // 2. Check if the type is a subtype of Map
      if (tpe.derivesFrom(mapSym)) {
        // 3. Upcast to Map[K, V] to get the specific type arguments
        val base = tpe.baseType(mapSym)
        base match {
          // 4. Extract Key and Value
          case AppliedType(_, List(keyType, valueType)) =>
            Some((keyType, valueType))

          // Edge case: Raw Map (Map[?, ?]) or errors
          case _ =>
            Some((TypeRepr.of[Any], TypeRepr.of[Any]))
        }
      } else {
        None
      }
    }
  }

  def buildMapLoop(using
      cache: StatementsCache
  )(
      iteratorName: String,
      keyTpe: cache.quotes.reflect.TypeRepr,
      valueTpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnEntry: (
          cache.quotes.reflect.Term,
          cache.quotes.reflect.Term
      ) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val tupleType = TypeRepr.of[Tuple2].appliedTo(List(keyTpe, valueTpe)) // (K, V)

    // FIX 1: Explicitly resolve Tuple2 symbols using caseFields
    // Tuple2 is a case class, so caseFields returns List(_1, _2)
    val tuple2Class = Symbol.requiredClass("scala.Tuple2")
    val _1Sym = tuple2Class.caseFields(0) // Symbol for ._1
    val _2Sym = tuple2Class.caseFields(1) // Symbol for ._2

    def call(obj: Term, methodName: String): Term = {
      val sym = obj.tpe.typeSymbol.methodMember(methodName).head
      val sel = Select(obj, sym)
      if (sym.paramSymss.headOption.contains(Nil)) Apply(sel, Nil) else sel
    }

    // 1. Iterator Setup
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

    // 2. Loop Condition
    val condition = call(iteratorRef, "hasNext")

    // 3. Loop Body
    val loopBody = {
      // A. val pair = it.next()
      val nextTerm = call(iteratorRef, "next")

      val pairSym = Symbol.newVal(
        Symbol.spliceOwner,
        "pair",
        tupleType,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val pairValDef = ValDef(pairSym, Some(nextTerm))
      val pairRef = Ref(pairSym)

      // B. val key = pair._1
      val keySym = Symbol.newVal(Symbol.spliceOwner, "key", keyTpe, Flags.EmptyFlags, Symbol.noSymbol)
      // FIX 2: Use the resolved _1Sym
      val keyValDef = ValDef(keySym, Some(Select(pairRef, _1Sym)))

      // C. val value = pair._2
      val valueSym = Symbol.newVal(Symbol.spliceOwner, "value", valueTpe, Flags.EmptyFlags, Symbol.noSymbol)
      // FIX 3: Use the resolved _2Sym
      val valueValDef = ValDef(valueSym, Some(Select(pairRef, _2Sym)))

      // D. Generate User Code: onItem(key, value)
      val userCode = functionOnEntry(Ref(keySym), Ref(valueSym))

      Block(
        List(pairValDef, keyValDef, valueValDef, userCode),
        Literal(UnitConstant())
      )
    }

    // 4. While Loop
    val whileTerm = While(condition, loopBody)

    Block(List(iteratorValDef), whileTerm)
  }

}
