package org.encalmo.utils

import scala.quoted.*

object JavaMapUtils {

  object TypeReprIsJavaMap {
    def unapply(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] = {
      import quotes.reflect.*
      // 1. Normalize: Widen (singleton -> type) and Dealias (alias -> underlying)
      val normalized = tpe.widen.dealias
      // 2. Resolve Java Map Symbol
      val mapSym = Symbol.requiredClass("java.util.Map")
      // 3. Check Inheritance
      if (normalized.derivesFrom(mapSym)) {
        // 4. Upcast to java.util.Map[K, V]
        val base = normalized.baseType(mapSym)
        base match {
          case AppliedType(_, List(keyArg, valueArg)) =>
            // Helper to handle wildcards (e.g., Map.of often returns Map[? <: K, ? <: V])
            def unwrap(t: TypeRepr): TypeRepr = t match {
              case TypeBounds(_, hi) => hi // Extract 'String' from '? <: String'
              case concrete          => concrete
            }
            Some((unwrap(keyArg), unwrap(valueArg)))
          // Edge case: Raw types (e.g. raw 'java.util.HashMap')
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

    // 1. Resolve Java Symbols
    val mapSym = Symbol.requiredClass("java.util.Map")
    val entrySym = Symbol.requiredClass("java.util.Map.Entry") // Nested interface
    val setSym = Symbol.requiredClass("java.util.Set")
    val iteratorSym = Symbol.requiredClass("java.util.Iterator")

    // 2. Resolve Methods
    val entrySetMethod = mapSym.methodMember("entrySet").head
    val iteratorMethod = setSym.methodMember("iterator").head
    val hasNextMethod = iteratorSym.methodMember("hasNext").head
    val nextMethod = iteratorSym.methodMember("next").head
    val getKeyMethod = entrySym.methodMember("getKey").head
    val getValueMethod = entrySym.methodMember("getValue").head

    // 3. Construct Types
    // Type: java.util.Map.Entry[K, V]
    val entryType = entrySym.typeRef.appliedTo(List(keyTpe, valueTpe))
    // Type: java.util.Iterator[java.util.Map.Entry[K, V]]
    val iteratorType = iteratorSym.typeRef.appliedTo(entryType)
    // Type: java.util.Map[K, V] (for casting)
    val mapType = mapSym.typeRef.appliedTo(List(keyTpe, valueTpe))

    // 4. Cast Target to Map[K, V]
    // Crucial for handling private subclasses (e.g. Map.of)
    val typedTarget = Typed(target, Inferred(mapType))

    // 5. Create Iterator Chain: map.entrySet().iterator()
    // A. call .entrySet()
    val entrySetCall = Apply(Select(typedTarget, entrySetMethod), Nil)
    // B. call .iterator() on the result
    val iteratorCall = Apply(Select(entrySetCall, iteratorMethod), Nil)

    // Define 'val it = ...'
    val iteratorSymVal = Symbol.newVal(
      Symbol.spliceOwner,
      iteratorName + "Iterator",
      iteratorType,
      Flags.EmptyFlags,
      Symbol.noSymbol
    )
    val iteratorValDef = ValDef(iteratorSymVal, Some(iteratorCall))
    val iteratorRef = Ref(iteratorSymVal)

    // 6. Loop Condition: it.hasNext()
    val condition = Apply(Select(iteratorRef, hasNextMethod), Nil)

    // 7. Loop Body
    val loopBody = {
      // A. val entry = it.next()
      val nextCall = Apply(Select(iteratorRef, nextMethod), Nil)
      val entryValSym = Symbol.newVal(Symbol.spliceOwner, "entry", entryType, Flags.EmptyFlags, Symbol.noSymbol)
      val entryValDef = ValDef(entryValSym, Some(nextCall))
      val entryRef = Ref(entryValSym)

      // B. val k = entry.getKey()
      val keyCall = Apply(Select(entryRef, getKeyMethod), Nil)
      val keyValSym = Symbol.newVal(Symbol.spliceOwner, "key", keyTpe, Flags.EmptyFlags, Symbol.noSymbol)
      val keyValDef = ValDef(keyValSym, Some(keyCall))

      // C. val v = entry.getValue()
      val valueCall = Apply(Select(entryRef, getValueMethod), Nil)
      val valValSym = Symbol.newVal(Symbol.spliceOwner, "value", valueTpe, Flags.EmptyFlags, Symbol.noSymbol)
      val valValDef = ValDef(valValSym, Some(valueCall))

      // D. User Logic: functionOnEntry(k, v)
      val userCode = functionOnEntry(Ref(keyValSym), Ref(valValSym))

      // E. Block Construction
      Block(
        List(entryValDef, keyValDef, valValDef, userCode),
        Literal(UnitConstant())
      )
    }

    // 8. Assemble Loop
    Block(
      List(iteratorValDef),
      While(condition, loopBody)
    )
  }

}
