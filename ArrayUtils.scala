package org.encalmo.utils

import scala.quoted.*

object ArrayUtils {

  /** Build an efficient loop over an array. */
  def buildArrayLoop(using
      cache: StatementsCache
  )(
      arrayNamePrefix: String,
      tpe: cache.quotes.reflect.TypeRepr,
      target: cache.quotes.reflect.Term,
      functionOnItem: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val itemType = tpe
    val intType = TypeRepr.of[Int]

    // 1. Array Variable
    val arraySym =
      Symbol.newVal(Symbol.spliceOwner, arrayNamePrefix + "Array", target.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
    val arrayValDef = ValDef(arraySym, Some(target))
    val arrayRef = Ref(arraySym)

    // 2. Length Variable
    val lengthSym = defn.ArrayClass.methodMember("length").head
    val lenSym =
      Symbol.newVal(Symbol.spliceOwner, arrayNamePrefix + "ArrayLength", intType, Flags.EmptyFlags, Symbol.noSymbol)
    val lenValDef = ValDef(lenSym, Some(Select(arrayRef, lengthSym)))
    val lenRef = Ref(lenSym)

    // 3. Index Variable
    val indexSym =
      Symbol.newVal(Symbol.spliceOwner, arrayNamePrefix + "ArrayIndex", intType, Flags.Mutable, Symbol.noSymbol)
    val indexValDef = ValDef(indexSym, Some(Literal(IntConstant(0))))
    val indexRef = Ref(indexSym)

    // 4. Condition: i < len
    // FIX: Select the correct '<' overload by checking signature "I" (Int)
    val lessThanSym = defn.IntClass
      .methodMember("<")
      .find { sym =>
        sym.signature.paramSigs match {
          case List("I") | List("scala.Int") => true
          case _                             => false
        }
      }
      .getOrElse(report.errorAndAbort("Could not find Int.<(Int)"))

    val condition = Apply(Select(indexRef, lessThanSym), List(lenRef))

    // 5. Loop Body
    val loopBody = {
      // A. Access Item: arr(i)
      val applySym = defn.ArrayClass.methodMember("apply").head
      val itemTerm = Apply(Select(arrayRef, applySym), List(indexRef))

      val itemSym = Symbol.newVal(Symbol.spliceOwner, arrayNamePrefix, itemType, Flags.EmptyFlags, Symbol.noSymbol)
      val itemValDef = ValDef(itemSym, Some(itemTerm))

      // B. User Logic
      val userCode = functionOnItem(tpe, Ref(itemSym))

      // C. Increment: i = i + 1
      // Note: Int.+ also has overloads, but usually Int is the default.
      // To be safe, we can use the same find() logic or rely on the fact that + usually prioritizes Int.
      val plusSym = defn.IntClass
        .methodMember("+")
        .find { sym =>
          sym.signature.paramSigs match {
            case List("I") | List("scala.Int") => true
            case _                             => false
          }
        }
        .getOrElse(defn.IntClass.methodMember("+").head)

      val nextIndex = Apply(Select(indexRef, plusSym), List(Literal(IntConstant(1))))
      val increment = Assign(indexRef, nextIndex)

      Block(
        List(itemValDef, userCode, increment),
        Literal(UnitConstant())
      )
    }

    // 6. Assemble
    Block(
      List(arrayValDef, lenValDef, indexValDef),
      While(condition, loopBody)
    )
  }

  def createArrayViaList(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      terms: List[cache.quotes.reflect.Term]
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // 1. Reuse your robust 'createStaticList' (using :: and Nil)
    val listTerm = IterableUtils.createStaticList(tpe, terms) // Calls the function we fixed previously

    // 2. Select .toArray method on the list
    // Signature: def toArray[B >: A : ClassTag]: Array[B]
    val toArraySym = Symbol
      .requiredClass("scala.collection.IterableOnceOps")
      .methodMember("toArray")
      .head

    // 3. Resolve ClassTag (Required for toArray as well)
    val itemType = tpe
    val classTagType = TypeRepr.of[scala.reflect.ClassTag].appliedTo(itemType)
    val classTagTerm = Implicits.search(classTagType) match {
      case iss: ImplicitSearchSuccess => iss.tree
      case _: ImplicitSearchFailure   =>
        report.errorAndAbort(s"ClassTag required for Array creation not found for ${itemType.show}")
    }

    // 4. Construct: list.toArray[T](classTag)
    val sel = Select(listTerm, toArraySym)
    val typeApplied = TypeApply(sel, List(Inferred(itemType)))

    Apply(typeApplied, List(classTagTerm))
  }

}
