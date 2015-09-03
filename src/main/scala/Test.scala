import shapeless._
import shapeless.ops.hlist.Selector

import Expr._

object Test {

  def addType[L <: HList](expr: Expr[L])
                         (implicit ev: L ContainsNot Type): Expr[Type :: L] = {
    expr match {
      case v: Value[L] => v.copy()(ts = Int() :: v.extensions)
      case a: Add[L]   => a.copy(lhs = addType(a.lhs),
        rhs = addType(a.rhs))(
          ts  = Int() :: a.extensions)
    }
  }

  def addMemory[L <: HList](expr: Expr[L])
                           (implicit ev0: L ContainsNot Memory,
                            ev1: L Contains Type): Expr[Memory :: L] = {
    expr match {
      case v: Value[L] => v.copy()(ts = PrivateMemory() :: v.extensions)
      case a: Add[L]   => a.copy(lhs = addMemory(a.lhs),
        rhs = addMemory(a.rhs))(
          ts  = GlobalMemory() :: a.extensions)
    }
  }

  def toStr[L <: HList](expr: Expr[L]): String = {
    expr.toString
  }

  def toStrWithType[L <: HList](expr: Expr[L])
                               (implicit ev: Selector[L, Type]): String = {
    expr match {
      case v: Value[L] => v.get[Type].toString
      case a: Add[L]   => a.get[Type].toString +
        "(" + toStrWithType(a.lhs) + "," +
        toStrWithType(a.rhs) + ")"
    }
  }

  def toStrWithMemory[L <: HList](expr: Expr[L])
                                 (implicit ev: Selector[L, Memory]): String = {
    expr match {
      case v: Value[L] => v.get[Memory].toString
      case a: Add[L]   => a.get[Memory].toString +
        "(" + toStrWithMemory(a.lhs) + "," +
        toStrWithMemory(a.rhs) + ")"
    }
  }

  def toStrWithBoth[L <: HList](expr: Expr[L])
                               (implicit ev0: Selector[L, Type],
                                ev1: Selector[L, Memory]): String = {
    toStrWithType(expr) +
      "\n" +
      toStrWithMemory(expr)
  }

  def main(args: Array[String]): Unit = {
    val expr = Add(Value(1), Add(Value(2), Value(3)))

    val withTypes = addType(expr)

    val withBoth = addMemory(withTypes)

    println(toStr(expr))

    println(toStrWithType(withBoth))

    println(toStrWithMemory(withBoth))

    println(toStrWithBoth(withBoth))

    println("Done")
  }
}