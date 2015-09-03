import shapeless.{=:!=, ::, HNil, HList}
import shapeless.ops.hlist.Selector

import scala.annotation.implicitNotFound

sealed abstract class Expr[L <: HList](val extensions: L) {
  def get[T](implicit ev: Selector[L, T]): T = extensions.select[T]
}

case class Value[T <: HList](value: Double)
                            (implicit ts: T) extends Expr[T](ts)

case class Add[T <: HList](lhs: Expr[T], rhs: Expr[T])
                          (implicit ts: T) extends Expr[T](ts)



object Expr {
  implicit val hnil : HNil = HNil

  trait Contains[L <: HList, T]

  object Contains {
    implicit def isHead[T, Tail <: HList] =
      new Contains[T :: Tail, T] {}

    implicit def isNotHeadButIsInTail[T, Head, Tail <: HList]
      (implicit ev0: T =:!= Head,
                ev1: Contains[Tail, T]) =
        new Contains[Head :: Tail, T] {}
  }

  trait ContainsNot[L <: HList, T]

  object ContainsNot {
    implicit def TisNotHNil[T]
      (implicit ev: T =:!= HNil) =
        new ContainsNot[HNil, T] {}

    implicit def isNotHeadAndNotInTail[T, Head, Tail <: HList]
      (implicit ev0: T =:!= Head,
                ev1: ContainsNot[Tail, T]) =
        new ContainsNot[Head :: Tail, T] {}
  }

  @implicitNotFound("Implicit not found: Expr.ContainsOnce[${L}, ${T}]. " +
                    "You requested that there is exactly one element of type " +
                    "${T} in the HList ${L}, but this is not true.")
  trait ContainsOnce[L <: HList, T]

  object ContainsOnce {
    def apply[L <: HList, T]()(implicit ev: ContainsOnce[L, T]): ContainsOnce[L, T] = ev

    implicit def isHeadAndNotInTail[T, Tail <: HList]
      (implicit ev: Tail ContainsNot T) =
        new ContainsOnce[T :: Tail, T] {}

    implicit def isNotHeadButOnceInTail[T, Head, Tail <: HList]
      (implicit ev0: T =:!= Head, ev1: ContainsOnce[Tail, T]) =
        new ContainsOnce[Head :: Tail, T] {}
  }
}
