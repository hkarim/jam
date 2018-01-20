package jam.slick

import shapeless._

trait AutoOptionalSlick { self: Slick =>

  @inline implicit def deriveGROHNil: GetResultOption[HNil] =
    GetResultOptionTypeClass.emptyProduct
  @inline implicit def deriveGROHCons1[H, T <: HList](implicit ch: Lazy[GetResultOption[H]], ct: Lazy[GetResultOption[T]]): GetResultOption[H :: T] =
    GetResultOptionTypeClass.product(ch.value, ct.value)
  @inline implicit def deriveGROHCons2[H, T <: HList](implicit ch: Lazy[GetResultOption[H]], ct: Lazy[GetResultOption[T]]): GetResultOption[Option[H] :: T] =
    GetResultOptionTypeClass.productWithHeadOption(ch.value, ct.value)
  @inline implicit def deriveGROInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[GetResultOption[G]]): GetResultOption[F] =
    GetResultOptionTypeClass.project(cg.value, gen.to, gen.from)


  @inline implicit def deriveReadOptionHNil: ReadOption[HNil] =
    ReadOptionTypeClass.emptyProduct
  @inline implicit def deriveReadOptionHCons1[H, T <: HList](implicit ch: Lazy[ReadOption[H]], ct: Lazy[ReadOption[T]]): ReadOption[H :: T] =
    ReadOptionTypeClass.product(ch.value, ct.value)
  @inline implicit def deriveReadOptionHCons2[H, T <: HList](implicit ch: Lazy[ReadOption[H]], ct: Lazy[ReadOption[T]]): ReadOption[Option[H] :: T] =
    ReadOptionTypeClass.productWithHeadOption(ch.value, ct.value)
  @inline implicit def deriveReadOptionInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[ReadOption[G]]): ReadOption[F] =
    ReadOptionTypeClass.project(cg.value, gen.to, gen.from)

}
