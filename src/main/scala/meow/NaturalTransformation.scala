package meow

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}

trait NaturalTransformationTypeOp {
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
}

object NaturalTransformation extends NaturalTransformationTypeOp
