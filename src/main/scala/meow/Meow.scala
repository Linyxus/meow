package meow

import meow.syntax._

object Meow extends CurryFunctions
  with MonadInstances with AlternativeInstances
  with FunctorOpInstances with ApplicativeOpInstances with MonadOpInstances with AlternativeOpInstances
  with CanFMapOpInstances with CanFMap2OpInstances
  with ApplicativePureOpInstances
  with MonadReturnOpInstances
  with MonadExtraOpInstances
  with AlternativeFunctions
  with Isomorphisms with IsoMonadOpInstances
  with SemigroupOpInstances
  with MonoidInstances

