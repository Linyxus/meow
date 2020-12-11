package meow

import meow.syntax._

object Meow extends CurryFunctions
  with FunctorInstances with ApplicativeInstances with MonadInstances
  with FunctorOpInstances with ApplicativeOpInstances with MonadOpInstances
  with CanFMapOpInstances with CanFMap2OpInstances
  with ApplicativePureOpInstances
  with MonadReturnOpInstances

