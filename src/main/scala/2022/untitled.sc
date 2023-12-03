val tipar = "mananc(.*)".r

val test = "mananc dia"
test match
  case tipar(ceva) => ceva
  case _ => "nu merge tiparul"

