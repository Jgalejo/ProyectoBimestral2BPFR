import rx._

object Ejercicio1 extends App {

  val temperature = Var(20.0)


  val isHot = Rx { temperature() > 25 }


  val hotObserver = isHot.trigger {
    if (isHot.now) {
      println("Hace calor!")
    } else {
      println("No hace tanto calor.")
    }
  }


  println(s"¿Hace calor ahora? ${isHot.now}")


  temperature() = 30.0


  println(s"¿Hace calor ahora? ${isHot.now}")


  hotObserver.kill()


  temperature() = 18.0


}



