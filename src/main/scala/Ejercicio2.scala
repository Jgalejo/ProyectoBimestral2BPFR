import rx.lang.scala.{Observable, Subject, Subscription}
import scala.concurrent.duration._

object Ejercicio2 extends App {
  // Crear un observable directamente con intervalo y toma solo 5 elementos
  val observable: Observable[Int] = Observable.interval(0.second, 1.second).take(5).map(_.toInt + 1)

  
  val subject: Subject[Int] = Subject[Int]()


  val observer: Subscription = subject.subscribe(
    onNext = elem => println(s"Recibido: $elem"),
    onError = error => println(s"Error: ${error.getMessage}"),
    onCompleted = () => println("Completado")
  )


  val transformed: Observable[Int] = observable
    .map(_ * 2)
    .filter(_ % 2 == 0)
    .delay(1.second)


  val subscription: Subscription = transformed.subscribe(subject)


  Thread.sleep(6000)


  subscription.unsubscribe()
}