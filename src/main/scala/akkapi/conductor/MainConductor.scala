package akkapi.conductor

import akkapi.random.RandomSupplier
import akkapi.supervisor.{RandomSupervisor}
import akkapi.calculator.{PiActor}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class MainConductor extends Application {
  val piActor = new PiActor("actor")
  val randomSupplier = new RandomSupplier("supplier")
  val randomSupervisor = new RandomSupervisor

  randomSupervisor.start
  //  randomSupervisor ! DoSupervise(piActor)
  //  randomSupervisor ! DoSupervise(randomSupplier)

  //  piActor ! EstimatePiWithNumberOfPoints(1000)


}