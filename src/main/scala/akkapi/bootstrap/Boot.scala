package akkapi.bootstrap

import se.scalablesolutions.akka.actor.SupervisorFactory
import se.scalablesolutions.akka.config.ScalaConfig._
import akkapi.random.RandomSupplier

object Boot {
  val random = new RandomSupplier("aRandom")
  val factory = SupervisorFactory(
    SupervisorConfig(
      RestartStrategy(OneForOne, 3, 100, List(classOf[Exception])),
      Supervise(
        random,
        LifeCycle(Permanent))
              :: Nil))
}