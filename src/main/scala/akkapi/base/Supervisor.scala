package akkapi.base

import se.scalablesolutions.akka.actor.SupervisorFactory
import se.scalablesolutions.akka.config.ScalaConfig.{LifeCycle, SupervisorConfig, RestartStrategy, Supervise, OneForOne, Permanent}

object Boot {
  val worker = new Worker("worker-1")
  val factory = SupervisorFactory(
    SupervisorConfig(
      RestartStrategy(OneForOne, 3, 100, List(classOf[Exception])),
      Supervise(
        worker,
        LifeCycle(Permanent))
              :: Nil))
}
