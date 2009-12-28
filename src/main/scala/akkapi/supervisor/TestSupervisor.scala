package akkapi.supervisor

import se.scalablesolutions.akka.actor.{SupervisorFactory, Actor}
import se.scalablesolutions.akka.config.ScalaConfig.{LifeCycle, Supervise, RestartStrategy, SupervisorConfig, Permanent, OneForOne}
import akkapi.random.RandomSupplier
import akkapi.pi.PiActor
import org.scalatest.{Suite, BeforeAndAfterAll}
import se.scalablesolutions.akka.util.Logging
import org.scalatest.matchers.ShouldMatchers

/**
 * Test supervisor. Provide a running supervisor before suite.
 * @author Anthonin Bonnefoy
 */

trait TestSupervisor extends Suite with ShouldMatchers with BeforeAndAfterAll with Logging {
  lazy val random = new RandomSupplier("randomSupplier")
  lazy val piActor = new PiActor("piCalculator")
  lazy val supervisor = factory.newInstance

  lazy val factory = SupervisorFactory(
    SupervisorConfig(
      RestartStrategy(OneForOne, 3, 10, List(classOf[Exception])),
      Supervise(random, LifeCycle(Permanent)) :: Supervise(piActor, LifeCycle(Permanent)) :: Nil)
    )

  /**
   * Start supervisor
   */
  override def beforeAll = {
    log.debug("Starting Supervisor")
    supervisor.start
    supervisor.isRunning should be(true)
    piActor.isRunning should be(true)
    random.isRunning should be(true)
  }

  /**
   * Stop supervisor
   */
  override def afterAll = {
    log.debug("Stoping Supervisor")
    supervisor.stop
  }

}