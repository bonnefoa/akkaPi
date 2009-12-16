package akkapi.random

import org.scalatest.matchers.ShouldMatchers
import se.scalablesolutions.akka.config.ScalaConfig._
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.{Actor, SupervisorFactory}

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class RandomTest extends FixtureFlatSpec with ShouldMatchers {
  // 1. define type FixtureParam
  type FixtureParam = Actor
  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    val random = new RandomSupplier
    val factory = SupervisorFactory(
      SupervisorConfig(
        RestartStrategy(OneForOne, 3, 100, List(classOf[Exception])),
        Supervise(random, LifeCycle(Permanent)) :: Nil))
    val supervisor = factory.newInstance
    println("\n===> starting supervisor")
    supervisor.start
    test(random)
    println("\n===> stoping supervisor")
    supervisor.stop
  }

  "A RandomSupplier" should "supply random value when asked" in {
    random =>
      val reply = (random !! AskRandom())
      reply should not be (None)
      val value = reply.getOrElse(0D)
      value should (be < (1D) and be > (0D))
  }

}
