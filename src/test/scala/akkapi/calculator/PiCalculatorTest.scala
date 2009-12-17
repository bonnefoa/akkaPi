package akkapi.calculator

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.Actor
import akkapi.supervisor.{RandomSupervisor, DoSupervise}
import akkapi.random.RandomSupplier

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class PiCalculatorTest extends FixtureFlatSpec with ShouldMatchers {

  // 1. define type FixtureParam
  type FixtureParam = Actor
  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    val supervisor = new RandomSupervisor()
    val random = new RandomSupplier("randomSupplier")
    val piActor = new PiActor("piCalculator")
    supervisor.start
    supervisor.!(new DoSupervise(random))(supervisor)
    supervisor.!(new DoSupervise(piActor))(supervisor)

    test(piActor)
    supervisor.stop
  }

  "A PiActor" should "supply a estimate of pi when asked" in {
    piActor =>

  }
}