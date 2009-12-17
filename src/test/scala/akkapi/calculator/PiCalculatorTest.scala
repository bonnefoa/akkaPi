package akkapi.calculator

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.Actor
import akkapi.supervisor.{RandomSupervisor, DoSupervise}
import akkapi.random.RandomSupplier
import org.scalatest.FlatSpec

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class PiActorTest extends FixtureFlatSpec with ShouldMatchers {

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

class PiCalculatorTest extends FlatSpec with ShouldMatchers {
  val piCalculator = new PiCalculator
  "A PiCalculator" should "find the number of point inside a unit circle" in {
    piCalculator.getNumberPointsInCircle(List((0, 0))) should be(1)
    piCalculator.getNumberPointsInCircle(List((1, 1))) should be(0)
    piCalculator.getNumberPointsInCircle(List((0, 0), (1, 1))) should be(1)
    piCalculator.getNumberPointsInCircle(List((0.5, 0), (0.5, 0.5), (0, 0.5), (0, 1))) should be(3)
  }

  it should "find an estimate of pi" in {
    val precision = 100
    val crible = (1 to precision).flatMap(j => {
      val y = j / precision.asInstanceOf[Double]
      (1 to precision).flatMap(i => {
        val x = i / precision.asInstanceOf[Double]
        List((x, y))
      })
    }).toList
    piCalculator.estimatePi(crible) should (be > (3D) and be < (4D));
  }
}                     

