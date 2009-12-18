package akkapi.calculator


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.Actor
import akkapi.supervisor.{RandomSupervisor, DoSupervise}
import akkapi.random.RandomSupplier
import org.scalatest.FlatSpec
import se.scalablesolutions.akka.util.Logging

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
    while (!piActor.isRunning) {
      Thread.sleep(100)
    }
    test(piActor)
    supervisor.stop
  }

  "A PiActor" should "support a great number of points" in {
    piActor =>
      (1 to 50).foreach {
        i =>
          Time("piactor simple " + i) {
            val response: Option[Double] = piActor !! AskPiWithNumberOfPoints(500)
            response should not be (None)
            response.get should (be > (2.8D) and be < (3.5D))
          }
      }
  }

  //
  //  it should "supply a estimate of pi when asked" in {
  //    piActor =>
  //      Time("ask pi normally") {
  //        (1 to 10).foreach {
  //          i =>
  //            val response: Option[Double] = piActor !! AskPiWithNumberOfPoints(1000)
  //            response should not be (None)
  //            response.get should (be > (2.8D) and be < (3.5D))
  //        }
  //      }
  //  }
  //  it should "supply an estimate of pi with batch method" in {
  //    piActor =>
  //      Time("ask pi normally with batch") {
  //        (1 to 10).foreach {
  //          i =>
  //            val response: Option[Double] = piActor !! AskPiWithNumberOfPointsAndBatchSize(1000, 100)
  //            response should not be (None)
  //            response.get should (be > (2.8D) and be < (3.5D))
  //        }
  //      }
  //  }
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
    val seive = getSeive(0.01)
    piCalculator.estimatePi(seive) should (be > (3D) and be < (3.5D))
  }

  "A PiCalculatorStateful" should "find an estimate of pi given enough point" in {
    val piCalculatorStateful = new PiCalculatorStateful
    val seive = getSeive(0.01)
    seive.foreach(tuple => {
      piCalculatorStateful.addPoint(tuple._1, tuple._2)
    }
      )
    piCalculator.estimatePi(seive) should be(piCalculatorStateful.processPi)
  }


  def getSeive(step: Double): List[(Double, Double)] = {
    def getSeive(x: Double, y: Double, list: List[(Double, Double)], step: Double): List[(Double, Double)] = {
      if (x + step > 1) {
        if (y + step > 1) (x, y) :: list
        else getSeive(0, y + step, (x, y) :: list, step)
      } else
        getSeive(x + step, y, (x, y) :: list, step)
    }
    getSeive(0, 0, Nil, step)
  }
}



object Time extends Logging {
  def apply[T](name: String)(block: => T) {
    val start = System.currentTimeMillis
    try {
      block
    } finally {
      val diff = System.currentTimeMillis - start
      log.debug("Block \"" + name + "\" completed, time taken: " + diff + " ms (" + diff / 1000.0 + " s)")
    }
  }
}
