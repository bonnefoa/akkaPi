package akkapi.calculator

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import akkapi.supervisor.{RandomSupervisor, DoSupervise}
import akkapi.random.RandomSupplier
import org.scalatest.FlatSpec
import se.scalablesolutions.akka.util.Logging
import se.scalablesolutions.akka.actor.{Actor}
import akkapi.test.util.Time
import akkapi.actor.test.{ActorTester}


/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class PiActorTest extends FixtureFlatSpec with ShouldMatchers with Logging with ActorTester {
  type TypeResult = Double

  // 1. define type FixtureParam
  type FixtureParam = Actor

  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {

    val supervisor = new RandomSupervisor()
    log.debug("Starting Supervisor")
    supervisor.start

    val random = new RandomSupplier("randomSupplier")
    val piActor = new PiActor("piCalculator")

    initTestActor {
      testActor => {
        case PiResponse(piEstimate) =>
          log.debug("Received " + piEstimate)
          testActor.result = Some(piEstimate)
      }
    }

    supervisor.send(new DoSupervise(random))
    supervisor.send(new DoSupervise(piActor))
    // wait a bit to start all actors
    Thread.sleep(200)
    Time(test.name) {
      test((piActor))
    }
    supervisor.stop
  }

  "A PiActor" should "reply asynchronously" in {
    fixture =>
      val (piActor) = fixture

      piActor.!(new EstimatePiWithNumberOfPoints(100))(testActor)

      response {
        (result, messageFailure) =>
          messageFailure should be(None)
          log.debug(result + "")
          result.get should (be > (2.8D) and be < (3.5D))
      }
  }

  it should "support a great number of points" in {
    fixture =>
      val (piActor) = fixture
      piActor.!(new EstimatePiWithNumberOfPoints(100000))(testActor)
      response {
        (result, messageFailure) =>
          messageFailure should be(None)
          log.debug(result + "")
          result.get should (be > (2.8D) and be < (3.5D))
      }
  }

  it should "supply an estimate of pi with batch method" in {
    fixture =>
      val (piActor) = fixture
      piActor.!(new EstimatePiWithNumberOfPointsAndBatchSize(100000, 100))(testActor)
      response {
        (result, messageFailure) =>
          messageFailure should be(None)
          log.debug(result + "")
          result.get should (be > (2.8D) and be < (3.5D))
      }
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
    val seive = getSeive(0.01)
    piCalculator.estimatePi(seive) should (be > (3D) and be < (3.5D))
  }

  "A PiCalculatorStateful" should "find an estimate of pi given enough point" in {
    val piCalculatorStateful = new PiCalculatorStateful(1000)(null)
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
