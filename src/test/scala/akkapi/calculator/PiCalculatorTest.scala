package akkapi.calculator

import scala.actors._
import se.scalablesolutions.akka.actor.Actor._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import akkapi.supervisor.{RandomSupervisor, DoSupervise}
import akkapi.random.RandomSupplier
import org.scalatest.FlatSpec
import se.scalablesolutions.akka.util.Logging
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}

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
    println("Starting Supervisor")
    supervisor.start
    supervisor.!(new DoSupervise(random))(supervisor)
    supervisor.!(new DoSupervise(piActor))(supervisor)
    test(piActor)
    supervisor.stop
  }



  "A PiActor" should "reply asynchronously" in {
    piActor: Actor =>
      val testActor = new TestActor()
      testActor.start
      val fut = future {
        piActor.!(new EstimatePiWithNumberOfPoints(10))(testActor)
        while (!testActor.received) {
          Thread.sleep(100)
        }
      }
       await(30000, fut: _*)
  }

  //  it should "support a great number of points" in {
  //
  //    piActor: Actor =>
  //      val testActor = new TestActor()
  //      testActor.start
  //      (1 to 50).foreach {
  //        i =>
  //          Time("piactor simple " + i) {
  //            println(testActor.isRunning)
  //            piActor.!(new EstimatePiWithNumberOfPoints(10))(testActor)
  //            testActor.hasFailed should not be (true)
  //            testActor.message should be("")
  //            //            response.get should (be > (2.8D) and be < (3.5D))
  //          }
  //      }
  //  }

  //
  //  it should "supply a estimate of pi when asked" in {
  //    piActor =>
  //      Time("ask pi normally") {
  //        (1 to 10).foreach {
  //          i =>
  //            val response: Option[Double] = piActor !! EstimatePiWithNumberOfPoints(1000)
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
  //            val response: Option[Double] = piActor !! EstimatePiWithNumberOfPointsAndBatchSize(1000, 100)
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


class TestActor extends Actor {
  var hasFailed = false
  var message = ""
  var result: Option[Double] = None
  var received = false

  def receive: PartialFunction[Any, Unit] = {
    case result: Option[Double] =>
      received = true
      this.result = result
    case other =>
      received = true
      hasFailed = true
      message = "response uknown : " + other
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
