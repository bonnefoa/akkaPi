package akkapi.pi

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import akkapi.random.RandomSupplier
import se.scalablesolutions.akka.util.Logging
import akkapi.test.util.Time
import akkapi.actor.test.{ActorTester}
import se.scalablesolutions.akka.actor.{SupervisorFactory, Actor}
import se.scalablesolutions.akka.config.ScalaConfig.{LifeCycle, Supervise, RestartStrategy, SupervisorConfig, Permanent, OneForOne}
import org.scalatest.BeforeAndAfterAll


/**
 * PiActor will supply pi estimation through differents message 
 *
 * @author Anthonin Bonnefoy
 */

class PiActorTest extends FixtureFlatSpec with ShouldMatchers with Logging with ActorTester with BeforeAndAfterAll {

  // Define type of expected result
  type TypeResult = Double

  // 1. define type FixtureParam
  type FixtureParam = Actor

  val random = new RandomSupplier("randomSupplier")
  val piActor = new PiActor("piCalculator")
  val supervisor = factory.newInstance

  val factory = SupervisorFactory(
    SupervisorConfig(
      RestartStrategy(OneForOne, 3, 10, List(classOf[Exception])),
      Supervise(random, LifeCycle(Permanent)) :: Supervise(piActor, LifeCycle(Permanent)) :: Nil)
    )

  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    initActorTester {
      testActor => {
        case PiResponse(piEstimate) =>
          log.debug("Received " + piEstimate)
          testActor.result = Some(piEstimate)
      }
    }
    Time(test.name) {
      test(piActor)
    }
    stopActorTester
  }

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
