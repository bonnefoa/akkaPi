package akkapi.recorder

import akkapi.supervisor.TestSupervisor
import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.matchers.ShouldMatchers
import se.scalablesolutions.akka.util.Logging
import akkapi.actor.test.util.ActorTester
import se.scalablesolutions.akka.actor.Actor
import akkapi.test.util.Time
import org.scalatest.FlatSpec

/**
 * Test of the result recorder.
 * @author Anthonin Bonnefoy
 */

class ResultRecorderTest extends FixtureFlatSpec with ShouldMatchers with Logging with ActorTester with TestSupervisor {

  // Define type of expected result
  type TypeResult = PiStatistique

  // 1. define type FixtureParam
  type FixtureParam = Actor

  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    initActorTester {
      testActor => {
        case ResultRecorderMessage(resultResponse) =>
          log.debug("Received " + resultResponse)
          testActor.result = Some(resultResponse)
      }
    }
    Time(test.name) {
      test(resultRecorder)
    }
    stopActorTester
  }

  //  "A resultRecorder" should "reply asynchronously" in {
  //    fixture =>
  //      val (piActor) = fixture
  //      //      piActor.!(new EstimatePiWithNumberOfPoints(100))(testActor)
  //      response {
  //        (result, messageFailure) =>
  //        //          messageFailure should be(None)
  //        //          log.debug(result + "")
  //        //          result.get should (be > (2.8D) and be < (3.5D))
  //      }
  //  }
}

class PiStatistiqueTest extends FlatSpec with ShouldMatchers with Logging {
  val piStatistique = PiStatistique(
    List(PiResult(3.6, 50), PiResult(2.6, 10), PiResult(3.1, 40))
    )

  "A PiStatistique " should "return correct weight when asked" in {
    piStatistique.totalweight should be(100D)
  }

  it should "return correct mean" in {
    piStatistique.mean should be(3.3)
  }
}