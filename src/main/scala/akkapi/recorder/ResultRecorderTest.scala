package akkapi.recorder

import akkapi.supervisor.TestSupervisor
import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.matchers.ShouldMatchers
import se.scalablesolutions.akka.util.Logging
import akkapi.actor.test.util.ActorTester
import se.scalablesolutions.akka.actor.Actor
import akkapi.test.util.Time

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
        case ResultRecorderResponse(resultResponse) =>
          log.debug("Received " + resultResponse)
          testActor.result = Some(resultResponse)
      }
    }
    Time(test.name) {
      test(piActor)
    }
    stopActorTester
  }

  "A resultRecorder" should "reply asynchronously" in {
    fixture =>
      val (piActor) = fixture
      //      piActor.!(new EstimatePiWithNumberOfPoints(100))(testActor)
      response {
        (result, messageFailure) =>
        //          messageFailure should be(None)
        //          log.debug(result + "")
        //          result.get should (be > (2.8D) and be < (3.5D))
      }
  }


}
