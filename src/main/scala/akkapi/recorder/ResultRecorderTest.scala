package akkapi.recorder

import akkapi.supervisor.TestSupervisor
import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.matchers.ShouldMatchers
import se.scalablesolutions.akka.util.Logging
import akkapi.actor.test.util.ActorTester
import akkapi.test.util.Time
import org.scalatest.FlatSpec
import java.lang.Thread

/**
 * Test of the result recorder.
 * @author Anthonin Bonnefoy
 */

class ResultRecorderTest extends FixtureFlatSpec with ShouldMatchers with Logging with ActorTester with TestSupervisor {

  // Define type of expected result
  type TypeResult = PiStatistique

  // 1. define type FixtureParam
  type FixtureParam = ResultRecorder

  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    initActorTester {
      testActor => {
        case piStatistiqueResponse: RequestRecorderPiStatistiqueResponse =>
          log.debug("Received " + piStatistiqueResponse)
          testActor.result = Some(piStatistiqueResponse.piStatistique)
      }
    }
    Time(test.name) {
      test(resultRecorder)
    }
    stopActorTester
  }

  "A resultRecorder" should "get pi estimation to fill piStatistique" in {
    fixture =>
      val (recorder: ResultRecorder) = fixture
      (1 to 10).foreach(_ => recorder.send(new RequestRecorderPiEstimateRequests(1000)))
      Thread.sleep(1000)
      recorder.!(new RequestRecorderAskPiStatistique())(testActor)
      response {
        (result, messageFailure) =>
          messageFailure should be(None)
          log.debug(result + "")
          result should not be (None)
          val piStat: TypeResult = result.get
          piStat.totalweight should be(1000 * 10)
          piStat.mean should (be > (3.1D) and be < (3.2D))
      }
  }
}

class PiStatistiqueTest extends FlatSpec with ShouldMatchers with Logging {
  val piStatistique = PiStatistique(
    List(RequestRecorderPiResult(3.6, 50), RequestRecorderPiResult(2.6, 10), RequestRecorderPiResult(3.1, 40))
    )

  "A PiStatistique " should "return correct weight when asked" in {
    piStatistique.totalweight should be(100D)
  }

  it should "return correct mean" in {
    piStatistique.mean should be(3.3)
  }
}