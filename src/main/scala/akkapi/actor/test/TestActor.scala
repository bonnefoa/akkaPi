package akkapi.actor.test

import se.scalablesolutions.akka.actor.Actor
import akkapi.calculator.PiResponse

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

class TestActor extends Actor {
  var messageFailure: Option[String] = None
  var result: Option[Double] = None
  var received = false

  def receive: PartialFunction[Any, Unit] = {
    case PiResponse(piEstimate) =>
      received = true
      this.result = Some(piEstimate)
    case other =>
      received = true
      messageFailure = Some("response uknown : " + other)
  }

}
