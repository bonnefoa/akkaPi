package akkapi.recorder

import se.scalablesolutions.akka.actor.Actor

/**
 * Ask for pi estimation and record results in a persistent storage.
 *
 * @author Anthonin Bonnefoy
 */

case class ResultRecorderResponse(piStat: PiStatistique)

class ResultRecorder extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case other =>
      log.error("Unknown response : " + other)
  }
}

case class PiStatistique()