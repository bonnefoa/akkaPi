package akkapi.recorder

import se.scalablesolutions.akka.actor.Actor

/**
 * Ask for pi estimation and record results in a persistent storage.
 *
 * @author Anthonin Bonnefoy
 */

class ResultRecorder extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case other =>
      log.error("Unknown response : " + other)
  }
}