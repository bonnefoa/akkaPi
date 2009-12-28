package akkapi.actor.test

import se.scalablesolutions.akka.actor.{Actor}
import se.scalablesolutions.akka.util.Logging

/**
 * Actor for testing purpose. It provides a testActor for receiving response from actors. You have to define the type of response
 * you expect through TypeResult and configure testActor with initActorTester.
 *
 * @author Anthonin Bonnefoy
 */

trait ActorTester extends Logging {
  type TypeResult

  var testActor: TestActor[TypeResult] = new TestActor

  /**
   * Create the actor, start it and define the receive partial funciont
   */
  def initActorTester(block: TestActor[TypeResult] => PartialFunction[Any, Unit]) {
    testActor = new TestActor
    testActor.start
    testActor.receiveFunction = block(testActor)
  }

  /**
   * Wait for a response
   *
   */
  //TODO use a timeout
  def waitResponse = {
    while (!testActor.received) {
      Thread.sleep(100)
    }
  }

  /**
   * Response block which wait for the response to arrive and call the block with the result
   */
  def response(block: (Option[TypeResult]) => Unit) {
    waitResponse
    block(testActor.result)
  }

  /**
   * Response block which wait for the response to arrive and call the block with the result and
   * the eventual error message
   */
  def response(block: (Option[TypeResult], Option[String]) => Unit) {
    waitResponse
    block(testActor.result, testActor.messageFailure)
  }

  def stopActorTester() {
    testActor.stop
  }
}

/**
 * Actor aimed to receive and store a response message.
 * You can configure the received function to store a result which will be accessible through response block of ActorTester trait.
 */
class TestActor[T] extends Actor {
  var messageFailure: Option[String] = None
  var result: Option[T] = None
  var receiveFunction: PartialFunction[Any, Unit] = failureAndThen

  def received = {
    result.isDefined || messageFailure.isDefined
  }

  def receive: PartialFunction[Any, Unit] = {
    receiveFunction.orElse(failureAndThen)
  }

  val failureAndThen: PartialFunction[Any, Unit] = {
    case other =>
      messageFailure = Some("response unknown : " + other)
  }

  override def toString() = "Test actor" + id
}
