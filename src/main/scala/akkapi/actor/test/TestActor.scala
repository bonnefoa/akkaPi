package akkapi.actor.test

import se.scalablesolutions.akka.actor.{Actor}
import se.scalablesolutions.akka.util.Logging

/**
 * Actor for testing purpose. It provides a testActor for receiving response from actors. You have to define the type of response
 * you expect through TypeResult and configure testActor through initActorTester.
 *
 * @author Anthonin Bonnefoy
 */

trait ActorTester extends Logging {
  type TypeResult

  var testActor: TestActor[TypeResult] = new TestActor

  def initActorTester(block: TestActor[TypeResult] => PartialFunction[Any, Unit]) {
    testActor = new TestActor
    testActor.start
    testActor.receiveFunction = block(testActor)
  }

  def waitResponse = {
    while (!testActor.received) {
      Thread.sleep(100)
    }
  }

  def response(block: (Option[TypeResult]) => Unit) {
    waitResponse
    block(testActor.result)
  }

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
 * You can configure the received function to store a result which will be accessible through response ActorTester.
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

  def cleanUp {
    result = None
    result = None
    messageFailure = None
  }

  override def toString() = "Test actor" + id
}
