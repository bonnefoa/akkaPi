package akkapi.actor.test

import se.scalablesolutions.akka.actor.{Actor}
import se.scalablesolutions.akka.util.Logging

/**
 * Actor for testing purpose
 *
 * @author Anthonin Bonnefoy
 */

trait ActorTester extends Logging {
  type TypeResult

  var testActor: TestActor[TypeResult] = new TestActor

  def initTestActor(block: TestActor[TypeResult] => PartialFunction[Any, Unit]) {
    testActor = new TestActor
    testActor.start
    testActor.receiveFunction = block(testActor)
  }


  def response(block: (Option[TypeResult], Option[String]) => Unit) {
    while (!testActor.received) {
      Thread.sleep(100)
    }
    block(testActor.result, testActor.messageFailure)
  }
}

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
