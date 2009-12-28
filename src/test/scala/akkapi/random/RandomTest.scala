package akkapi.random

import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.FlatSpec
import akkapi.supervisor.{DoSupervise, RandomSupervisor}
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import akkapi.actor.test.{TestActor, ActorTester}

abstract class BaseRandomActorTest extends FixtureFlatSpec with CheckRandomReply with Logging with ActorTester {
  type OptionResult = Option[TypeResult]

  type FixtureParam = (Actor, TestActor[TypeResult])

  def withFixture(test: OneArgTest) {
    val supervisor = new RandomSupervisor()
    val random = new RandomSupplier("randomSupplier")
    supervisor.start
    log.debug("===> starting supervisor")
    supervisor.send(new DoSupervise(random))

    initActorTester {
      testActor => {
        case result: OptionResult =>
          testActor.result = result
      }
    }
    // wait a bit to start all actors
    Thread.sleep(200)

    test((random, testActor))
    log.debug("===> stoping supervisor")
    stopActorTester
    supervisor.stop
  }
}

/**
 * Test randomSupplier actor features
 *
 * @author Anthonin Bonnefoy
 */
class RandomTestSingleElement extends BaseRandomActorTest {
  type TypeResult = Double

  "A RandomSupplier" should "supply random value when asked" in {
    fixture =>
      val (random, testActor) = fixture
      random.!(new AskRandomAsync)(testActor)
      response {
        result =>
          checkReply(result, 0D, 1D)
      }
  }

  it should "supply random list value between min and max given" in {
    fixture =>
      val (random, testActor) = fixture
      (1 to 100).foreach {
        i =>
          random.!(AskRandomBetweenAsync(i * 1D, i * 2D))(testActor)
          response(result =>
            checkReply(result, i * 1D, i * 2D)
            )
      }
  }
}

class RandomTestListElement extends BaseRandomActorTest {
  override type TypeResult = List[Double]

  "A random supplier" should "supply random list value when asked" in {
    fixture =>
      val (random, testActor) = fixture
      (1 to 100).foreach {
        i =>
          val min = i * 1D
          val max = i * 2D
          random.!(new AskRandomListBetweenAsync(i, min, max))(testActor)
          response {
            result =>
              checkReply(result, i, min, max)
          }
      }
  }

}

/**
 * Test the random generator
 */
class RandomGeneratorTest extends FlatSpec with CheckRandomReply {
  "A RandomGenerator" should "generate random value between 0 and 1 " in {
    (1 to 100).foreach {
      i =>
        val value = RandomGenerator.nextDouble
        value should (be < (1D) and be > (0D))
    }
  }
  it should "generate random value between given min and max" in {
    (0 to 20).foreach {
      i =>
        (i to 40).foreach {
          j =>
            val value = RandomGenerator.nextDouble(i, j)
            value should (be <= (j.asInstanceOf[Double]) and be >= (i.asInstanceOf[Double]))
        }
    }
  }

  it should "generate random a list of value at the given size" in {
    (0 to 20).foreach {
      i => val listRandom = RandomGenerator.listDouble(i)
      listRandom should have size (i)
      listRandom.foreach(
        value => value should (be <= (1D) and be >= (0D))
        )
    }
  }

  it should "generate random a list of value at the given size with min and max defines" in {
    (0 to 20).foreach {
      i => val listRandom = RandomGenerator.listDouble(i, 0, i)
      listRandom should have size (i)
      listRandom.foreach(
        value => value should (be <= (i.asInstanceOf[Double]) and be >= (0D))
        )
    }
  }

}