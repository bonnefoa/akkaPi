package akkapi.random

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.{Actor, SupervisorFactory}
import org.scalatest.FlatSpec
import akkapi.supervisor.{DoSupervise, RandomSupervisor}

/**
 * Test random features
 *
 * @author Anthonin Bonnefoy
 */

class SupervisorTest extends FlatSpec with ShouldMatchers {
  "A supervisor Test" should "start stop and actor should still work" in {
    var random = new RandomSupplier("randomSupplier")

    val supervisor = new RandomSupervisor()
    supervisor.start
    supervisor.!(new DoSupervise(random))(supervisor)
    random !! AskRandom() should not be (None)
    supervisor.stop

    val second = new RandomSupplier("randomSupplier2")
    second.start
    second !! AskRandom() should not be (None)
    second.stop
  }
}

class RandomTest extends FixtureFlatSpec with CheckRandomReply {

  // 1. define type FixtureParam
  type FixtureParam = Actor
  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    val supervisor = new RandomSupervisor()
    val random = new RandomSupplier("randomSupplier")
    supervisor.start
    println("\n===> starting supervisor")
    supervisor.!(new DoSupervise(random))(supervisor)
    test(random)
    println("\n===> stoping supervisor")
    supervisor.stop
  }



  "A RandomSupplier" should "supply random value when asked" in {
    random =>
      (1 to 20).foreach {
        i =>
          checkReply(random !! AskRandom(), 0D, 1D)
      }
  }

  it should "supply random list value between min and max given" in {
    random =>
      (1 to 100).foreach {
        i =>
          checkReply(random !! AskRandomBetween(i * 1D, i * 2D), i * 1D, i * 2D)
      }
  }
  it should "supply random list value when asked" in {
    random =>
      (1 to 100).foreach {
        i =>
          val min = i * 1D
          val max = i * 2D
          val reply: Option[List[Double]] = (random !! AskRandomListBetween(i, min, max))
          checkReply(reply, i, min, max)
      }
  }

}

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