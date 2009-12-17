package akkapi.random

import org.scalatest.matchers.ShouldMatchers
import se.scalablesolutions.akka.config.ScalaConfig._
import org.scalatest.fixture.FixtureFlatSpec
import se.scalablesolutions.akka.actor.{Actor, SupervisorFactory}
import org.scalatest.FlatSpec

/**
 * Test random features
 *
 * @author Anthonin Bonnefoy
 */

class RandomTest extends FixtureFlatSpec with ShouldMatchers {

  // 1. define type FixtureParam
  type FixtureParam = Actor
  // 2. define the withFixture method
  def withFixture(test: OneArgTest) {
    val random = new RandomSupplier
    val factory = SupervisorFactory(
      SupervisorConfig(
        RestartStrategy(OneForOne, 3, 100, List(classOf[Exception])),
        Supervise(random, LifeCycle(Temporary)) :: Nil))
    val supervisor = factory.newInstance

    println("\n===> starting supervisor")
    supervisor.start
    test(random)
    println("\n===> stoping supervisor")
    supervisor.stop
  }

  "A RandomSupplier" should "supply random value when asked" in {
    random =>
      (1 to 100).foreach {
        i =>
          val reply: Option[Double] = (random !! AskRandom())
          reply should not be (None)
          val value = reply.get
          value should (be < (1D) and be > (0D))
      }
  }

  it should "supply random list value between min and max given" in {
    random =>
      (1 to 100).foreach {
        i =>
          val reply: Option[Double] = (random !! AskRandomBetween(i * 1D, i * 2D))
          reply should not be (None)
          val value = reply.get
          value should (be < (i * 1D) and be > (i * 2D))
      }
  }
  it should "supply random list value when asked" in {
    random =>
      (1 to 100).foreach {
        i =>
          val reply: Option[List[Double]] = (random !! AskRandomListBetween(10, i * 1D, i * 2D))
          reply should not be (None)
          val list = reply.get
          list should have size (10)
          list.foreach(value =>
            value should (be < (i * 1D) and be > (i * 2D))
            )

      }
  }

}

class RandomGeneratorTest extends FlatSpec with ShouldMatchers {
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