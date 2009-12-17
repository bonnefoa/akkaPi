package akkapi.random

import org.scalatest.matchers.ShouldMatchers

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

trait CheckRandomReply extends ShouldMatchers {
  def checkReply(reply: Option[Double], min: Double, max: Double) {
    reply should not be (None)
    val value = reply.get
    value should (be >= (min) and be <= (max))
  }

  def checkReply(reply: Option[List[Double]], size: Int, min: Double, max: Double) {
    reply should not be (None)
    val list = reply.get
    list should have size (size)
    list.foreach(value =>
      value should (be >= (min) and be <= (max))
      )
  }
}