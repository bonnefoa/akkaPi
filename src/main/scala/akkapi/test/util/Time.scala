package akkapi.test.util

import se.scalablesolutions.akka.util.Logging

/**
 * Time object 
 *
 * @author Anthonin Bonnefoy
 */
object Time extends Logging {
  def apply[T](name: String)(block: => T) {
    val start = System.currentTimeMillis
    try {
      block
    } finally {
      val diff = System.currentTimeMillis - start
      log.debug("Block \"" + name + "\" completed, time taken: " + diff + " ms (" + diff / 1000.0 + " s)")
    }
  }
}
