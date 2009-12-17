package akkapi.base

import org.junit.{Before, After, Test}
import se.scalablesolutions.akka.actor.Supervisor

class SimulationTest {
  implicit var supervisor: Supervisor = Boot.factory.newInstance
  var worker: Worker = Boot.worker

  @Before
  def setUp: Unit = {
    supervisor = Boot.factory.newInstance

    worker = Boot.worker
    println("\n===> starting supervisor")
    supervisor.start
  }

  @After
  def tearDown = {
    println("\n===> stoping supervisor")
    supervisor.stop
  }

  @Test
  def testDoWork = {
    Thread.sleep(500)
    println("\n===> start working")

    worker ! DoWork("Some work")
    Thread.sleep(500)
  }

  @Test
  def testDoDie = {
    worker ! DoDie
    Thread.sleep(1000)
  }

  @Test
  def testDoOtherWork = {
    worker ! DoWork("Some more work")
    println("\n===> finished")
  }
}
