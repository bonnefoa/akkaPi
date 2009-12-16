package akkapi

import org.junit.{Before, After, Test}

class SimulationTest {
  implicit val  supervisor = Boot.factory.newInstance
  var worker: Worker = Boot.worker

  @Before
  def setUp:Unit = {
    println("\n===> starting supervisor")
    supervisor.start
  }

  @After
  def tearDown = {
    println("\n===> stoping supervisor")
    supervisor.stop
  }

  @Test
  def testSuperviseWorker = {
    Thread.sleep(500)
    println("\n===> start working")

    worker ! DoWork("Some work")
    Thread.sleep(500)

    worker ! DoDie
    Thread.sleep(1000)

    worker ! DoWork("Some more work")

    println("\n===> finished")

  }
}
