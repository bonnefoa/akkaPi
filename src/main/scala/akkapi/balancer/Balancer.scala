package akkapi.balancer

import akkapi.pi.PiActor
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.random.RandomSupplier
import se.scalablesolutions.akka.util.Logging

/**
 * Created by IntelliJ IDEA.
 * @author Anthonin Bonnefoy
 */

object Balancer extends Logging{
  lazy val actorManager = ActorRegistry.actorsFor(classOf[ActorManager]).head

    def getFreePiActor: Option[Actor] = {
    val listPiActor = ActorRegistry.actorsFor(classOf[PiActor]).asInstanceOf[List[PiActor]]
    val filteredList = listPiActor.filter((actor: PiActor) => !actor.busy)
    if (filteredList == Nil) {
      actorManager ! ActorManagerCreatePiActor
      Thread.sleep(100)
      getFreePiActor
    }
    else Some(filteredList.head.asInstanceOf[Actor])
  }

  def getRandomSupplier: RandomSupplier = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier])
    log.debug("Got randomSupplierList " + listRandomSupplier)
    listRandomSupplier.head.asInstanceOf[RandomSupplier]
  }
}