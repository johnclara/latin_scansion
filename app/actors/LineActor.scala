package actors

import akka.actor.{Props, ActorRef, Actor}
import scala.collection.JavaConverters._
import scala.collection.immutable.{HashSet}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.fasterxml.jackson.databind.node.ObjectNode;
import play.libs.Akka
import play.libs.Json
import play.api.libs.json.JsArray
import com.fasterxml.jackson.databind.JsonNode;
import scansion._

class LineActor extends Actor {
  protected[this] var watchers: HashSet[ActorRef] = HashSet.empty[ActorRef]

  var lines: List[String] = {
    List()
  }
  
  def receive = {
    case WatchLine(x) =>

      val parsed= x.split("\n").map(x => ScansionParser.parseLine(x).asJava)
      val blah = Json.toJson(parsed)

      sender ! AddLines( blah )
      watchers = watchers + sender
    case UnWatchLine(_) =>
      watchers = watchers - sender
      println("HEYO")
  }
}

object LineActor {
  lazy val lineActor: ActorRef = Akka.system.actorOf(Props(classOf[LineActor]))
}

case class WatchLine(symbol:String)

case class UnWatchLine(symbol:String)

case class AddLines(lines:JsonNode)
