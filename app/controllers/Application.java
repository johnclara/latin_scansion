package controllers;

import actors.*;
import akka.actor.*;
import akka.actor.ActorRef;
import com.fasterxml.jackson.databind.JsonNode;
import play.libs.Akka;
import play.libs.F;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.WebSocket;
import scala.Option;


/**
 * The main web controller that handles returning the index page, setting up a WebSocket, and watching a stock.
 */
public class Application extends Controller {

    public Result index() {
        return ok(views.html.index.render());
    }

    public WebSocket<JsonNode> ws() {
        return new WebSocket<JsonNode>() {
            public void onReady(final WebSocket.In<JsonNode> in, final WebSocket.Out<JsonNode> out) {
                // create a new UserActor and give it the default stocks to watch
                final ActorRef userActor = Akka.system().actorOf(Props.create(UserActor.class, out));
                
                // send all WebSocket message to the UserActor
                in.onMessage(new F.Callback<JsonNode>() {
                    @Override
                    public void invoke(JsonNode jsonNode) throws Throwable {
                        // parse the JSON into WatchLine
                        WatchLine watchStock = new WatchLine(jsonNode.get("full_text").textValue());
                        // send the watchStock message to the StocksActor
                        LineActor.lineActor().tell(watchStock, userActor);
                    }
                });

                // on close, tell the userActor to shutdown
                in.onClose(new F.Callback0() {
                    @Override
                    public void invoke() throws Throwable {
                        final Option<String> none = Option.empty();
                        LineActor.lineActor().tell(new UnWatchLine(""), userActor);
                        Akka.system().stop(userActor);
                    }
                });
            }
        };
    }

}
