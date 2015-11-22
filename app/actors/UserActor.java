package actors;

import akka.actor.UntypedActor;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import play.Play;
import play.libs.Json;
import play.mvc.WebSocket;

import java.util.List;

public class UserActor extends UntypedActor {

    private final WebSocket.Out<JsonNode> out;
    
    public UserActor(WebSocket.Out<JsonNode> out) {
        this.out = out;
        
        // watch the default stocks
        String defaultLine = Play.application().configuration().getString("default.line");

        LineActor.lineActor().tell(new WatchLine(defaultLine), getSelf());
    }
    
    public void onReceive(Object message) {
      if (message instanceof AddLines) {
        // push the stock to the client
        AddLines lines = (AddLines)message;
        ObjectNode addLinesMessage = Json.newObject();
        addLinesMessage.put("type", "addlines");
        addLinesMessage.put("lines",lines.lines());
        System.out.println(lines.lines().toString());
        out.write(addLinesMessage);
      }
    }
}
