import java.net.*;
import java.io.*;
import java.util.*;

//Die folgende Klasse behandelt nur das Parsing(Syntaxanalyse) von Nachrichten
public class MessageParsing {

        public static LinkedList<Message> parseMessages(BufferedReader streamIn, boolean ignoreTime) throws Exception{
                LinkedList<Message>  list = new LinkedList<Message>();
                //Erkennen um wie viele Nachrichten es sich handelt
                int listCount   = Integer.parseInt(streamIn.readLine());
                while (listCount != 0){
                        //einzelne Nachrichten parsen
                        Message m  = parseMessage(streamIn, ignoreTime);
                        list.add(m);
                        --listCount;
                }
                return list;
        }
        public static Message parseMessage(BufferedReader streamIn, boolean ignoreTime) throws Exception{
                //da them von text separiert wird muss mit 1 subtrahiert werden
                int     numLines = Integer.parseInt(streamIn.readLine()) - 1;
                //fuer weitere Syntaxanalyse wird die headline zwischengespeichert
                String  headLine = streamIn.readLine();
                long    time     = Long.parseLong(headLine.split(" ", 2)[0]);
                String  theme    = (headLine.split(" ", 2)[1]).trim();
                LinkedList<String> text = new LinkedList<String>();
                while(numLines != 0){
                        text.add(streamIn.readLine()); 
                        --numLines;
                }
                Message message = new Message(theme, text);
                if (!ignoreTime)    message.setTime(time);
                return message;
        }
}
