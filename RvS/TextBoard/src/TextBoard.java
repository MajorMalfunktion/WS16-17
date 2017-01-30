import java.net.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

/*Die Nachrichten werden im TextBoard ueber eine Hashmap gespeichert 
 * Dabei sind die Themen die Keys
 * Mehrere Nachrichten werden in einer ConcurrentLinkedQueue am jeweiligen Key gespeichert
 */
public class TextBoard{
        private ConcurrentHashMap<String, ConcurrentLinkedQueue<Message>> board;

        public TextBoard(){
                this.board = new ConcurrentHashMap<String, ConcurrentLinkedQueue<Message>>();
        }
        public void addMessage(String theme, Message message){
                //falls das Thema existiert wird der Queue hinzugefuegt
                if (this.board.containsKey(theme)){
                        ConcurrentLinkedQueue<Message> queue = this.board.get(theme);
                        queue.add(message);
                //ansonsten wird eine Neue erstellt und mit dem Key eingeordnet
                }else{
                        ConcurrentLinkedQueue<Message> queue = new ConcurrentLinkedQueue<Message>();
                        queue.add(message);
                        this.board.put(theme, queue);
                }
        }
        public PriorityQueue<Message> getMessages(String theme){
                PriorityQueue<Message> outQueue = new PriorityQueue<Message>();
                if (this.board.containsKey(theme)){
                        ConcurrentLinkedQueue<Message> inQueue = this.board.get(theme);
                        //erstellt eine automatisch sortierte Queue
                        for (Message m : inQueue){
                                outQueue.add(m);
                        }
                }
                return outQueue;
        }
        //getter
        public Collection<ConcurrentLinkedQueue<Message>> getMessages(){
                return this.board.values();
        }
        public LinkedList<Message> getMessagesNewerThan(int time){
                LinkedList<Message> messages = new LinkedList<Message>();
                for (ConcurrentLinkedQueue<Message> thread : getMessages()){
                        //gleicht bei allen Messages ab, ob sie nach time geschrieben wurden
                        for (Message m : thread){
                                if (m.time > time){
                                        messages.add(m);
                                }
                        }
                }
                return messages;
        }
        public PriorityQueue<Message> getNewestTopics(int nTopics){
                //Queue die nur ThemeOnlyMassege Objekte enthalten wird
                PriorityQueue<Message> messages = new PriorityQueue<Message>();
                //sucht die zuletzt geanderte Zeit des jew. Themas raus und fuegt es zur Queue hinzu
                for (Map.Entry<String,ConcurrentLinkedQueue<Message>> entry : board.entrySet()){
                        ConcurrentLinkedQueue<Message> thread = entry.getValue();
                        long time = 0;
                        for (Message m : thread){
                                time = Math.max(time, m.time);
                        }
                        ThemeOnlyMessage msg = new ThemeOnlyMessage(entry.getKey());
                        msg.setTime(time);
                        messages.add(msg);
                }
                //da nur nTopics ausgegeben werden sollen, werden auch nur soviele ausgegeben
                if (nTopics > 0 && nTopics < messages.size()){
                        PriorityQueue<Message> result = new PriorityQueue<Message>();
                        for (int i = 0; i < nTopics; ++i)   result.add(messages.poll());
                        return result;
                                       
                }else{
                        return messages;
                }
        }
}
