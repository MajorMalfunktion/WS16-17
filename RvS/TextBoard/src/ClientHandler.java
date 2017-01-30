import java.net.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class ClientHandler extends Thread{
        /* Die private Klasse sorgt dafuer, dass der Input des Clients gelesen
         * und verarbeitet wird
         */
        private class InputReaderThread extends Thread{
                TextBoard       board;
                BufferedReader  streamIn;
                ClientHandler   handler;
                
                public InputReaderThread(TextBoard board, BufferedReader streamIn, ClientHandler handler){
                        this.board = board;
                        this.streamIn = streamIn;
                        this.handler = handler;
                }
                //Hilfsfunktionen
                private void addError(String error){
                        ErrorMessage m = new ErrorMessage();
                        m.addLine(error);
                        this.handler.addMessage(m);
                }
                private void addZeroMessage(){
                        ZeroMessage m = new ZeroMessage();
                        this.handler.addMessage(m);
                }
                private void addMessages(Queue<Message> messages){
                        if (messages.isEmpty())     addZeroMessage();
                        while (!messages.isEmpty()){
                                handler.addMessage(messages.poll());
                        }
                }
                private String[] safeSplit(String line, String error){
                        String[] splitted = line.split(" ", 2);
                        if (splitted.length < 2) {
                                addError(error);
                                return null;
                        }else{
                                return splitted;
                        }
                        
                }
                //Beginn vom message parsing
                public void parseClientRequest() throws Exception{
                        String initialLine = streamIn.readLine();
                        if (streamIn.ready() || initialLine == null){
                                System.err.println("Stoppe client handler wegen einer getrennten Verbindung.");
                                interrupt();
                                return;
                        }
                        //liest den ersten Character und gleicht ihn ab
                        char c = initialLine.charAt(0);
                        if(c == 'P'){
                                try {
                                        LinkedList<Message> messages = MessageParsing.parseMessages(this.streamIn, true);
                                        for (Message m : messages){
                                                board.addMessage(m.getTheme(), m);
                                                handler.broadcastToAll(m);
                                        }
                                }catch(Exception e){
                                        System.err.println("Error beim message parsing-> sende error");
                                        e.printStackTrace();
                                        addError("Ungueltige Nachricht vom Client.");
                                }
                        }else if(c == 'W'){
                                String[] splitted = safeSplit(initialLine, "W braucht eine Zeitangabe!");
                                if (splitted == null)   return;
                                int time = Integer.parseInt(splitted[1]);
                                if (time < 0){
                                        addError("Ungueltige Zeitangabe fuer W!");
                                }else{
                                        // Ausgabe aller Nachrichten nach time
                                        // die Aufgabenstellung erwartet nicht eine sortierte Reihenfolge
                                        // so geht es schneller
                                        LinkedList<Message> messages = board.getMessagesNewerThan(time);
                                        addMessages(messages);
                                }
                        }else if(c == 'T'){
                                String[] splitted = safeSplit(initialLine, "T braucht Thema!");
                                if (splitted == null)   return;
                                // keine Leerzeichen 
                                String theme = (splitted[1]).trim();
                                PriorityQueue<Message> messages = board.getMessages(theme);
                                addMessages(messages);
                        }else if(c == 'L'){
                                String[] splitted = safeSplit(initialLine, "L braucht eine Anzahl von Themen!");
                                if (splitted == null)   return;
                                int nTopics = Integer.parseInt(splitted[1]);
                                PriorityQueue<Message> messages = board.getNewestTopics(nTopics);
                                addMessages(messages);
                        }else if(c == 'X'){
                                System.err.println("Client will einen interrupt! Fahre runter!");
                                interrupt();
                        }else{
                                addError("unbekannter Befehl " + c);
                        }
                }
                @Override
                public void run(){
                        while(!isInterrupted()){
                                try{
                                        parseClientRequest();
                                }catch(Exception ioe){
                                        System.out.println("Exception: " + ioe.getMessage());
                                        ioe.printStackTrace();
                                        addError("ungueltige Anfrage des Client");
                                }
                        }
                }
        }
        private Socket                                  socket;
        private TextBoard                               board;
        private ConcurrentLinkedQueue<ClientHandler>    clients;
        private BufferedReader                          streamIn;
        private BufferedWriter                          streamOut;
        private ConcurrentLinkedQueue<Message>          retrievedMessages;
        private ConcurrentLinkedQueue<Message>          broadcasts;

        public ClientHandler(TextBoard board, Socket socket,  ConcurrentLinkedQueue<ClientHandler> clients){
                this.board = board;
                this.socket = socket;
                this.clients = clients;
                this.retrievedMessages = new ConcurrentLinkedQueue<Message>();
                this.broadcasts = new ConcurrentLinkedQueue<Message>();
                try {
                        this.streamIn = new BufferedReader(
                                                new InputStreamReader(
                                                        socket.getInputStream()
                                                        )
                                                );
                        this.streamOut = new BufferedWriter(
                                                new OutputStreamWriter(
                                                        socket.getOutputStream()
                                                        )
                                                );
                }catch (IOException e){
                        System.out.println(e.getMessage());
                }
        }
        private void sendMessages(){
                int N = retrievedMessages.size();
                boolean multiMessage = N > 1;
                int i = 0;
                while (!retrievedMessages.isEmpty()){
                        Message m = retrievedMessages.poll();
                        try {
                                // die einzelnen messages werden getrennt behandelt
                                if (m instanceof ErrorMessage){
                                        // fuer ErrorMessage
                                        this.streamOut.write(m.toString());
                                }else if (!multiMessage || i > 0){
                                        // bei nur einer Nachricht oder einer Folgenachricht
                                        this.streamOut.write(m.toString());
                                }else{
                                        // N ist die Anzahl der Nachrichten zu Beginn
                                        // N wird noch vor der ersten message gesendet
                                        this.streamOut.write(N + "\n");
                                        this.streamOut.write(m.toString());
                                }
                                this.streamOut.flush();
                                ++i;
                        }catch (IOException ioe){
                                System.out.println("kann die Nachricht nicht dem Client sende, gehe in interrupt.");
                                interrupt();
                        }
                }
                
        }
        private void checkBroadcasts(){
                // Wie in der Aufgabenstellung werden alle Nachrichten anderer Clients eine nach der anderen gesendet
                while (!broadcasts.isEmpty()){
                        Message m = broadcasts.poll();
                        try {
                                this.streamOut.write(m.toString());
                                this.streamOut.flush();
                        }catch (IOException ioe){
                                System.out.println("kann die Nachricht nicht dem Client sende, gehe in interrupt.");
                                interrupt();
                        }
                        
                }
        }
        public void broadcastToAll(Message m){
                for (ClientHandler c: clients){
                        c.addBroadcast(m);
                }
        }
        public void addBroadcast(Message m){
                // broadcast fuer alle anderen Clients
                this.broadcasts.add(m);
        }
        public void addMessage(Message m){
                this.retrievedMessages.add(m);
        }
        @Override
        public void run(){
                InputReaderThread child = new InputReaderThread(this.board, this.streamIn, this);
                child.start();
                while(!isInterrupted() && child.isAlive()){
                        try{
                                // zuruecksenden von messages
                                sendMessages();
                                // Verarbeiten neu angekommener messages
                                checkBroadcasts();
                                // keine Aufgabe, damit wird yield() aufgerufen, sodass Zeit fuer andere Prozesse bleibt
                                yield();
                                // pausieren fuer 100 Milisek. (Nimmt dann nicht zuviel Zeit der CPU in anspruch)
                                sleep(100);
                        }catch(Exception ioe){
                                System.out.println("Exception: " + ioe.getMessage());
                                ioe.printStackTrace();
                                interrupt();
                                child.interrupt();
                        }
                }
                // wartet auf das Runterfahren des InputReaderThread
                try{
                        socket.close();
                        streamIn.close();
                        streamOut.close();
                }catch(IOException ioe){
                        System.err.println("Konnte Socket oder Stream nicht schliessen.!");
                }
                try{
                        child.join();
                }catch(InterruptedException e){
                        System.err.println("join() funktioniert nicht, Runterfahren wird erzwungen!");
                }
        }
}
