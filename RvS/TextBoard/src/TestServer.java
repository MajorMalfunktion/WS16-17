import java.net.*;
import java.io.*;
import java.util.LinkedList;
import java.util.concurrent.*;

public class TestServer extends Thread{
        private ServerSocket                            ssocket;
        private TextBoard                               board;
        private ConcurrentLinkedQueue<ClientHandler>    clients;

        public TestServer(int port){
                try{
                        ssocket = new ServerSocket(port);
                }catch(IOException ioe){
                        System.out.println("IOException: " + ioe.getMessage());
                }
                clients = new ConcurrentLinkedQueue<ClientHandler>();
                board   = new TextBoard();
        }
        @Override
        public void run(){
                boolean terminate = false;
                while(!terminate){
                        try{
                                handleConnection(ssocket.accept());
                        }catch(IOException ioe){
                                System.out.println("IOException: " + ioe.getMessage());
				                terminate = true;
                        }                      
                }
        }
        /*handleConnection uebergibt den Socket and einen ClientHandler
         *Dieser uebernimmt dann die Verteilung der Nachrichten
         */
        public void handleConnection(Socket socket){
                try{
                        System.out.println("Starte neuen client handler");
                        ClientHandler client = new ClientHandler(board, socket, clients);
                        client.start();
                        clients.add(client);
                }catch(Exception e){
                        System.out.println("Exception in handleConnection " + e.getMessage());
                        e.printStackTrace();
                }
        }
        public void finalize() throws IOException{
                // Leert die Liste der Clients
                clients.clear();
                if(ssocket != null) ssocket.close();
	    }
        public static void main(String[] args){
                if(args.length != 1){
                        System.out.println("Nur den Port angeben, es wird per default" +
                                            " der Hostserver benutzt");
                }else{
		                try {
                                TestServer server = new TestServer(Integer.parseInt(args[0]));
			                    server.start();
			                    server.join();
			                    System.out.println("Server ist runter gefahren!");
		                }catch (Exception e){
			                    System.out.println(e.getMessage());
		                }
                }
        }
}
