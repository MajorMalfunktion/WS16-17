import java.net.*;
import java.io.*;

public class TestClient extends Thread{
        /*Die folgende private Klasse dient zum lesen und 
         * ausgeben des Inputstreams
         */
        private class InputReaderThread extends Thread {
                private BufferedReader streamIn;

                InputReaderThread(BufferedReader streamIn){
                        this.streamIn = streamIn;
                }
                public void run() {
                        while (!isInterrupted() && this.streamIn != null){
                                try{
                                        String line = this.streamIn.readLine();
                                        if (line != null)   System.out.println("< " + line);
                                        else                throw new IOException("received null");
                                }catch(IOException ioe){
                                        //Bei einer Exception wird die Verbindung abgebrochen
                                        System.err.println("Error in InputReader -> shutting down");
                                        interrupt();
                                }
                        }
                }
        }
        private Socket              socket      = null;
        private BufferedReader      streamIn    = null;
        private BufferedWriter      streamOut   = null;
        private BufferedReader      userInput   = null;

        public TestClient(String host, int port){
                try{
                        System.out.println("Suche Server");
                        this.socket = new Socket(host, port);
                        System.out.println("Verbindung zum Server wurde erstellt");
                        streamOut   = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
                        streamIn    = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                        userInput   = new BufferedReader(new InputStreamReader(System.in));
                        System.out.println("board verfuegbar, mit 'X' wird der client beendet");
                }catch(UnknownHostException uhe){
                        System.out.println("UnknownHostException: " + uhe.getMessage());
                }catch(IOException ioe){
                        System.out.println("IOException: " + ioe.getMessage());
                }
        }

        @Override
        public void run(){
                /*An dieser stelle starte ich den Thread aus der privaten KLasse,
                 * somit existiert fuer den Ausleseprozess ein eigener Thread.
                 */
                InputReaderThread reader = new InputReaderThread(this.streamIn);
                reader.start();
                while(!this.isInterrupted() && reader.isAlive()){
                        try{
                                //Zuerst sehen wir uns die Eingaben an
                                String line = userInput.readLine();
                                //Dann wird gecheckt ob wir eine Message haben und "flushen"
                                if (line.equals("X"))/*Abbruchkriterium*/{ 
                                        streamOut.write(line + "\n");
                                        streamOut.flush();
                                        interrupt();
                                        reader.interrupt();
                                }else{
                                        streamOut.write(line + "\n");
                                        streamOut.flush();
                                }
                        }catch(IOException ioe){
                                System.err.println("Server shutdown -> stopping");
                                System.out.println(ioe.getMessage());
                                interrupt();
                        }
                }
                try {
                        reader.join();
                } catch(Exception e) {
                        System.err.println("Error waiting for reader");
                }
        }
        //In der Main muss nur der Port angegeben werden.
        public static void main(String[] args){
                TestClient client = null;
                if (args.length != 1){
                        System.out.println("Nur den Port angeben, es wird per default" +
                                           " der Hostserver benutzt");
                }else{
                        try{
                                client = new TestClient("127.0.0.1", Integer.parseInt(args[0]));
                                client.start();
                                client.join();
                        }catch (Exception e){
                                System.out.println(e.getMessage());
                        }
                }
        }
}
