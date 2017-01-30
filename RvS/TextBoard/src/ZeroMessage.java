//Falls ein Thema nicht existiert sollte eine 0 zurueck gegeben werden
class ZeroMessage extends Message{

        public ZeroMessage(){
                super("0");
        }
        public String toString() {
                return "0\n";
        }
}
       
