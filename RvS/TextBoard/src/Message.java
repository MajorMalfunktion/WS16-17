import java.util.LinkedList;

class Message implements Comparable{
        //protected wird benutzt, da aus Message geerbt wird
        protected long time;
        protected String theme;
        protected LinkedList<String> text;

        //Ein Konstruktor fuer Nachrichten, die nur Ein Thema und eine Zeit enthalten
        public Message(String theme){
                this.time = System.currentTimeMillis() / 1000l;
                this.theme = theme;
                this.text = new LinkedList<String>();
        }
        //Ein Konstruktor fuer normale Nachrichten
        public Message(String theme, LinkedList<String> text){
                this.theme = theme;
                this.time = System.currentTimeMillis() / 1000l;
                this.text = text;
        }
        //setter und getter
        public void setTime(long time){
                this.time = time;
        }
        public long getTime(){
                return this.time;
        }
        public LinkedList<String> getText(){
                return this.text;
        }
        public String getTheme(){
                return this.theme;
        }
        public void addLine(String line){
                this.text.add(line);
        }
        @Override
        public String toString(){
                StringBuffer sbf = new StringBuffer();
                //Vor text muss theme in den Buffer eingelesen werden
                sbf.append("" + (text.size()+1) + "\n");
                sbf.append("" + time + " " + theme + "\n"); 
                for (String line : text){
                        sbf.append(line + "\n");
                }
                return sbf.toString();
        }
        @Override
	    public int compareTo(Object oo){
                //Object muss eine Klasse von Message sein
                Message o = (Message) oo;
                //Abgleichen der Zeit
                if (time == o.time){
                        return 0;
                }else if (time < o.time){
                        return 1;
                }else{
                        return -1;
                }                       
        }
        
}
