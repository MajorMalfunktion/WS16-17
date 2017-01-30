//Dise Klasse ist fuer Nachrichten die nur Zeit und Thema ausgeben sollen
public class ThemeOnlyMessage extends Message {

        public ThemeOnlyMessage(String theme){
                super(theme);
        }
        @Override
        public String toString(){
                return "" + time + " " + theme + "\n"; 
        }
}
