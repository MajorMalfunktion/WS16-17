//Die Klasse ist fuer die  Error Messages zustaendig
class ErrorMessage extends Message {

        public ErrorMessage(){
                super("E");
        }
        @Override
        public String toString(){
                return "E " + text.peek() + "\n";
        }
}
