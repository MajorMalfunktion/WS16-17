
Im folgenden werde ich Hinweise zur Ausfuehrung meines Programmes geben:

1.  Compilieren mit: path/to/src$ javac *.java <Enter>
2.  Es muessen TestServer und TestClient gestartet werden.
    Dies geht ueber java TestServer <Portnummer> <Enter>
              bzw.  java TestClient <Portnummer> <Enter>
    Es wird per default der local host(127.0.0.1) beutzt.
3.  Es kann nun mit den Befehlen, wie in der Aufgabenstellung ueber das TextBoard komuniziert, 
    bzw. Tests durchgefuehrt werden.

Post:   
        P
        <Anzahl der Nachrichten in der Liste>
        <Anzahl der Zeilen von n0>
        0 <Thema n0>
        <Zeilen von n0>
        ...
        <Anzal der Zeilen von n>
        0 <Thema n>
        <Zeilen von n>

Nachrichten seit einem Zeitpunkt:
        W <Zeitpunkt>

Nachrichten von einem Thema in zeitlich absteigender Reihenfolge:
        T <Thema>

Letzten n geaenderten Themen in zeitlich absteigender Reihenfolge:
        L <n>

Prtotokoll beenden:
X
