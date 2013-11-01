VS-2013-WS-A2-LMJM
==================

VS Praktikum WS 2013, Aufgabe 2

Umsetzen eines distributiven Algorithmus zum ermitteln des minimalen Spannbaums in ungerichteten
gewichteten Graphen nach Gallager, Humblet, Spira.

Start des Algorithmus über das Shell-Skript
`run_nodestarter Graph` wobei Graph ein Pfad zu den Konfigurationsdateien ist, die den Graphen beschreiben.

Initialisieren eines einzelnen Nodes über node_start.erl

Beispiel
`erl -sname node_x -setcookie dmst`

Jetzt die Erlang-Quelldateien Kompilieren
`c(node_start), c(util), c(node).`

Einen node über main starten
`node_start:main("./5_nodes/node_0.conf").`

Die Konfiguration eines einzelnen nodes beschreibt den Namen des Nodes über den Dateinamen und
die adjazenten Kanten mit der Syntax `Weight, NeighbourNodeName`
Beispiel: node_0.conf
    6,node_1
    4,node_4
 
 Nach dem alle Nodes des Graphen initialisiert sind, muss mindestens einer aufgeweckt werden, um den
 Algortihmus zu starten (in der Erlang-Konsole).
 
 `global:whereis_name(node_0) ! wakeup.`
