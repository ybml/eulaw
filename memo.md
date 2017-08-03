# Small Solutions

Hey,

Fionn und ich haben heute doch kurz über das Datenprojekt gesprochen. Bezgl. des Umgangs mit den Veränderungen hatten wir etwas unterschiedliche Auffassungen. Ich beschreibe kurz einmal wie ich es machen würde und warum.

Ich würde so vorgehen wie in merger.R. Ich würde zunächst den Vertrag scrapen. Dann würde ich Artikel für Artikel durch den Vertrag gehen und jede Veränderung durchführen.

Die Artikel 1-3, z.B., fügen neue Artikel zu. Dann habe ich code der so was macht.

``` R
new_articles = subset(merger, merger$merger_id == "1.1" | merger$merger_id == "1.2" | merger$merger_id == "1.3")
all_articles= bind_rows(all_articles, new_articles)
```
all_articles sind die Daten mit den aktuellen Artikeln, also ECSC, EEC und EURATOM am Anfang (in merger.R heißt es to_be_changed).

Der nächste Artikel im Treaty sagt an, dass zwei Artikel gelöscht (repealed) werden sollen.

``` R
all_articles = all_articles[!all_articles$ecsc_id == "2.3.27", ]
all_articles = all_articles[!all_articles$ecsc_id == "2.3.29", ]
```

Danach kommt ein Artikel, der den ersten Paragraph in einem Artikel ersetzt.

``` R
all_articles$text[all_articles$ecsc_id == "2.2.22"] = gsub("The Assembly shall hold an annual session.",
"The Assembly shall hold an annual session. It shall meet, without requiring to be convened, on the second Tuesday in March.",
all_articles$text[all_articles$ecsc_id == "2.2.22"])
```

Dann kommen wieder Artikel die hinzugefügt werden sollen und wir benutzen wieder code wie den für Artikel 1-3.

Wir gehen also nach und nach durch und schreiben relativ viel code. Funktionen schreiben wir, um uns die einzelnen Schritte etwas leichter zu machen.

Dabei verzichten wir auch eine Tabelle, die die Veränderungen trägt und über die wir eine Funktion laufen lassen.

Aus folgenden Gründen würde ich es so machen: 1) Handarbeit haben wir so oder so, entweder für die Tabelle, oder im Skript. 2) Meine Lösung funktionert und ist nicht auf eine große Funktion angewiesen. Es ist zwar nicht besonders ellegant, aber wir kommen kontinuierlich weiter. 3) Kleine Probleme und Unvorhergesehenes können wir immer schnell local handeln und müssen nicht die Funktion verändert, um eine Lösung für den ganzen Vertrag zu finden.

Ich habe gestern den Merger Treaty gemacht und natürlich ist es viel Arbeit, aber man kommt so durch. TEU, Amsterdam und Lissabon sind ganz andere Brocken, aber auch die sollte man in 2-3 Tagen bewältigen können. Damit wären wir in 3-4 Tagen Einsatz pro Person (natürlich verteilt über die nächste Zeit) fertig.
 
