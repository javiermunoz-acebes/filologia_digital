# R für Philolog_innen: Ein praktischer Einstieg mit politischen Reden

Didaktisches Projekt zur Einführung in **R** für Philologiestudierende anhand der Analyse politischer Reden auf Deutsch.

## Beschreibung

Dieses Projekt bietet ein praxisorientiertes Beispiel für den Einsatz von **R** in der text- und datenbasierten Analyse innerhalb der digitalen Philologie. Das Material richtet sich an Studierende mit geringen oder keinen Vorkenntnissen in der Programmierung und führt Schritt für Schritt in einen reproduzierbaren Workflow zur Untersuchung eines politischen Redekorpus ein.

Als Fallbeispiel dienen die Reden der deutschen **Bundespräsident:innen**, die aus dem *German Political Speeches Corpus* von Adrien Barbaresi stammen. Auf der Grundlage dieses XML-Korpus zeigt das Skript, wie Texte und Metadaten in eine analysierbare Arbeitsumgebung überführt und für quantitative sowie interpretative Fragestellungen genutzt werden können.

## Ziele

- Einführung in die grundlegende Nutzung von **R** und **RStudio** in philologischen Kontexten.
- Vermittlung zentraler Begriffe der Korpuslinguistik und der digitalen Textanalyse.
- Verbindung von **close reading** und **distant reading** in einem anwendungsnahen Projekt.
- Bereitstellung einer wiederverwendbaren Vorlage für andere Korpora, Sprachen und Forschungsfragen.

## Inhalt des Skripts

Das Skript entwickelt schrittweise einen vollständigen Analyseprozess:

1. **Laden und Vorbereiten des Korpus**
   - Automatischer Download des Korpus von Zenodo.
   - Einlesen der XML-Datei.
   - Extraktion von Metadaten und Redeinhalten.

2. **Erstellung eines Data Frames**
   - Umwandlung der Reden in eine strukturierte Tabelle.
   - Organisation von Variablen wie Präsident:in, Datum, Ort und Text.

3. **Explorative Grundanalyse**
   - Anzahl der Reden pro Präsident:in.
   - Verteilung der Reden nach Jahren.
   - Erste Visualisierungen mit `ggplot2`.

4. **Längenmaße**
   - Berechnung von Zeichen-, Wort- und Satzanzahl.
   - Vergleich der durchschnittlichen Redelänge.

5. **Tokenisierung und Wortfrequenzen**
   - Zerlegung der Texte in einzelne Wörter.
   - Entfernung von Stopwörtern.
   - Bestimmung des häufigsten Vokabulars.

6. **Lexikalische Visualisierung**
   - Erstellung von Wortwolken.
   - Vergleich zwischen Präsident:innen oder Zeitabschnitten.

7. **Bigramme und häufige Wortverbindungen**
   - Identifikation wiederkehrender Wortpaare.
   - Beobachtung rhetorischer Formeln und diskursiver Muster.

8. **Lexikalische Vielfalt und Komplexität**
   - Berechnung der Type-Token-Ratio (TTR).
   - Zusammenhang zwischen Textlänge und lexikalischer Variation.

9. **Schlüsselwörter und Konkordanzen**
   - Analyse von Begriffen wie *Europa*, *Freiheit*, *Krise* oder *Flüchtlinge*.
   - Kontextanalyse mittels KWIC (*Key Word In Context*).

10. **Diachrone Perspektive**
    - Zeitliche Entwicklung politisch relevanter Begriffe.
    - Verbindung zwischen Wortfrequenzen und zeithistorischen Konstellationen.

## Didaktischer Ansatz

Das Projekt entstand im Rahmen eines Seminars zur digitalen Philologie mit Studierenden der Germanistik an der Universidad de Valladolid. Ziel war es, **R** nicht als Selbstzweck, sondern als praktisches Werkzeug zur Bearbeitung philologischer Fragestellungen einzuführen. 

Das Tutorial folgt einem schrittweisen und stark anwendungsbezogenen Aufbau. Es eignet sich daher nicht nur als konkretes Unterrichtsmaterial, sondern auch als Vorlage für ähnliche Projekte in den Digital Humanities. 

## Verwendete Pakete

- `xml2`
- `dplyr`
- `tibble`
- `lubridate`
- `ggplot2`
- `stringr`
- `tidytext`
- `stopwords`
- `tidyr`
- `wordcloud`
- `RColorBrewer`
- `udpipe` (als methodische Erweiterung)

## Korpus

Die Analyse basiert auf der Datei `Bundespräsidenten.xml`, die Teil des *German Political Speeches Corpus* ist und über Zenodo (DOI: 10.5281/zenodo.3611246) bereitgestellt wird. Das Skript lädt das Material bei Bedarf automatisch herunter und verarbeitet die XML-Struktur für die weitere Analyse. 

DOI: `10.5281/zenodo.3611246` 

## Zielgruppe

Dieses Material richtet sich insbesondere an:

- Studierende der Philologie oder Linguistik ohne Programmiererfahrung.
- Einführungsveranstaltungen in den Digital Humanities.
- Lehrkontexte mit Schwerpunkt auf Korpusarbeit und reproduzierbarer Textanalyse.

## Erweiterungsmöglichkeiten

Das Skript ist bewusst als offene Vorlage konzipiert. Aufbauend auf diesem Workflow lassen sich unter anderem folgende Erweiterungen integrieren:

- Lemmatisierung,
- stilometrische Analysen,
- Vergleiche zwischen Autor:innen oder Zeiträumen,
- Sentimentanalyse,
- Einbindung weiterer literarischer, journalistischer oder historischer Korpora.

## Autorenschaft

Ángeles González Miguel  
Francisco Javier Muñoz-Acebes
