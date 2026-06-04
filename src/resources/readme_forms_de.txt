PascalScript Forms – Ablagekonvention

TurboBird speichert benutzerdefinierte Formulare im folgenden Verzeichnis:

[TurboBird]/data/PSStudio/Forms/<Server>/<DBAlias>/<Formularname>/

Beispiel:
/opt/TurboBird/data/PSStudio/Forms/MeinServer/EmployeeDB/KundenEditor/KundenEditor.cfrm
/opt/TurboBird/data/PSStudio/Forms/MeinServer/EmployeeDB/KundenEditor/KundenEditor.rops


Wichtige Regeln:

Jedes Formular liegt in einem eigenen Unterverzeichnis, dessen Name dem Formularnamen entspricht.
Im Verzeichnis befinden sich genau zwei Dateien:

<Formularname>.cfrm – das Formular-Layout
<Formularname>.rops – das Pascal-Script

Der Ordnername und beide Dateinamen (ohne Endung) müssen exakt gleich sein – inklusive Groß-/Kleinschreibung.
Die Dateiendungen sind immer klein zu schreiben (.cfrm, .rops).
Server- und Datenbank-Alias werden automatisch von TurboBird bereinigt (Sonderzeichen werden durch _ ersetzt).
Nur Verzeichnisse, die diese Bedingungen erfüllen, werden von TurboBird als gültige Formulare erkannt und im Baum unter Forms angezeigt.
