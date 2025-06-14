# Elm Pomodoro Timer

Ein einfacher Pomodoro-Timer, geschrieben in Elm.

## Installation

1. Stelle sicher, dass Elm installiert ist:
```bash
npm install -g elm
```

2. Klone dieses Repository

3. Installiere die Abhängigkeiten:
```bash
elm make src/Main.elm
```

## Verwendung

1. Starte den Entwicklungsserver:
```bash
elm reactor
```

2. Öffne http://localhost:8000 in deinem Browser

3. Gib die gewünschte Zeit in Minuten ein und klicke auf "Start Timer"

## Funktionen

- Timer starten mit benutzerdefinierter Zeit
- Timer pausieren/fortsetzen
- Timer zurücksetzen
- Farbliche Anzeige des Timer-Status (grün = aktiv, grau = pausiert) 