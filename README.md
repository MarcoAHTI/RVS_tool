# RVS Tool

Korte reminder om de app lokaal te starten.

## Lokaal runnen (aanrader: R Interactive)

1. Open een **R Interactive** terminal in VS Code.
2. Run:

```r
setwd("c:/Users/MarcoGriepAHTI/Git Repos/RVS_tool")
shiny::runApp("app.R")
```


asdfkjhasdf

## Alternatief (als je al in de projectmap zit)

```r
shiny::runApp("app.R")
```

## Stoppen

- Druk `Esc` in de R-console, of gebruik `Ctrl + C` in de terminal.

## Opmerking

- Benodigde packages worden in `app.R` automatisch geïnstalleerd en geladen bij opstarten.
