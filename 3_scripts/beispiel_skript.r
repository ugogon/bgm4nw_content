# Libraries
library(jsonlite)

# Der allgemeine Dataframe mit allen Variablen
dat <- read.csv("input.csv")

# Berechnungen
Umgebung <- mean(dat$Umgebung1, dat$Umgebung2)
Umgebung <- Umgebung + 1

Text1 <- "Ihre Arbeitsumgebung ist wenig digitalisiert."
Text2 <- "Ihre Arbeitsumgebung ist mittelmäßig digitalisiert."
Text3 <- "Ihre Arbeitsumgebung ist stark digitalisiert."

Text <- Text1
Text <- ifelse(Umgebung == 3, Text2, Text)
Text <- ifelse(Umgebung > 3, Text3, Text)

CompVal1 <- dat$Kompetenz1
CompVal2 <- dat$Kompetenz2

RefVal1 <- 1.5
RefVal1 <- ifelse(Umgebung == 3, 3.5, RefVal1)
RefVal1 <- ifelse(Umgebung > 3, 4, RefVal1)

RefVal2 <- 1
RefVal2 <- ifelse(Umgebung == 3, 2, RefVal2)
RefVal2 <- ifelse(Umgebung > 3, 4, RefVal2)

Problem <- ifelse(CompVal1 < RefVal1, 1, 0)

CompText1 <- "Ihre digitale Kompetenz ist im Vergleich zu anderen Personen, welche in ähnlich digitalen Arbeitssettings angestellt sind, eher niedrig ausgeprägt. Hier könnte Handlungsbedarf bestehen. Informieren Sie sich am besten über Angebote und Materialien, welche Ihnen helfen können, diese zu stärken."
CompText2 <- "Ihre digitale Kompetenz ist im Vergleich zu anderen Personen, welche in ähnlich digitalen Arbeitssettings angestellt sind, gut ausgeprägt. Hier besteht aktuell kein Handlungsbedarf."
CompText <- ifelse(Problem == 1, CompText1, CompText2)

Stress <- ifelse(CompVal2 > RefVal2, 1, 0)

StressText1 <- "Ihr Technostress ist für Ihre Arbeitsumgebung eher hoch ausgeprägt. Behalten Sie dies im Auge und informieren Sie sich über mögliche Unterstützung, welche Ihnen bereitstehen."
StressText2 <- "Ihr Technostress ist für Ihre Arbeitsumgebung gering ausgeprägt. Hier besteht kein Handlungsbedarf."
StressText <- ifelse(Stress == 1, StressText1, StressText2)

# Ergebnisse
result <- data.frame(Umgebung = Umgebung,
                     Text = Text,
                     CompVal1 = CompVal1,
                     CompVal2 = CompVal2,
                     RefVal1 = RefVal1,
                     RefVal2 = RefVal2,
                     CompText = CompText,
                     StressText = StressText)
exportJSON <- toJSON(result)
write(exportJSON, "output.json")