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


# Ergebnisse
result <- data.frame(Umgebung = Umgebung,
                     Text = Text)
exportJSON <- toJSON(result)
write(exportJSON, "output.json")