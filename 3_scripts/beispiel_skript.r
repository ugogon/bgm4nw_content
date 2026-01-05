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

# Ergebnisse
result <- data.frame(Umgebung = Umgebung,
                     Text = Text,
                     CompVal1,
                     CompVal2,
                     RefVal1,
                     RefVal2)
exportJSON <- toJSON(result)
write(exportJSON, "output.json")