# Der allgemeine Dataframe mit allen Variablen
dat <- read.csv("input.csv")

# Berechnungen
Umgebung <- mean(dat$Umgebung1, dat$Umgebung2)


# Ergebnisse
result <- data.frame(Umgebung = Umgebung)