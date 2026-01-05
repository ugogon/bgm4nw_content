# Der allgemeine Dataframe mit allen Variablen
dat <- read.csv("input.csv")
Umgebung <- mean(Umgebung1, Umgebung2)
result <- data.frame(Umgebung = Umgebung)
exportJSON <- toJSON(result)
write(exportJSON, "output.json")