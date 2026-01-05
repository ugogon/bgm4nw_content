# Der allgemeine Dataframe mit allen Variablen
dat <- read.csv("input.csv")

result <- data.frame()
exportJSON <- toJSON(result)
write(exportJSON, "output.json")