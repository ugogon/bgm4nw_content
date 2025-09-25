library("jsonlite")

dat <- read.csv("input.csv")
print("Zwischenergebniss")
result <- data.frame( weiter="Test", world=4, result = dat[1,3]+dat[1,4])

exportJSON <- toJSON(result)
write(exportJSON, "output.json")
