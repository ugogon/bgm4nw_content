# Erstellt von Felix Leitner, am 04.07.25
# Für das Projekt BGM4NW der Universitätsmedizin Göttingen
# Dieser Code Erstellt allgemeine Variablen, welche nicht für jede Person
# aktualisiert werden müssen
#### Libraries #### ------------------------------------------------------------
library(cluster)
library(Hmisc)
library(fmsb)
library(scales)
library(randomForest)
library(ggplot2)
library(dplyr)
library(writexl)
library(caret)
library(mclust)
library(r2pmml)
#### Code for the cluster-analysis #### ----------------------------------------
#Import dataset
df <- read.csv("df.csv")

#############################################################################
# Bestes Modell speichern
saveRDS(model_short_20$finalModel, "rf_model_cluster.rds")

r2pmml(rf_model, "rf_model.pmml")

# Modell laden 
#loaded_model <- readRDS("rf_model_cluster.rds")

Stringing <- "Führung"
Encoding(Stringing)