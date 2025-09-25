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
View(df5)
#Calculate missing variables & standardize the dataset
df$virtual_mean <- rowMeans(subset(df, select = c (Digi1IKTHaeuf3, Digi1IKTHaeuf4, Digi1IKTHaeuf5, Digi1IKTHaeuf6, Digi1IKTHaeuf7)), na.rm = TRUE)
df$AGIL2AgilMeth_r <- ifelse(is.na(df$AGIL2AgilMeth_scale), 1, df$AGIL2AgilMeth_scale) 

nw.setting2 <- data.frame(df$Flex1_mean_z, df$Flex1_mean_oe, df$Flex4E_mean,
                          df$AGIL1_mean, df$AGIL2AgilMeth_r,
                          df$Demo1OrgStr_mean, df$Demo4Autonomie_mean, df$Demo3Partizipation_mean, 
                          df$virtual_mean, df$Digi3IKT_Nutzung_Vernetzung_mean, df$Digi3IKTAutomation_mean)
nw.setting2_z <- scale(nw.setting2, center=TRUE, scale=TRUE)

#Cluster-analysis
cor <- rcorr(as.matrix(nw.setting2))
d1 <- dist(nw.setting2_z[,1:11], method = "euclidian")
hc2 <- agnes(d1, method = "ward")
d3 <- cophenetic(hc2) 
cor(d1, d3)
df$clust <- cutree(hc2, 6)

#### Competencies used in the LS #### ------------------------------------------
CompetenciesFrame <- data.frame(AGK_Planung <- df$T2_KR1AGK_Planung_mean,
                                AGK_Selbstmotivierung <- df$T2_KR1AGK_Selbstmotivierung_mean,
                                AGK_Stressvermeidung <- df$T2_KR1AGK_Stressvermeidung_mean,
                                PsyEmp_Bedeutsamkeit <- df$KR2PsyEmp_Bed_mean,
                                PsyEmp_Einfluss <- df$KR2PsyEmp_Einfl_mean,
                                PsyEmp_Kompetenz <- df$KR2PsyEmp_Komp_mean,
                                PsyEmp_Selbstbestimmung <- df$KR2PsyEmp_Selbstb_mean,
                                DigKomp_Verarbeitung <- df$T2_KR4DigKomp_Verarbeitung <- rowMeans(subset(df, select = c(T2_KR4DigiKomp1, T2_KR4DigiKomp2, T2_KR4DigiKomp3))),
                                DigKomp_Kommunikation <- df$T2_KR4DigKomp_Kommunikation <- rowMeans(subset(df, select = c(T2_KR4DigiKomp4, T2_KR4DigiKomp5))),
                                DigKomp_Erstellung <- df$T2_KR4DigKomp_Erstellung <- rowMeans(subset(df, select = c(T2_KR4DigiKomp6, T2_KR4DigiKomp7, T2_KR4DigiKomp8))),
                                DigKomp_Sicherheit <- df$T2_KR4DigKomp_Sicherheit <- rowMeans(subset(df, select = c(T2_KR4DigiKomp9, T2_KR4DigiKomp10, T2_KR4DigiKomp11))),
                                DigKomp_Problemloesung <- df$T2_KR4DigKomp_Problemloesung <- rowMeans(subset(df, select = c(T2_KR4DigiKomp12, T2_KR4DigiKomp13, T2_KR4DigiKomp14, T2_KR4DigiKomp15))),
                                HoL_Achtsamkeit <- df$T2_KR5HolSC_Achtsamkeit_mean,
                                HoL_Verhalten <- df$T2_KR5HolSC_Verhalten_mean,
                                Resilienz <- df$KR4Res_mean,
                                GesKomp <- df$T2_KR3GesKomp_mean,
                                Cluster <- df$clust)
write.csv(CompetenciesFrame, file = "CompetenciesFrame.csv")

#### Calculating the Competence-means for each cluster #### --------------------
# Function to calculate means of a variable by Cluster
getMeans <- function(variable)
{
  a1 <- ifelse(df$clust == 1, variable, NA)
  a2 <- ifelse(df$clust == 2, variable, NA)
  a3 <- ifelse(df$clust == 3, variable, NA)
  a4 <- ifelse(df$clust == 4, variable, NA)
  a5 <- ifelse(df$clust == 5, variable, NA)
  a6 <- ifelse(df$clust == 6, variable, NA)
  a <- c(mean(a1, na.rm = TRUE), mean(a2, na.rm = TRUE), mean(a3, na.rm = TRUE),
         mean(a4, na.rm = TRUE), mean(a5, na.rm = TRUE), mean(a6, na.rm = TRUE))
  a <- round(a, digits = 2)
  return(a)
}

# Put the means in a dataframe and save it
CompetenceMeans <- data.frame(AGK_Planung <- getMeans(df$T2_KR1AGK_Planung_mean),
                              AGK_Selbst <- getMeans(df$T2_KR1AGK_Selbstmotivierung_mean),
                              AGK_Stress <- getMeans(df$KR1AGK_Stressvermeidung_mean),
                              PsyEmp_Bed <- getMeans(df$T2_KR2PsyEmp_Bed_mean),
                              PsyEmp_Einf <- getMeans(df$T2_KR2PsyEmp_Einfl_mean),
                              PsyEmp_Komp <- getMeans(df$T2_KR2PsyEmp_Komp_mean),
                              PsyEmp_Selbst <- getMeans(df$T2_KR2PsyEmp_Selbstb_mean),
                              DigKom_Verarb <- getMeans(df$T2_KR4DigKomp_Verarbeitung),
                              DigKom_Komm <- getMeans(df$T2_KR4DigKomp_Kommunikation),
                              DigKom_Erst <- getMeans(df$T2_KR4DigKomp_Erstellung),
                              DigKom_Sich <- getMeans(df$T2_KR4DigKomp_Sicherheit),
                              DigKom_Prob <- getMeans(df$T2_KR4DigKomp_Problemloesung),
                              Care_Acht <- getMeans(df$T2_KR5HolSC_Achtsamkeit_mean),
                              Care_Verh <- getMeans(df$T2_KR5HolSC_Verhalten_mean),
                              Res <- getMeans(df$KR4Res_mean),
                              GesKomp <- getMeans(df$T2_KR3GesKomp_mean))

# Function to calculate the standard deviation of a variable by Cluster
getSd <- function(variable)
{
  a1 <- ifelse(df$clust == 1, variable, NA)
  a2 <- ifelse(df$clust == 2, variable, NA)
  a3 <- ifelse(df$clust == 3, variable, NA)
  a4 <- ifelse(df$clust == 4, variable, NA)
  a5 <- ifelse(df$clust == 5, variable, NA)
  a6 <- ifelse(df$clust == 6, variable, NA)
  a <- c(sd(a1, na.rm = TRUE), sd(a2, na.rm = TRUE), sd(a3, na.rm = TRUE),
         sd(a4, na.rm = TRUE), sd(a5, na.rm = TRUE), sd(a6, na.rm = TRUE))
  a <- round(a, digits = 2)
  return(a)
}

# Put the standard deviations in a dataframe and save it
DeviationMeans <- data.frame(AGK_Planung_sd <- getSd(df$T2_KR1AGK_Planung_mean),
                             AGK_Selbst_sd <- getSd(df$T2_KR1AGK_Selbstmotivierung_mean),
                             AGK_Stress_sd <- getSd(df$KR1AGK_Stressvermeidung_mean),
                             PsyEmp_Bed_sd <- getSd(df$T2_KR2PsyEmp_Bed_mean),
                             PsyEmp_Einf_sd <- getSd(df$T2_KR2PsyEmp_Einfl_mean),
                             PsyEmp_Komp_sd <- getSd(df$T2_KR2PsyEmp_Komp_mean),
                             PsyEmp_Selbst_sd <- getSd(df$T2_KR2PsyEmp_Selbstb_mean),
                             DigKom_Verarb_sd <- getSd(df$T2_KR4DigKomp_Verarbeitung),
                             DigKom_Komm_sd <- getSd(df$T2_KR4DigKomp_Kommunikation),
                             DigKom_Erst_sd <- getSd(df$T2_KR4DigKomp_Erstellung),
                             DigKom_Sich_sd <- getSd(df$T2_KR4DigKomp_Sicherheit),
                             DigKom_Prob_sd <- getSd(df$T2_KR4DigKomp_Problemloesung),
                             Care_Acht_sd <- getSd(df$T2_KR5HolSC_Achtsamkeit_mean),
                             Care_Verh_sd <- getSd(df$T2_KR5HolSC_Verhalten_mean),
                             Res_sd <- getSd(df$KR4Res_mean),
                             GesKomp_sd <- getSd(df$T2_KR3GesKomp_mean))
write.csv(DeviationMeans, file = "DeviationMeans.csv")

#### Calculation of the BGM-score #### -----------------------------------------
# The BGM-score is made up of 3 parts:
# 1 "Haben Sie diese Angebote selbst in Anspruch genommen?" Prozentual auf eine 0 - 5 skala umgerechnet
# 2 "Ich habe diese Angebote als hilfreich empfunden." skala von 0 - 5
# 3 "Ich wünsche mir Angebote zur Förderung körperlicher Bewegung." skala von 0 - 5
calculateBGMScore <- function(inanspruch, nutzen, wunsch)
{
  #Inanspruchnahme unter den einzelnen Clustern Variablenbildung
  i1 <- ifelse(df$clust == 1, inanspruch, NA)
  i2 <- ifelse(df$clust == 2, inanspruch, NA)
  i3 <- ifelse(df$clust == 3, inanspruch, NA)
  i4 <- ifelse(df$clust == 4, inanspruch, NA)
  i5 <- ifelse(df$clust == 5, inanspruch, NA)
  i6 <- ifelse(df$clust == 6, inanspruch, NA)
  
  #Inanspruchnahme als geskalte Variable
  i_1 <- ( table(i1)[1]/sum(table(i1)) ) * 5
  i_2 <- ( table(i2)[1]/sum(table(i2)) ) * 5
  i_3 <- ( table(i3)[1]/sum(table(i3)) ) * 5
  i_4 <- ( table(i4)[1]/sum(table(i4)) ) * 5
  i_5 <- ( table(i5)[1]/sum(table(i5)) ) * 5
  i_6 <- ( table(i6)[1]/sum(table(i6)) ) * 5
  
  #Bewertung des Nutzens und der Wunsch nach Maßnahmen
  means_nutzen <- getMeans(nutzen)
  means_wunsch <- getMeans(wunsch)
  
  #Finale Scores berechnen
  s1 <- (means_nutzen[1]) + (means_wunsch[1]) + (i_1)
  s2 <- (means_nutzen[2]) + (means_wunsch[2]) + (i_2)
  s3 <- (means_nutzen[3]) + (means_wunsch[3]) + (i_3)
  s4 <- (means_nutzen[4]) + (means_wunsch[4]) + (i_4)
  s5 <- (means_nutzen[5]) + (means_wunsch[5]) + (i_5)
  s6 <- (means_nutzen[6]) + (means_wunsch[6]) + (i_6)
  
  Result <- c(s1, s2, s3, s4, s5, s6)
  Result[is.na(Result)] <- 0
  
  return(Result)
}

# Calculate the scores for each measure per cluster
Sport <- calculateBGMScore(df$T2_BGM6Inanspruch1a, df$T2_BGM6Inanspruch1b, df$T2_BGM6Wunsch1c)
Stress <- calculateBGMScore(df$T2_BGM6Inanspruch2a, df$T2_BGM6Inanspruch2b, df$T2_BGM6Wunsch2c)
Ernährung <- calculateBGMScore(df$T2_BGM6Inanspruch3a, df$T2_BGM6Inanspruch3b, df$T2_BGM6Wunsch3c)
Sucht <- calculateBGMScore(df$T2_BGM6Inanspruch4a, df$T2_BGM6Inanspruch4b, df$T2_BGM6Wunsch4c)
Arbeitsunfälle <- calculateBGMScore(df$T2_BGM6Inanspruch5a, df$T2_BGM6Inanspruch5b, df$T2_BGM6Wunsch5c)
Atmosphäre <- calculateBGMScore(df$T2_BGM6Inanspruch6a, df$T2_BGM6Inanspruch6b, df$T2_BGM6Wunsch6c)
Weiterbildungen <- calculateBGMScore(df$T2_BGM6Inanspruch7a, df$T2_BGM6Inanspruch7b, df$T2_BGM6Wunsch7c)
Ergonomie <- calculateBGMScore(df$T2_BGM6Inanspruch8a, df$T2_BGM6Inanspruch8b, df$T2_BGM6Wunsch8c)
IKT <- calculateBGMScore(df$T2_BGM6Inanspruch9a, df$T2_BGM6Inanspruch9b, df$T2_BGM6Wunsch9c)
Technostress <- calculateBGMScore(df$T2_BGM6Inanspruch10a, df$T2_BGM6Inanspruch10b, df$T2_BGM6Wunsch10a)
Familie <- calculateBGMScore(df$T2_BGM6Inanspruch11a, df$T2_BGM6Inanspruch11b, df$T2_BGM6Wunsch11c)
Eltern <- calculateBGMScore(df$T2_BGM6Inanspruch12a, df$T2_BGM6Inanspruch12b, df$T2_BGM6Wunsch12c)
Kinderbetreuung <- calculateBGMScore(df$T2_BGM6Inanspruch13a, df$T2_BGM6Inanspruch13b, df$T2_BGM6Wunsch13c)
Frauen <- calculateBGMScore(df$T2_BGM6Inanspruch14a, df$T2_BGM6Inanspruch14b, df$T2_BGM6Wunsch14c)
WLB <- calculateBGMScore(df$T2_BGM6Inanspruch15a, df$T2_BGM6Inanspruch15b, df$T2_BGM6Wunsch15c)

# Create lists for each cluster per measure
Values1 <- c(Sport[1], Stress[1], Ernährung[1], Sucht[1], Arbeitsunfälle[1], Atmosphäre[1], Weiterbildungen[1],
             Ergonomie[1], IKT[1], Technostress[1], Familie[1], Eltern[1], Kinderbetreuung[1], Frauen[1], WLB[1])
Values2 <- c(Sport[2], Stress[2], Ernährung[2], Sucht[2], Arbeitsunfälle[2], Atmosphäre[2], Weiterbildungen[2],
             Ergonomie[2], IKT[2], Technostress[2], Familie[2], Eltern[2], Kinderbetreuung[2], Frauen[2], WLB[2])
Values3 <- c(Sport[3], Stress[3], Ernährung[3], Sucht[3], Arbeitsunfälle[3], Atmosphäre[3], Weiterbildungen[3],
             Ergonomie[3], IKT[3], Technostress[3], Familie[3], Eltern[3], Kinderbetreuung[3], Frauen[3], WLB[3])
Values4 <- c(Sport[4], Stress[4], Ernährung[4], Sucht[4], Arbeitsunfälle[4], Atmosphäre[4], Weiterbildungen[4],
             Ergonomie[4], IKT[4], Technostress[4], Familie[4], Eltern[4], Kinderbetreuung[4], Frauen[4], WLB[4])
Values5 <- c(Sport[5], Stress[5], Ernährung[5], Sucht[5], Arbeitsunfälle[5], Atmosphäre[5], Weiterbildungen[5],
             Ergonomie[5], IKT[5], Technostress[5], Familie[5], Eltern[5], Kinderbetreuung[5], Frauen[5], WLB[5])
Values6 <- c(Sport[6], Stress[6], Ernährung[6], Sucht[6], Arbeitsunfälle[6], Atmosphäre[6], Weiterbildungen[6],
             Ergonomie[6], IKT[6], Technostress[6], Familie[6], Eltern[6], Kinderbetreuung[6], Frauen[6], WLB[6])

# Names for each measure
Names <-  c("Sport", "Stress", "Ernaehrung", "Sucht", "Arbeitsunfaelle", "Atmosphaere", "Weiterbildungen",
            "Ergonomie", "IKT", "Technostress", "Familie", "Eltern", "Kinderbetreuung", "Frauen", "WLB")

# Create am empty ranking
Ranking <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Create dataframes for each cluster with the 
Cluster1 <- data.frame("Values" = Values1, Names, Ranking)
Cluster2 <- data.frame("Values" = Values2, Names, Ranking)
Cluster3 <- data.frame("Values" = Values3, Names, Ranking)
Cluster4 <- data.frame("Values" = Values4, Names, Ranking)
Cluster5 <- data.frame("Values" = Values5, Names, Ranking)
Cluster6 <- data.frame("Values" = Values6, Names, Ranking)

# Fuction to update the ranking for each cluster
rankMeasures <- function(dat)
{
  Mean <- mean(dat$Values)
  Min <- min(dat$Values)
  Max <- max(dat$Values)
  BStep <- (Max-Mean)/2
  Step <- BStep/4
  ReturnValue <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  for(x in 1:15)
  {
    Value <- 5
    ifelse(dat$Values[x] <= Max-Step, Value <- 4, Value)
    ifelse(dat$Values[x] <= Max-(Step*2), Value <- 3, Value)
    ifelse(dat$Values[x] <= Max-(Step*3), Value <- 2, Value)
    ifelse(dat$Values[x] <= Max-BStep, Value <- 1, Value)
    ifelse(dat$Values[x] <= Max-(BStep + (BStep/2)), Value <- 0, Value)
    ReturnValue[x] <- Value
  }
  
  return(ReturnValue)
}

# Update the rakings for each cluster and save the files
Cluster1$Ranking <- rankMeasures(Cluster1)
write.csv(Cluster1, file = "Cluster1.csv")
Cluster2$Ranking <- rankMeasures(Cluster2)
write.csv(Cluster2, file = "Cluster2.csv")
Cluster3$Ranking <- rankMeasures(Cluster3)
write.csv(Cluster3, file = "Cluster3.csv")
Cluster4$Ranking <- rankMeasures(Cluster4)
write.csv(Cluster4, file = "Cluster4.csv")
Cluster5$Ranking <- rankMeasures(Cluster5)
write.csv(Cluster5, file = "Cluster5.csv")
Cluster6$Ranking <- rankMeasures(Cluster6)
write.csv(Cluster6, file = "Cluster6.csv")

#### Cluster visualisations #### -----------------------------------------------
# Create a new dataframe for the visualisations
Clusterframe <- data.frame(nw.setting2, df$clust)

Value1 <- aggregate(scales::rescale(Clusterframe$df.Flex1_mean_z, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[1]

# Create and save a new garphic for each cluster
for(x in 1:6)
{
  # Get the values for each Dimension per cluster
  # The rescale is there to bring all variables on a scale from 1 to 10
  Value1 <- aggregate(scales::rescale(Clusterframe$df.Flex1_mean_z, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value2 <- aggregate(scales::rescale(Clusterframe$df.Flex1_mean_oe, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value3 <- aggregate(scales::rescale(Clusterframe$df.Flex1_mean_oe, c(0, 10), c(0, 7)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value4 <- aggregate(scales::rescale(Clusterframe$df.AGIL1_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value5 <- aggregate(scales::rescale(Clusterframe$df.AGIL2AgilMeth_r, c(0, 10), c(0, 4)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value6 <- aggregate(Clusterframe$df.Demo1OrgStr_mean, list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value7 <- aggregate(scales::rescale(Clusterframe$df.Demo4Autonomie_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value8 <- aggregate(scales::rescale(Clusterframe$df.Demo3Partizipation_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value9 <- aggregate(scales::rescale(Clusterframe$df.virtual_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value10 <- aggregate(scales::rescale(Clusterframe$df.Digi3IKT_Nutzung_Vernetzung_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Value11 <- aggregate(scales::rescale(Clusterframe$df.Digi3IKTAutomation_mean, c(0, 10), c(0, 5)), list(Clusterframe$df.clust), FUN=mean)$x[x]
  Values <- c(Value1, Value2, Value3, Value4, Value5, Value6, Value7, Value8, Value9, Value10, Value11)
  # The dataframe needs to be prepared in this way for the radarchart-Fuction
  # For each Value it needs c(Max, Min, Value)
  Name <- paste("Clusterframe_",x, sep = "")
  Clusterframe_ <- data.frame(c(10, 0, Value1), c(10, 0, Value2), c(10, 0, Value3),
                              c(10, 0, Value4), c(10, 0, Value5), c(10, 0, Value6),
                              c(10, 0, Value7), c(10, 0, Value8), c(10, 0, Value9),
                              c(10, 0, Value10), c(10, 0, Value11))
  colnames(Clusterframe_) <- c("Zeitliche Flexibilität", "Örtliche Flexibilität", "Erreichbarkeit",
                               "Pro-/Reaktivität", "Agile Methoden", "Organisationsstruktur",
                               "Autonomie", "Partizipation", "Virtuelle Zusammenarbeit",
                               "IKT Nutzung & Vernetzung", "Automation")
  library(fmsb)
  fmsb::radarchart(Clusterframe_)
  assign(Name, Clusterframe_)
  # Create a unique Name for each File in the form of Cluster(x).png
  String <- paste("Cluster_",x,".png", sep = "")
  png(file=String, width = 1500, height = 1500)
  radarchart(Clusterframe_,
             # Make the grid
             cglty = 1, cglcol = "black",
             # Make the polygon
             pcol = "orange", plwd = 2, pfcol = rgb(0.2,0.5,0.5,0.5), vlcex = 2, seg = 5,
             # Text Labels
             axistype = 1, caxislabels=seq(0,10,2), axislabcol = "black", calcex = 2)
  # This function is required to save the files correctly
  dev.off()
}
write.csv(Clusterframe_1, file = "Clusterframe_1.csv")
write.csv(Clusterframe_2, file = "Clusterframe_2.csv")
write.csv(Clusterframe_3, file = "Clusterframe_3.csv")
write.csv(Clusterframe_4, file = "Clusterframe_4.csv")
write.csv(Clusterframe_5, file = "Clusterframe_5.csv")
write.csv(Clusterframe_6, file = "Clusterframe_6.csv")

#### Random Forest Modell training #### ----------------------------------------
########################################################
# Preliminaries (Item-Scale Mapping)
########################################################

item_scale_mapping <- data.frame(
  item = c(
    "AGIL1Pro1", "AGIL1Pro2", "AGIL1Re",
    "AGIL2AgilMeth",
    "Demo1OrgStr1", "Demo1OrgStr2", "Demo1OrgStr3", "Demo1OrgStr4", "Demo1OrgStrB",
    "Demo3Parti1", "Demo3Parti2", "Demo3Parti3", "Demo3Parti4", "Demo3Parti5",
    "Demo4Auto1", "Demo4Auto2", "Demo4Auto3", "Demo4Auto4", "Demo4Auto5",
    "Demo4Auto6", "Demo4Auto7", "Demo4Auto8", "Demo4Auto9",
    "Digi1IKTHaeuf3", "Digi1IKTHaeuf4", "Digi1IKTHaeuf5", "Digi1IKTHaeuf6", "Digi1IKTHaeuf7",
    "Digi3IKTNutzung1", "Digi3IKTNutzung2", "Digi3IKTVernetzung",
    "Digi3IKTAutomation1", "Digi3IKTAutomation2", "Digi3IKTAutomation3", "Digi3IKTAutomation4",
    "Flex1ze1", "Flex1ze2", 
    "Flex2oe1a", "Flex2oe1b", "Flex2oe1c",
    "Flex4E1", "Flex4E2", "Flex4E3", "Flex4E4", "Flex4E5"
  ),
  scale = c(
    "AGIL1_mean", "AGIL1_mean", "AGIL1_mean",
    "AGIL2AgilMeth_r",
    "Demo1OrgStr_mean", "Demo1OrgStr_mean", "Demo1OrgStr_mean", "Demo1OrgStr_mean", "Demo1OrgStr_mean",
    "Demo3Partizipation_mean", "Demo3Partizipation_mean", "Demo3Partizipation_mean", "Demo3Partizipation_mean", "Demo3Partizipation_mean",
    "Demo4Autonomie_mean", "Demo4Autonomie_mean", "Demo4Autonomie_mean", "Demo4Autonomie_mean", "Demo4Autonomie_mean",
    "Demo4Autonomie_mean", "Demo4Autonomie_mean", "Demo4Autonomie_mean", "Demo4Autonomie_mean",
    "virtual_mean", "virtual_mean", "virtual_mean", "virtual_mean", "virtual_mean",
    "Digi3IKT_Nutzung_Vernetzung_mean", "Digi3IKT_Nutzung_Vernetzung_mean", "Digi3IKT_Nutzung_Vernetzung_mean",
    "Digi3IKTAutomation_mean", "Digi3IKTAutomation_mean", "Digi3IKTAutomation_mean", "Digi3IKTAutomation_mean",
    "Flex1_mean_z", "Flex1_mean_z", 
    "Flex1_mean_oe", "Flex1_mean_oe", "Flex1_mean_oe",
    "Flex4E_mean", "Flex4E_mean", "Flex4E_mean", "Flex4E_mean", "Flex4E_mean"
  )
)

########################################################
# 1)  Datenvorbereitung

# Cluster label extrahieren
df$clust <- as.factor(df$clust)

# Skalen-Mittelwerte festlegen, die vom RF genutzt werden dürfen
allowed_scales <- unique(item_scale_mapping$scale)

# Skalen-Mittelwerte aus Gesamtdatensatz extrahieren
mittelwert_vars <- allowed_scales[allowed_scales %in% colnames(df)]

# die einzelnen Items extrahieren
einzelitems <- item_scale_mapping$item

########################################################
# 2)  Datensatz in Trainings- und Testdaten aufteilen (80% für Training; 20% für Testung)

set.seed(123)
train_index <- createDataPartition(df$clust, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

########################################################
# 3)  Trainingsdaten aufbereiten + Zielvariablen angleichen

# z-Standardisierung der Cluster-Analyse übernehmen
mittelwert_vars <- c(
  "Flex1_mean_z", "Flex1_mean_oe", "Flex4E_mean",
  "AGIL1_mean", "AGIL2AgilMeth_r",
  "Demo1OrgStr_mean", "Demo4Autonomie_mean", "Demo3Partizipation_mean",
  "virtual_mean",
  "Digi3IKT_Nutzung_Vernetzung_mean", "Digi3IKTAutomation_mean"
)

# Einmalige Skalierung über ALLE Fälle in df (entspricht Variable nw.setting2_z aus Cluster-Analyse)
cluster_scaler <- scale(
  df[, mittelwert_vars],
  center = TRUE,
  scale  = TRUE
)

# Skalierung extrahieren und auf zukünftige Modelle anwenden
cluster_center <- attr(cluster_scaler, "scaled:center")
cluster_scale  <- attr(cluster_scaler, "scaled:scale")

# a) Modell 1: Mittelwerte
# Trainings-Daten mit den Parametern der z-Standardisierung transformieren
x_train_mean <- as.data.frame(
  scale(
    train_data[, mittelwert_vars],
    center = cluster_center,
    scale  = cluster_scale
  )
)

# Test-Daten ebenfalls mit den Parametern der z-Standardisierung transformieren
x_test_mean  <- as.data.frame(
  scale(
    test_data[, mittelwert_vars],
    center = cluster_center,
    scale  = cluster_scale
  )
)

# b) Modell 2: Einzelitems
# Trainings- und Testdaten werden ebenfalls z-standardisiert
x_train_items <- as.data.frame(scale(train_data[, einzelitems]))
x_test_items  <- as.data.frame(
  scale(test_data[, einzelitems],
        center = attr(scale(train_data[, einzelitems]), "scaled:center"),
        scale  = attr(scale(train_data[, einzelitems]), "scaled:scale"))
)

# Skalierung extrahieren und auf zukünftige Modelle anwenden
cluster_center_items <- attr(scale(train_data[, einzelitems]), "scaled:center")
cluster_scale_items  <- attr(scale(train_data[, einzelitems]), "scaled:scale")

# Datensatz erstellen, um ihn dann mit den Topitems abspeichern zu können
df_items <- data.frame(ShortFormItems = names(cluster_center_items), center = cluster_center_items, scale = cluster_scale_items)


# Prädiktoren und Zielvariablen für "RF-Modell 1: Mittelwerte" festlegen, dabei NA's entfernen
valid_train_mean  <- complete.cases(x_train_mean)
x_train_mean      <- x_train_mean[valid_train_mean, ]
y_train_mean      <- train_data$clust[valid_train_mean]

# Prädiktoren und Zielvariablen für "RF-Modell 2: Einzelitems" festlegen, dabei NA's entfernen
valid_train_items <- complete.cases(x_train_items)
x_train_items     <- x_train_items[valid_train_items, ]
y_train_items     <- train_data$clust[valid_train_items]

# Testdaten 
y_test <- test_data$clust

########################################################
# 4.1)  Modell 1: Mittelwerte

ctrl <- trainControl(method = "cv", number = 5)

set.seed(123)
model_mean  <- train(
  x = x_train_mean,
  y = as.factor(y_train_mean),
  method = "rf",
  trControl = ctrl,
  ntree = 500
)

########################################################
# 4.2)  Modell 2: Einzelitems

set.seed(123)
model_items <- train(
  x = x_train_items,
  y = as.factor(y_train_items),
  method = "rf",
  trControl = ctrl,
  ntree = 500
)

########################################################-
# 4.3)  Modell 3: Short Form

importance_vals  <- importance(model_items$finalModel)
top_items        <- names(sort(importance_vals[, "MeanDecreaseGini"], decreasing = TRUE))[1:45] # bei n=45 werden alle Einzelitems genutzt

# Excel-Export der Items, wobei die Items nach MeanDecreaseGini sortiert sind; die Auswahl erfolgt also von oben nach unten und 
# Hinzufügen von mean und sd, um auch die zukünfitgen Werte standardisieren zu können 
df_topitems <- data.frame(ShortFormItems = top_items)
df_topitems <- df_topitems %>% 
  left_join(df_items, by = "ShortFormItems")
write_xlsx(df_topitems, "short_form_items.xlsx")


# Short Form Daten
x_train_short <- x_train_items[, top_items, drop = FALSE]
x_test_short  <- x_test_items[, top_items, drop = FALSE]
y_train_short <- y_train_items

set.seed(123)
model_short <- train(
  x = x_train_short,
  y = as.factor(y_train_short),
  method = "rf",
  trControl = ctrl,
  ntree = 500
)


########################################################-
# 4.4)  Modell 4: Short Form - Mittelwerte
importance_mean  <- importance(model_mean$finalModel)
top_means        <- names(sort(importance_mean[, "MeanDecreaseGini"], decreasing = TRUE))[1:11] # bei n=11 werden alle Einzelitems genutzt

# Short Form Daten
x_train_short_mean <- x_train_mean[, top_means, drop = FALSE]
x_test_short_mean  <- x_test_mean[, top_means, drop = FALSE]
y_train_short_mean <- y_train_mean

set.seed(123)
model_short_mean <- train(
  x = x_train_short_mean,
  y = as.factor(y_train_short_mean),
  method = "rf",
  trControl = ctrl,
  ntree = 500
)



########################################################
# 5)  Modellvergleich auf Testdaten (inklusive NA-Bereinigung der Testdaten)

# Modell 1: Mittelwerte
valid_test_mean  <- complete.cases(x_test_mean)
x_test_mean     <- x_test_mean[valid_test_mean, ]
y_test_mean     <- y_test[valid_test_mean]

# Modell 2: Einzelitems
valid_test_items <- complete.cases(x_test_items)
x_test_items    <- x_test_items[valid_test_items, ]
y_test_items    <- y_test[valid_test_items]

# Modell 3: Short Form
valid_test_short <- complete.cases(x_test_short)
x_test_short     <- x_test_short[valid_test_short, ]
y_test_short     <- y_test[valid_test_short]

pred_mean  <- predict(model_mean,  x_test_mean)
pred_items <- predict(model_items, x_test_items)
pred_short <- predict(model_short, x_test_short)

conf_mean  <- confusionMatrix(pred_mean,  as.factor(y_test_mean))
conf_items <- confusionMatrix(pred_items, as.factor(y_test_items))
conf_short <- confusionMatrix(pred_short, as.factor(y_test_short))

########################################################
# 6)  Vergleichstabelle

results_compare <- tibble(
  Modell   = c("Mittelwerte", "Einzelitems", "Short Form"),
  Accuracy = c(conf_mean$overall["Accuracy"], conf_items$overall["Accuracy"], conf_short$overall["Accuracy"]),
  Kappa    = c(conf_mean$overall["Kappa"],    conf_items$overall["Kappa"],    conf_short$overall["Kappa"])
)
print(results_compare)


resamps <- resamples(list(RF_mean = model_mean, RF_items = model_items, RF_short = model_short))
summary(resamps)

########################################################
# 7.1)  Cross-Validation über Itemanzahlen für Short Form Performance

set.seed(123)
results <- data.frame(ItemAnzahl = integer(), Accuracy = numeric())
for (n_items in seq_along(top_items)) {
  current_items <- top_items[1:n_items]
  x_sub <- x_train_items[, current_items, drop = FALSE]
  y_sub <- as.factor(y_train_items)
  model_cv <- train(
    x = x_sub,
    y = y_sub,
    method = "rf",
    trControl = ctrl,
    ntree = 500,
    tuneLength = 1
  )
  acc <- max(model_cv$results$Accuracy)
  results <- rbind(results, data.frame(ItemAnzahl = n_items, Accuracy = acc))
}


########################################################
# 7.2)  Cross-Validation über Itemanzahlen für Short Form Performance Mittelwerte

set.seed(123)
results_means <- data.frame(ItemAnzahl = integer(), Accuracy = numeric())
for (n_means in seq_along(top_means)) {
  current_means <- top_means[1:n_means]
  x_sub <- x_train_mean[, current_means, drop = FALSE]
  y_sub <- as.factor(y_train_mean)
  model_cv <- train(
    x = x_sub,
    y = y_sub,
    method = "rf",
    trControl = ctrl,
    ntree = 500,
    tuneLength = 1
  )
  acc <- max(model_cv$results$Accuracy)
  results_means <- rbind(results_means, data.frame(ItemAnzahl = n_means, Accuracy = acc))
}

########################################################
# 8)  Optimale Anzahl an Items bestimmen

# 8.1)  Methode mit Fokus: Geringster Accuracy-Verlust (gekürzter Fragebogen erzielt 98% Accuracy des Referenz-Modells (Modell Mittelwerte))
max_acc <- max(results$Accuracy)
optimal_items_acc <- results %>%
  filter(Accuracy >= 0.98 * max_acc) %>%
  slice_min(ItemAnzahl)
# 20 Items, Accuracy: 0.81

# 8.2)  Methode mit Fokus: Länge möglichst gering (gekürzter Fragebogen erzielt 95% Accuracy des Referenz-Modells (Modell Mittelwerte))
max_acc <- max(results$Accuracy)
optimal_items_leng <- results %>%
  filter(Accuracy >= 0.95 * max_acc) %>%
  slice_min(ItemAnzahl)
# 16 Items, Accuracy: 0.78

# 8.3)  Graphische Methode anhand des Elbow-Prinzips (auf "Accuracy vs Item Anzahl" Graph diejenige Stelle, an der der Zuwachs der Accuracy pro zusätzlichem Item stark abflacht)

# Funktion, um Knee zu bestimmen, anhandet der Kneedle-Logik
find_kneedle <- function(x, y) {
  # 1. Auf [0,1] normieren
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  # 2. Abstandsfunktion s(i) = y_norm(i) - x_norm(i)
  s <- y_norm - x_norm
  
  # 3. Index des maximalen Abstands (Knee)
  knee_idx <- which.max(s)
  
  # Ergebnis zurückgeben
  list(
    index = knee_idx,
    x      = x[knee_idx],
    y      = y[knee_idx],
    s      = s
  )
}

knee <- find_kneedle(results$ItemAnzahl, results$Accuracy)

########################################################
# Plot: Accuracy vs Number of Items; wobei Knee rot eingezeichnet ist; Kneedle algorithm

plot_accuracy <- ggplot(results, aes(x = ItemAnzahl, y = Accuracy)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_point(
    data = results[knee$index, , drop = FALSE],
    aes(x = ItemAnzahl, y = Accuracy),
    color = "red", size = 3
  ) +
  annotate(
    "text",
    x = knee$x, y = knee$y,
    label = paste0("Knee\n(", knee$x, ", ", round(knee$y, 3), ")"),
    vjust = -1
  ) +
  theme_minimal() +
  labs(
    title = "Accuracy vs Number of Items",
    x = "Number of Items",
    y = "Cross-Validated Accuracy",)

print(plot_accuracy)
# 6 Items, Accuracy: 0.72

########################################################
# 9)  Ergebnisse des Modellvergleichs

# results Tabelle von oben; Modell 1: Mittelwerte fungiert als Referenz; ShortForm beinhaltet in dieser Tabelle bisher nicht die konkrete Anzahl der Items!
print(results_compare)

cat("Auswahlkriterium 1 = geringster Accuracy-Verlust; dann optimale Anzahl an Items:", optimal_items_acc$ItemAnzahl, "Accuracy:", optimal_items_acc$Accuracy)

cat("Auswahlkriterium 2 = geringste Länge; dann optimale Anzahl an Items:", optimal_items_leng$ItemAnzahl, "Accuracy:", optimal_items_leng$Accuracy)

cat("Auswahlkriterium 3 = Knee (graphisch; siehe auch ggplot); dann optimale Anzahl an Items", knee$x, "Accuracy:", round(knee$y, 4), "\n")

optimal_items_acc
optimal_items_leng
cbind(knee$index, knee$y)


########################################################
# 10) Auswahl geringster Accuracy-Verlust mit 20 Items:

# RF-Modell für die Klassifizierung trainieren: 

# Short Form Daten
x_train_short_20 <- x_train_items[, top_items[1:20], drop = FALSE]
x_test_short_20  <- x_test_items[, top_items[1:20], drop = FALSE]
y_train_short_20 <- y_train_items


# tuning parameter optimieren und für ntree = 500 und ntree = 1000 probieren:
tune_grid <- expand.grid(mtry = 1:20)

set.seed(123)
model_short_20 <- train(
  x = x_train_short_20,
  y = as.factor(y_train_short_20),
  method = "rf",
  trControl = ctrl,
  tuneGrid = tune_grid,
  ntree = 500
)

set.seed(123)
model_short_20.2 <- train(
  x = x_train_short_20,
  y = as.factor(y_train_short_20),
  method = "rf",
  trControl = ctrl,
  tuneGrid = tune_grid,
  ntree = 1000
)

model_short_20$results$Accuracy
model_short_20.2$results$Accuracy
# ntree = 500: Accuracy = 0.8024
# ntree = 1000: Accuracy = 0.8047
# ->  keine deutliche Verbesserung bei ntree = 1000, deswegen wird ntree = 500 gewählt

model_short_20$finalModel
plot(model_short_20)
# No. of variables tried at each split: 8 (mtry)

model_short_20.2$finalModel
plot(model_short_20.2)
# No. of variables tried at each split: 5 (mtry)

randomForest::varImpPlot(model_short_20$finalModel)

# Vorhersage auf Testdaten
pred_short_20 <- predict(model_short_20$finalModel, newdata = x_test_short_20)

conf_short_20 <- confusionMatrix(pred_short_20, as.factor(y_test_short))


#############################################################################
# Bestes Modell speichern
saveRDS(model_short_20$finalModel, "rf_model_cluster.rds")

r2pmml(rf_model, "rf_model.pmml")

# Modell laden 
#loaded_model <- readRDS("rf_model_cluster.rds")

Stringing <- "Führung"
Encoding(Stringing)
