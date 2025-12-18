# Erstellt von Felix Leitner, Anna-Maria Knieps am 04.07.25
# Für das Projekt BGM4NW der Universitätsmedizin Göttingen
# Dieser Code berechnet individuelle Variablen für jede Person
# install.packages("randomForest",repos = "http://cran.us.r-project.org")
# install.packages("RColorBrewer",repos = "http://cran.us.r-project.org")
# install.packages("dplyr",repos = "http://cran.us.r-project.org")
# install.packages("ggplot2",repos = "http://cran.us.r-project.org")
# install.packages("png",repos = "http://cran.us.r-project.org")
# install.packages("fmsb",repos = "http://cran.us.r-project.org")
library(randomForest)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(png)
library(fmsb)
#### Input fuer das System #### ------------------------------------------------
dat <- read.csv("input.csv") # Der allgemeine Dataframe mit allen Variablen
dat["lang"] <- NULL
df <- data.frame(read.csv("../../df.csv"))
df <- subset(df, select = -c(X))
CompetenciesFrame <- data.frame(read.csv("../../competenciesframe.csv")) # Kompetenzwerte für jede Person
CompetenciesFrame <- subset(CompetenciesFrame, select = -c(X))
CompetenceMeans <- data.frame(read.csv("../../competencemeans.csv")) # Mittelwerte der Kompetenzen nach Cluster
CompetenceMeans <- subset(CompetenceMeans, select = -c(X))
DeviationMeans  <- data.frame(read.csv("../../deviationmeans.csv")) # Standardabweichungen der Kompetenzen nach Cluster
DeviationMeans <- subset(DeviationMeans, select = -c(X))
short_form_items  <- data.frame(read.csv("../../short_form_items.csv")) # Werte der gekürzten Skala 
short_form_items <- subset(short_form_items, select = -c(X))
Cluster1  <- data.frame(read.csv("../../cluster1.csv"))
Cluster1 <- subset(Cluster1, select = -c(X))
Cluster2 <- data.frame(read.csv("../../cluster2.csv"))
Cluster2 <- subset(Cluster2, select = -c(X))
Cluster3 <- data.frame(read.csv("../../cluster3.csv"))
Cluster3 <- subset(Cluster3, select = -c(X))
Cluster4 <- data.frame(read.csv("../../cluster4.csv"))
Cluster4 <- subset(Cluster4, select = -c(X))
Cluster5 <- data.frame(read.csv("../../cluster5.csv"))
Cluster5 <- subset(Cluster5, select = -c(X))
Cluster6 <- data.frame(read.csv("../../cluster6.csv"))
Cluster6 <- subset(Cluster6, select = -c(X))
Clusterframe_1 <- data.frame(read.csv("../../clusterframe_1.csv"))
Clusterframe_1 <- subset(Clusterframe_1, select = -c(X))
Clusterframe_2 <- data.frame(read.csv("../../clusterframe_2.csv"))
Clusterframe_2 <- subset(Clusterframe_2, select = -c(X))
Clusterframe_3 <- data.frame(read.csv("../../clusterframe_3.csv"))
Clusterframe_3 <- subset(Clusterframe_3, select = -c(X))
Clusterframe_4 <- data.frame(read.csv("../../clusterframe_4.csv"))
Clusterframe_4 <- subset(Clusterframe_4, select = -c(X))
Clusterframe_5 <- data.frame(read.csv("../../clusterframe_5.csv"))
Clusterframe_5 <- subset(Clusterframe_5, select = -c(X))
Clusterframe_6 <- data.frame(read.csv("../../clusterframe_6.csv"))
Clusterframe_6 <- subset(Clusterframe_6, select = -c(X))

#### Erstellen des NW-Profiles für eine Person #### --------------------------------
#Output.Variable  Name
#Cluster 01 ZeitlFlexibilitaet
ZeitlFlexibilitaet <- mean(dat$Flex1ze1, dat$Flex1ze2)
#Cluster 02 OertlicheFlexibiltaet
OertlicheFlexibiltaet <- mean(dat$Flex2oe1a, dat$Flex2oe1b, dat$Flex2oe1c)
#Cluster 03 Erreichbarkeit
Erreichbarkeit <- mean(dat$Flex4E1, dat$Flex4E2, dat$Flex4E3, dat$Flex4E4, dat$Flex4E5)
#Cluster 04 ProReaktivitaet
ProReaktivitaet <- mean(dat$AGIL1Pro1, dat$AGIL1Pro2, dat$AGIL1Re)
#Cluster 05 AgileMethoden
AgileMethoden <- ifelse(is.na(dat$AGIL2AgilMeth_scale), 1, dat$AGIL2AgilMeth_scale) 
#Cluster 06 Organisationsstruktur
Organisationsstruktur <- mean(dat$Demo1OrgStr1, dat$Demo1OrgStr2, dat$Demo1OrgStr3, dat$Demo1OrgStr4, dat$Demo1OrgStrB)
#Cluster 07 Autonomie
Autonomie <- mean(dat$Demo4Auto1, dat$Demo4Auto2, dat$Demo4Auto3, dat$Demo4Auto4, dat$Demo4Auto5, dat$Demo4Auto6, dat$Demo4Auto7, dat$Demo4Auto8, dat$Demo4Auto9)
#Cluster 08 Partizipation
Partizipation <- mean(dat$Demo3Parti1, dat$Demo3Parti2, dat$Demo3Parti3, dat$Demo3Parti4, dat$Demo3Parti5)
#Cluster 09 VirtuelleZusammenarbeit
VirtuelleZusammenarbeit <- mean(dat$Digi1IKTHaeuf3, dat$Digi1IKTHaeuf4, dat$Digi1IKTHaeuf5, dat$Digi1IKTHaeuf6, dat$Digi1IKTHaeuf7)
#Cluster 10 IktNutzungVernetzung
IktNutzungVernetzung <- mean(dat$Digi3IKTNutzung1, dat$Digi3IKTNutzung2, dat$Digi3IKTVernetzung)
#Cluster 11 Automation
Automation <- mean(dat$Digi3IKTAutomation1, dat$Digi3IKTAutomation2, dat$Digi3IKTAutomation3, dat$Digi3IKTAutomation4)

# Laden des Random-Forest-Modells
#rf_model <- readRDS("../../rf_model_cluster.rds")
# Variablen umbenennen (bei allen Variablennamen Endung "-0" entfernen)
names(dat) <- gsub(".0$", "", names(dat))  

# Variablen umkodieren
dat <- dat %>%
  # in Datensatz geht es von "0" los, muss aber von "1" losgehen (bei allen Variablen außer den unten genannten)
  mutate(across(.cols = -all_of(c("id", "Geschlecht", "Alter", "Beruf_Stellung", "Branche")), .fns = ~ . + 1)) %>% 
  # 5 Fragen bei den Kompetenzen müssen rekodiert werden für die Berechnung d. Mittelwerte
  mutate(across(c(T2_KR5HolSC_A4, T2_KR5HolSC_A5, KR4Res2, KR4Res4, KR4Res6), ~ 6 - .)) %>% 
  # eine Frage muss bei Wert 5 (Ich weiß es nicht.) auch den Wert 1 haben
  mutate(AGIL2AgilMeth = ifelse(AGIL2AgilMeth == 5, 1, AGIL2AgilMeth))

# Mittelwerte der Kompetenzen berechnen
dat <- dat %>% 
  mutate(AGK_Planung = rowMeans(across(T2_KR1AGK1:T2_KR1AGK5))) %>% 
  mutate(AGK_Selbstmotivierung = rowMeans(across(T2_KR1AGK6:T2_KR1AGK8))) %>% 
  mutate(AGK_Stressvermeidung = rowMeans(across(T2_KR1AGK9:T2_KR1AGK11))) %>% 
  mutate(PsyEmp_Bedeutsamkeit = rowMeans(across(T2_KR2PsyEmp1:T2_KR2PsyEmp3))) %>%
  mutate(PsyEmp_Einfluss = rowMeans(across(T2_KR2PsyEmp10:T2_KR2PsyEmp12))) %>%
  mutate(PsyEmp_Kompetenz = rowMeans(across(T2_KR2PsyEmp4:T2_KR2PsyEmp6))) %>%
  mutate(PsyEmp_Selbstbestimmung = rowMeans(across(T2_KR2PsyEmp7:T2_KR2PsyEmp9))) %>%
  mutate(DigKomp_Verarbeitung = rowMeans(across(T2_KR4DigiKomp1:T2_KR4DigiKomp3))) %>%
  mutate(DigKomp_Kommunikation = rowMeans(across(T2_KR4DigiKomp4:T2_KR4DigiKomp5))) %>%
  mutate(DigKomp_Erstellung = rowMeans(across(T2_KR4DigiKomp6:T2_KR4DigiKomp8))) %>%
  mutate(DigKomp_Sicherheit = rowMeans(across(T2_KR4DigiKomp9:T2_KR4DigiKomp11))) %>%
  mutate(DigKomp_Problemloesung = rowMeans(across(T2_KR4DigiKomp12:T2_KR4DigiKomp15))) %>%
  mutate(HoL_Achtsamkeit = rowMeans(across(T2_KR5HolSC_A1:T2_KR5HolSC_A6))) %>%
  mutate(HoL_Verhalten = rowMeans(across(T2_KR5HolSC_V1:T2_KR5HolSC_V2))) %>%
  mutate(Resilienz = rowMeans(across(KR4Res1:KR4Res6))) %>% 
  mutate(GesKomp = rowMeans(across(T2_KR3GesKomp1:T2_KR3GesKomp6)))


#Kompetenzen Vergleichswerte
#AGK_Planung ZeitlFlex
AGK_Planung_values <- ifelse(df$Flex1_mean_z <= ZeitlFlexibilitaet + 1 & df$Flex1_mean_z >= ZeitlFlexibilitaet - 1, df$KR1AGK_Planung_mean, NA)
#AGK_Planung_values <- ifelse(df$Flex1_mean_z >= ZeitlFlexibilitaet - 1, df$KR1AGK_Planung_mean, NA)
AGK_Planung_v <- mean(AGK_Planung_values, na.rm = TRUE)
print(AGK_Planung_v)
#print(df$Flex1_mean_z)

# Nötige Variablen auswählen
#items_clust <- dat[, short_form_items$items]
# Items standardisieren
#new_data_z <- scale(items_clust, center = short_form_items$center, scale = short_form_items$scale)

# Zuweisung der Person zu den Clustern via Random-Forest-Modelle
#dat$cluster <- predict(rf_model, newdata = new_data_z)

# updateRanking - Funktion für die individuelle Reihung der Maßnahmen für eine Person
updateRanking <- function(number, cl)
{
  # Input, number - sind die Maßnahmen für die eine Person; cl - Cluster, in dem die Person ist
  # "Bonuspunkte" für Maßnahmen, welche bei besonders hohen/niedrigen Kompetenzen vergeben werden
  Bonus_AGK <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
  Bonus_PsyEmp <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  Bonus_DigKomp <- c(0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0)
  Bonus_Care <- c(0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Bonus_Res <- c(0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  Bonus_GesKomp <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Bonus_Kinder <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, -10, -10, -10, 0, 0, 0)
  Testperson <- number
  rank <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Multiplicator <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  c <- cl

  # Durchgehen aller Maßnahmen und vergeben der Punkte
  for(x in 1:16)
  {
    LowB <- CompetenceMeans[,x][c] - DeviationMeans[,x][c]
    HighB <- CompetenceMeans[,x][c] + DeviationMeans[,x][c]
    ifelse(Testperson[x] <= LowB, Multiplicator[x] <- -1, 0)
    ifelse(Testperson[x] >= HighB, Multiplicator[x] <- 1, 0)
  }
  AGK <- Multiplicator[1] + Multiplicator[2] + Multiplicator[3]
  if(AGK > 0)
  {
    rank <- rank - Bonus_AGK
  }
  if(AGK < 0)
  {
    rank <- rank + Bonus_AGK
  }
  PsyEmp <- Multiplicator[4] + Multiplicator[5] + Multiplicator[6] + Multiplicator[7]
  if(PsyEmp > 1)
  {
    rank <- rank - Bonus_PsyEmp
  }
  if(PsyEmp < 1)
  {
    rank <- rank + Bonus_PsyEmp
  }
  DigKomp <- Multiplicator[8] + Multiplicator[9] + Multiplicator[10] + Multiplicator[11] + Multiplicator[12]
  if(DigKomp > 1)
  {
    rank <- rank - Bonus_DigKomp
  }
  if(DigKomp < 1)
  {
    rank <- rank + Bonus_DigKomp
  }
  Care <- Multiplicator[13] + Multiplicator[14]
  if(Care > 0)
  {
    rank <- rank - Bonus_Care
  }
  if(Care < 0)
  {
    rank <- rank + Bonus_Care
  }
  Res <- Multiplicator[15]
  if(Res > 0)
  {
    rank <- rank - Bonus_Res
  }
  if(Res < 0)
  {
    rank <- rank + Bonus_Res
  }
  GesKomp <- Multiplicator[16]
  if(GesKomp > 0)
  {
    rank <- rank - Bonus_GesKomp
  }
  if(GesKomp < 0)
  {
    rank <- rank + Bonus_GesKomp
  }

  # Falls die Person keine Frau ist, Maßnahmen der Frauenförderung ganz nach hinten
  if(dat$Geschlecht != 0)
  {
    rank[13] <- -10
  }
  if(dat$Kinder == 1)
  {
    rank <- rank + Bonus_Kinder
  }
  return(rank)
}

setCluster <- function(variable)
{
  ReturnVar <- Cluster1
  if (variable == 2)
  {
    ReturnVar <- Cluster2
  }
  if (variable == 3)
  {
    ReturnVar <- Cluster3
  }
  if (variable == 4)
  {
    ReturnVar <- Cluster4
  }
  if (variable == 5)
  {
    ReturnVar <- Cluster5
  }
  if (variable == 6)
  {
    ReturnVar <- Cluster6
  }
  return(ReturnVar)
}

# Ranking für die Person im passenden Cluster
Person <- setCluster(dat$cluster)
Testperson <- dat %>% dplyr::select(AGK_Planung:GesKomp) # Auswahl der Variablen für das Ranking
RankingUpdates <- updateRanking(Testperson, dat$cluster) # Individualisierung des Rankings
Person$Ranking <- Person$Ranking + RankingUpdates
Person <- Person[order(Person$Ranking, decreasing=TRUE),] # Ordnung des Rankings

# Informationen zu den Kompetenzen einfügen
Competencies <- c("Planungskompetenz", "Selbstmotivierungskompetenz", "Stressvermeidungskompetenz",
                  "Bedeutsamkeit d. Arbeit", "Einfluss", "pers. Kompetenz", "Selbstbestimmung",
                  "Datenverarbeitung", "Kommunikation/Kooperation", "Erstellung von Inhalten", "Digitale Sicherheit", "Digitale Problemloesung",
                  "Gesundheitsbezogene Achtsamkeit", "Gesundheitsbezogenes Verhalten",
                  "Resilienz", "Gesundheitskompetenz")

Number <- c(16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

IndividualCompetencies <- c(dat$AGK_Planung, dat$AGK_Selbstmotivierung, dat$AGK_Stressvermeidung, 
                            dat$PsyEmp_Bedeutsamkeit, dat$PsyEmp_Einfluss, dat$PsyEmp_Kompetenz,
                            dat$PsyEmp_Selbstbestimmung, dat$DigKomp_Verarbeitung, dat$DigKomp_Kommunikation,
                            dat$DigKomp_Erstellung, dat$DigKomp_Sicherheit, dat$DigKomp_Problemloesung, 
                            dat$HoL_Achtsamkeit, dat$HoL_Verhalten, dat$Resilienz, dat$GesKomp)

ClusterCompetencies <- as.vector(unlist(CompetenceMeans[dat$cluster, ])) # passende Means werden hier ausgewaehlt

# Für Visualisierung vorbereiten
VisualisationFrame <- data.frame(Number, IndividualCompetencies, ClusterCompetencies, Competencies)

# Visualisierung
# Ja nach ueber/unterdurchschnittlicher Kompetenz erhalten die Balken andere Farben
Colorfill <- c("black", "black", "black", "black", "black", "black", "black", "black",
               "black", "black", "black", "black", "black", "black", "black", "black")
for(x in 1:16)
{
  if(IndividualCompetencies[x] >= ClusterCompetencies[x])
  {
    Colorfill[x] <- "#31A354"
  }
  else
  {
    Colorfill[x] <- "#DE2D26" 
  }
}

plot <- ggplot(data=VisualisationFrame, aes(x=Number, y=IndividualCompetencies)) +
  geom_bar(stat="identity", color = "black", fill=Colorfill) +
  geom_segment(aes(x=0.5,y=CompetenceMeans[dat$cluster,16],xend=1.5,yend=CompetenceMeans[dat$cluster,16]), linewidth = 0.8) +
  geom_segment(aes(x=1.5,y=CompetenceMeans[dat$cluster,15],xend=2.5,yend=CompetenceMeans[dat$cluster,15]), linewidth = 0.8) +
  geom_segment(aes(x=2.5,y=CompetenceMeans[dat$cluster,14],xend=3.5,yend=CompetenceMeans[dat$cluster,14]), linewidth = 0.8) +
  geom_segment(aes(x=3.5,y=CompetenceMeans[dat$cluster,13],xend=4.5,yend=CompetenceMeans[dat$cluster,13]), linewidth = 0.8) +
  geom_segment(aes(x=4.5,y=CompetenceMeans[dat$cluster,12],xend=5.5,yend=CompetenceMeans[dat$cluster,12]), linewidth = 0.8) +
  geom_segment(aes(x=5.5,y=CompetenceMeans[dat$cluster,11],xend=6.5,yend=CompetenceMeans[dat$cluster,11]), linewidth = 0.8) +
  geom_segment(aes(x=6.5,y=CompetenceMeans[dat$cluster,10],xend=7.5,yend=CompetenceMeans[dat$cluster,10]), linewidth = 0.8) +
  geom_segment(aes(x=7.5,y=CompetenceMeans[dat$cluster,9],xend=8.5,yend=CompetenceMeans[dat$cluster,9]), linewidth = 0.8) +
  geom_segment(aes(x=8.5,y=CompetenceMeans[dat$cluster,8],xend=9.5,yend=CompetenceMeans[dat$cluster,8]), linewidth = 0.8) +
  geom_segment(aes(x=9.5,y=CompetenceMeans[dat$cluster,7],xend=10.5,yend=CompetenceMeans[dat$cluster,7]), linewidth = 0.8) +
  geom_segment(aes(x=10.5,y=CompetenceMeans[dat$cluster,6],xend=11.5,yend=CompetenceMeans[dat$cluster,6]), linewidth = 0.8) +
  geom_segment(aes(x=11.5,y=CompetenceMeans[dat$cluster,5],xend=12.5,yend=CompetenceMeans[dat$cluster,5]), linewidth = 0.8) +
  geom_segment(aes(x=12.5,y=CompetenceMeans[dat$cluster,4],xend=13.5,yend=CompetenceMeans[dat$cluster,4]), linewidth = 0.8) +
  geom_segment(aes(x=13.5,y=CompetenceMeans[dat$cluster,3],xend=14.5,yend=CompetenceMeans[dat$cluster,3]), linewidth = 0.8) +
  geom_segment(aes(x=14.5,y=CompetenceMeans[dat$cluster,2],xend=15.5,yend=CompetenceMeans[dat$cluster,2]), linewidth = 0.8) +
  geom_segment(aes(x=15.5,y=CompetenceMeans[dat$cluster,1],xend=16.5,yend=CompetenceMeans[dat$cluster,1]), linewidth = 0.8) +
  geom_text(aes(x = Number, 0.1, label = Competencies, hjust = "left")) +
  labs(x = "Kompetenzen", y = "Mittelwerte d. Kompetenzen") + 
  geom_text(aes(label = round(IndividualCompetencies, digits = 2)), hjust=-0.25) + theme_minimal()
plot + coord_flip()
ggsave('plot.png') # , width = 1000, height = 800, units = "px" 

# Erklaehrungen fuer die BGM-Maßnahmen
Texte <- c("Verhaltensprävention: Es gibt eine Vielzahl an Möglichkeiten für die Gestaltung eines aktiven Lebensstils. Sollten Sie in Ihrer Arbeit viel sitzen, so können Sie Ihr Muskel-Skelett-System entlasten, indem Sie regelmäßige Pausen machen, um kurz aufzustehen, oder sich zu strecken. Für die Steigerung der körperlichen Fitness empfiehlt die Deutsche Gesellschaft für Sportmedizin und Prävention, etwa 10.000 Schritte am Tag hinzulegen und 150 Minuten körperliche Aktivität pro Woche. Verhältnisprävention: Viele Arbeitgeber bieten Maßnahmen an, um Sie in der aktiven Lebensgestaltung zu unterstützen. Dazu gehören: Firmenläufe, Sportkurse nach der Arbeit oder auch die Förderung privater Sportangebote durch verbilligte Mitgliedschaften bei diversen Fitnessstudios oder anderen Dienstleistern wie dem Urban Sports Club.",
           "Verhaltensprävention: Ein wichtiger Aspekt, um Stress zu reduzieren, ist es, klare Grenzen zu wahren und Arbeit und Privates getrennt zu halten. Des Weiteren trägt ein ausgeglichener Lebensstil zur Stressreduktion bei. Arbeiten Sie beispielsweise vor dem Computer, so ist es sinnvoll, den Feierabend in der Natur zu verbringen oder diesen für sportliche Betätigung zu nutzen. Verhältnisprävention: Sollten Sie sich durch Ihr Arbeitsumfeld regelmäßig gestresst fühlen, informieren Sie sich darüber, welche Angebote Ihr Arbeitgeber für die Minderung oder die Vorsorge von Stress anbietet. Oft können Schulungen und Vorträge zur Arbeitsgestaltung dabei helfen, Arbeitszeit effizienter zu strukturieren und besser mit stressreichen Situationen umzugehen.",
           "Verhaltensprävention: Fühlen Sie sich nach dem Essen häufig energielos oder demotiviert, dann kann dies ein Hinweis darauf sein, dass Sie Ihren Ernährungsstil ändern sollten. Dafür ist es immer sinnvoll, sich professionell beraten zu lassen etwa durch Ärzt*innen, Ernährungsexpert*innen oder Personal Trainer. Im Allgemeinen ist es aber immer sinnvoll, darauf zu achten, proteinreiche, ausgewogene Mahlzeiten zu sich zu nehmen. Verhältnisprävention: Informieren Sie sich, ob Ihr Arbeitgeber Angebote für gesunde Ernährung anbietet. Viele Arbeitgeber bieten ausgewogenes Kantinenessen, Catering oder ähnliche Dienstleistungen an, um Sie vor Ort in ihrer Ernährung zu unterstützen. Falls Sie keine Möglichkeit haben, Angebote vor Ort zu nutzen, können auch Vorträge Sie dabei unterstützen, zuhause gesunde Gerichte vorzubereiten oder für stressreiche Tage vorzukochen.",
           "Verhaltensprävention: Mittlerweile gibt es eine große Bandbreite an Möglichkeiten, um sich über Sucht und mögliche präventive Maßnahmen zu informieren. Die beste Anlaufstelle dafür sind natürlich immer professionelle Quellen wie Ärzt*innen oder Beratungsstellen. Für einen ersten Überblick können sie sich aber auch online informieren. Verhältnisprävention: Wenn Sie gerade versuchen, mit dem Rauchen aufzuhören, weniger Alkohol zu trinken oder den Konsum von anderen Suchtmitteln zu verringern, erkundigen Sie sich, ob Ihr Arbeitsplatz Sie dabei unterstützen kann. Häufig werden Workshops oder Vorträge zu diesen Themen oder Hilfsprogramme angeboten, welche Ihnen helfen können, einen gesunden Lebensstil zu führen.",
           "Verhaltensprävention: Das eigene Verhalten am Arbeitsplatz hat entscheidenden Einfluss auf die eigene Sicherheit und die der Kolleginnen und Kollegen. Achten Sie deshalb darauf, die Sicherheitsregeln einzuhalten und die vorgeschriebene persönliche Schutzausrüstung zu tragen. Ein bewusster und vorsichtiger Umgang mit Maschinen, Werkzeugen und Gefahrstoffen trägt zur Vermeidung von Risiken bei. Verhältnisprävention: Fragen Sie bei Ihrem Arbeitgeber nach, wo kritische Situationen in Ihrer Arbeit auftreten können und welche Vorsorgen es dafür gibt. Schulungen und Vorträge sind dabei essentiell, um neue Kompetenzen zu entwickeln.",
           "Verhaltensprävention: Eine positive Arbeitsatmosphäre entsteht durch respektvolles und faires Miteinander. Kollegiale Unterstützung, freundliche Kommunikation und ein achtsamer Umgang miteinander tragen dazu bei, Spannungen zu vermeiden und das Team zu stärken. Auch kleine Gesten wie Anerkennung, Dank und Rücksichtnahme fördern das Wohlbefinden am Arbeitsplatz. Verhältnisprävention: Fragen Sie nach, ob Ihr Unternehmen Angebote für eine Verbesserung der Arbeitsatmosphäre anbietet. Dies kann gemeinsame Aktivitäten, Kurse für bessere Führung oder Workshops gegen Mobbing beinhalten.",
           "Verhaltensprävention: Lernen beginnt mit einer Einstellung, welche offen für Neues ist. Versuchen Sie, offen an Aufgaben heranzugehen und zu überlegen, in welchen Aspekten Sie noch etwas dazulernen können. Nutzen Sie auch Ihre Freizeit für Weiterbildungen. Es gibt mittlerweile ausreichend Angebote entweder online oder in Ihrer Umgebung durch staatliche Hochschulen und ähnliche Einrichtungen. Verhältnisprävention: Erkundigen Sie sich, ob Ihr Arbeitgeber Sie bei Ihren Fortbildungen unterstützt. Viele Arbeitgeber bieten Kurse, Seminare und andere Möglichkeiten, sich beruflich und privat weiterzubilden. Informieren Sie sich, welche Möglichkeiten bestehen und wie Sie diese in Ihre berufliche Laufbahn integrieren können.",
           "Verhaltensprävention: Gerade wenn Sie von Zuhause arbeiten, haben Sie selbst viel Gestaltungsspielraum über Ihren Arbeitsplatz. Informieren Sie sich ausreichend über ergonomische Anforderungen und gestalten Sie Ihren Arbeitsplatz auf eine gesunde Weise. Dazu gehört unter anderem eine gesunde Sitzposition mit ausreichend Stützung für den Nacken, genügend Abstand zu Bildschirmen und eine angemessene Beleuchtung. Verhältnisprävention: Informieren Sie sich darüber, ob Ihr Arbeitgeber Sie bei der ergonomischen Ausstattung Ihres Arbeitsplatzes unterstützen kann. Dazu gehören Gefährdungsevaluationen, welche feststellen können, ob Ihr Arbeitsplatz richtig ausgestattet ist, aber auch die Anschaffung von entsprechenden Möbeln, um langfristig Ihre Gesundheit an Ihrem Arbeitsplatz zu erhalten.",
           "Verhaltensprävention: Wichtig für einen gesunden Umgang mit Technologien ist es, auf sich selbst zu hören. Achten Sie auf einen bewussten Umgang und reagieren Sie, wenn Sie Belastungen bemerken. Dafür können Sie Ratgeber konsultieren, welche Ihnen helfen können, wenn Sie beispielsweise mit Entgrenzung oder Informationsüberlastung kämpfen. Verhältnisprävention: Wenn Sie das Gefühl haben, dass Sie durch diese Technologien regelmäßig Stress erfahren, erkundigen Sie sich nach Weiterbildungsmöglichkeiten, welche Ihnen helfen könnten, diese Technologien besser in Ihren Arbeitsalltag einzubinden. Viele Arbeitgeber haben Angebote, welche auf Ihre Tätigkeit abgestimmt sind und Ihnen helfen können",
           "Verhaltensprävention: Achten Sie darauf, mit neuen Technologien nachhaltig umzugehen. Das heißt, sich schon bevor diese eingeführt werden, damit auseinanderzusetzen, um so Ihre Gesundheit präemptiv zu schützen. Konstante Fortbildungen und eine reflektierte Herangehensweise für die eigene Arbeit können dies unterstützen. Verhältnisprävention: Wenn Sie sich durch neue Technologien an Ihrem Arbeitsplatz regelmäßig unter Druck gesetzt fühlen, fragen Sie in Ihrem Unternehmen nach, ob diese Ihnen entsprechende Schulungen oder Beratungen anbieten, welche Ihnen helfen können, Ihren Umgang mit diesen Technologien besser zu gestalten.",
           "Verhaltensprävention: Familienfreundliche Arbeitsmodelle unterstützen die Vereinbarkeit von Beruf und Privatleben und tragen zu einer höheren Zufriedenheit der Beschäftigten bei. Damit diese Modelle erfolgreich umgesetzt werden können, ist ein verantwortungsbewusstes Verhalten aller Beteiligten erforderlich. Dazu gehören die zuverlässige Planung und Einhaltung von Arbeitszeiten, eine offene Kommunikation bei Änderungswünschen sowie gegenseitige Rücksichtnahme im Team. Verhältnisprävention: Viele Unternehmen bieten familienfreundliche Arbeitsmodelle an. Fühlen Sie sich davon betroffen, erkundigen Sie sich bei Ihrem Unternehmen, ob dieses Sie dabei unterstützen kann. Dies kann unter anderem Angebote wie flexible Tagesarbeitszeit, flexible Wochenarbeitszeit, vorübergehende Stundenreduzierung, Arbeitszeitkonten, mobiler Arbeitsplatz, Jobsharing, oder Sabbaticals inkludieren.",
           "Verhaltensprävention: Eltern stehen häufig vor der Herausforderung, berufliche Aufgaben und familiäre Verantwortung miteinander zu vereinbaren. Ein bewusster Umgang mit diesen Anforderungen ist wichtig, um Belastungen zu reduzieren und Gesundheit sowie Leistungsfähigkeit zu erhalten. Verantwortungsvolles Planen und die offene Kommunikation mit Vorgesetzten und Kolleginnen und Kollegen können dazu beitragen, Stress zu vermeiden und die eigene Work-Life-Balance zu stärken. Verhältnisprävention: Erkundigen Sie sich, wie Ihr Unternehmen Eltern unterstützt. Dies kann Kontakthalteprogramme, Wiedereinstiegsprogramme, Weiterbildung in Elternzeit, Elternzeit über gesetzliches Maß, Elternzeit speziell für Väter und ähnliches beinhalten.",
           "Verhaltensprävention: Eine frühzeitige Organisation der Betreuung, transparente Absprachen mit dem Arbeitgeber sowie die offene Kommunikation im Team schaffen Klarheit und Planungssicherheit. Verhältnisprävention: Erkundigen Sie sich, ob Ihr Unternehmen dafür Unterstützung anbietet. Dies kann unter anderem Unterstützung bei der Betreuungsplatzsuche, finanzielle Unterstützung bei Kinderbetreuung, Notfallbetreuung, Mitnahme am Arbeitsplatz, Betreuung Schulferien, Betriebskindergarten, -krippe und weiteres inkludieren.",
           "Verhältnisprävention: Heutzutage bieten gewisse Unternehmen Förderungen an, um diesen Benachteiligungen systematisch entgegenzuwirken. Darunter zählen die gezielte Förderung des weiblichen Nachwuchses, gezielte Karriereplanung, Mentoringprogramme für Frauen, bevorzugte Stellenbesetzung mit Frauen, Quotierung und viele weitere. Erkundigen Sie sich darüber, ob Ihr Arbeitgeber diese anbietet und ob Sie von diesen profitieren könnten.",
           "Verhaltensprävention: Eine klare Grenzsetzung beginnt bei Ihnen. Setzen Sie eine klare Trennung zwischen Ihrer Arbeits- und Privatzeit. Deaktivieren Sie Notifikationen und nehmen Sie keine Anrufe außerhalb dieser Zeit an. Falls Sie im Homeoffice arbeiten, versuchen Sie, Ihre Arbeit von Ihrem Privatleben abzugrenzen, indem Sie sich einen klaren Arbeitsplatz einrichten, welchen Sie nicht für private Zwecke nutzen. Verhältnisprävention: Auch ihr Arbeitgeber kann Ihnen bei der Grenzsetzung helfen. Nutzen Sie einen Dienstrechner oder ein Diensttelefon, um Ihre privaten Geräte von Ihrer Arbeit abzutrennen. Schulungen oder Vorträge können Ihnen auch helfen, Kompetenzen aufzubauen und so eine nachhaltig gesunde Arbeitsgestaltung zu ermöglichen.")
Names <-  c("Bewegungsförderliche Maßnahmen", "Stressreduzierende Maßnahmen", "Maßnahmen für eine gesunde Ernährung", "Suchpräventive Maßnahmen", "Vermeidung von Arbeitsunfällen", "Verbesserung der Arbeitsatmosphäre", "Weiterbildungen",
            "Verbesserung der Ergonomie", "Umgang mit Informations- und Kommunikationstechnologien", "Umgang mit Technostress", "Familienfreundliche Arbeitszeitmodelle", "Maßnahmen für Eltern", "Kinderbetreuung", "Maßnahmen der Frauenförderung", "Verbesserung der Work-Life-Balance")
Names <-  c("Sport", "Stress", "Ernährung", "Sucht", "Arbeitsunfälle", "Atmosphäre", "Weiterbildungen",
            "Ergonomie", "IKT", "Technostress", "Familie", "Eltern", "Kinderbetreuung", "Frauen", "WLB")
#stri_enc_toutf8(Texte, is_unknown_8bit = FALSE, validate = FALSE)
BGM_Texts <- data.frame(Names, Texte)

# Fuer den Output, nach den fuenf hoechstgerankten Maßnahmen die Texte auswaehlen
BGM1 <- BGM_Texts[which(BGM_Texts$Names == Person$Names[1]),]$Texte
BGM2 <- BGM_Texts[which(BGM_Texts$Names == Person$Names[2]),]$Texte
BGM3 <- BGM_Texts[which(BGM_Texts$Names == Person$Names[3]),]$Texte
BGM4 <- BGM_Texts[which(BGM_Texts$Names == Person$Names[4]),]$Texte
BGM5 <- BGM_Texts[which(BGM_Texts$Names == Person$Names[5]),]$Texte
#BGM_Texts[which(BGM_Texts$Names == Person$Names[2]),]
#print(BGM_Texts$Names)
#print(Person$Names[1])
# Fuer den Output, erfassen, welche Kompetenzen ueber/unterdurchschnittlich sind
Low_High <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
for(x in 1:16)
{
  LowB <- CompetenceMeans[,x][dat$cluster] - DeviationMeans[,x][dat$cluster]
  HighB <- CompetenceMeans[,x][dat$cluster] + DeviationMeans[,x][dat$cluster]
  ifelse(IndividualCompetencies[x] <= LowB, Low_High[x] <- -1, 0)
  ifelse(IndividualCompetencies[x] >= HighB, Low_High[x] <- 1, 0)
}

getComp <- function(x)
{
  Comp <- "ist durchschnittlich"
  ifelse(x == 1, Comp <- "ist ueberdurchschnittlich", Comp <- Comp)
  ifelse(x == -1, Comp <- "ist unterdurchschnittlich", Comp <- Comp)
  return(Comp);
}

Comp_1 <- getComp(Low_High[1])
Comp_2 <- getComp(Low_High[2])
Comp_3 <- getComp(Low_High[3])
Comp_4 <- getComp(Low_High[4])
Comp_5 <- getComp(Low_High[5])
Comp_6 <- getComp(Low_High[6])
Comp_7 <- getComp(Low_High[7])
Comp_8 <- getComp(Low_High[8])
Comp_9 <- getComp(Low_High[9])
Comp_10 <- getComp(Low_High[10])
Comp_11 <- getComp(Low_High[11])
Comp_12 <- getComp(Low_High[12])
Comp_13 <- getComp(Low_High[13])
Comp_14 <- getComp(Low_High[14])
Comp_15 <- getComp(Low_High[15])
Comp_16 <- getComp(Low_High[16])

# Cluster-Texte
#Cluster_Text <- c("Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil) mit einer hohen Arbeitszufriedenheit. Ihre Gruppe zeichnet sich eher durch feste, geregelte Arbeitszeiten aus und dadurch besteht auch keine Notwendigkeit, ständig erreichbar zu sein. Sie arbeiten eher vor Ort mit Ihren Kollegen zusammen und weniger virtuell.",
#                  "Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil) mit einer geringeren Arbeitszufriedenheit. Ihre Gruppe zeichnet sich eher durch feste, geregelte Arbeitszeiten aus und dadurch besteht auch keine Notwendigkeit, ständig erreichbar zu sein. Sie arbeiten eher vor Ort mit Ihren Kollegen zusammen und weniger virtuell. Agilität und Digitalisierung spielen in Ihrem Arbeitskontext keine bedeutende Rolle.",
#                  "Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil)  ",
#                  "Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil) mit einer hohen Arbeitszufriedenheit und einem hohen Arbeitsengagement. Sie haben einen hohen Grad an Autonomie bei Ihrer Arbeit und können diese zeitlich flexibel gestalten. Allerdings besteht in Ihrer Gruppe auch eine hohe Angst vor Arbeitsplatzverlust und auch die persönliche Belastung ist in Ihrer Gruppe höher.",
#                  "Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil) mit dem höchsten Arbeitsengagement und einer hohen Arbeitszufriedenheit. Sie haben einen hohen Grad an Autonomie bei Ihrer Arbeit und können diese zeitlich flexibel und selbstbestimmt gestalten.", 
#                  "Sie gehören zu der Gruppe (Personen mit ähnlichem Arbeitsprofil) mit der höchsten Arbeitszufriedenheit und einem hohen Arbeitsengagement. Allerdings besteht in Ihrer Gruppe auch die höchste Angst vor Arbeitsplatzverlust und auch die persönliche Belastung ist in Ihrer Gruppe am höchsten. Allgemein zeigt sich, dass in Ihrem Arbeitskontext alle New Work Settings (Digitalisierung, Flexibilisierung, Agilität und Demokratisierung) eine bedeutende Rolle spielen.")
#Cluster = Cluster_Text[dat$cluster],
#String <- paste("../../Cluster_", dat$cluster, ".png", sep = "")
Clusterframe_ <- Clusterframe_1
if(dat$cluster == 2) { Clusterframe_ <- Clusterframe_2 }
if(dat$cluster == 3) { Clusterframe_ <- Clusterframe_3 }
if(dat$cluster == 4) { Clusterframe_ <- Clusterframe_4 }
if(dat$cluster == 5) { Clusterframe_ <- Clusterframe_5 }
if(dat$cluster == 6) { Clusterframe_ <- Clusterframe_6 }
#test
#String <- "Führung"
#Encoding(String)
#install.packages("stringi")
#library(stringi)
#stringi::stri_encode(String, from = NULL, to = "UTF-8")
#Encoding(String)
#print(String)
#ClusterPlot <- readPNG(String)
colnames(Clusterframe_) <- c("Zeitl Flexibiliät", "Oertliche_Flex", "Erreichbarkeit",
                             "Pro-/Reaktivitaet", "Agile Methoden", "Organisationsstruktur",
                             "Autonomie", "Partizipation", "Virtuelle Zusammenarbeit",
                             "IKT Nutzung & Vernetzung", "Automation")
String <- paste("ClusterPlot.png", sep = "")
png(file=String, width = 500, height = 500)
#radarchart(Clusterframe_,
#           # Make the grid
#           cglty = 1, cglcol = "black",
#           # Make the polygon
#           pcol = "orange", plwd = 2, pfcol = rgb(0.2,0.5,0.5,0.5), vlcex = 2, seg = 5,
#           # Text Labels
#           axistype = 1, caxislabels=seq(0,10,2), axislabcol = "black", calcex = 2)
# This function is required to save the files correctly
fmsb::radarchart(Clusterframe_)
dev.off()

# Output weitergeben
result <- data.frame(Bgm1 = BGM1,
                     Bgm2 = BGM2,
                     Bgm3 = BGM3,
                     Bgm4 = BGM4,
                     Bgm5 = BGM5,                     
                     Name1 = Person$Names[1],
                     Name2 = Person$Names[2],
                     Name3 = Person$Names[3],
                     Name4 = Person$Names[4],
                     Name5 = Person$Names[5],
                     Comp1 = Comp_1,
                     Comp2 = Comp_2,
                     Comp3 = Comp_3,
                     Comp4 = Comp_4,
                     Comp5 = Comp_5,
                     Comp6 = Comp_6,
                     Comp7 = Comp_7,
                     Comp8 = Comp_8,
                     Comp9 = Comp_9,
                     Comp10 = Comp_10,
                     Comp11 = Comp_11,
                     Comp12 = Comp_12,
                     Comp13 = Comp_13,
                     Comp14 = Comp_14,
                     Comp15 = Comp_15,
                     Comp16 = Comp_16,
                     CompVal1 = IndividualCompetencies[1]/5,
                     CompVal2 = IndividualCompetencies[2]/5,
                     CompVal3 = IndividualCompetencies[3]/5,
                     CompVal4 = IndividualCompetencies[4]/5,
                     CompVal5 = IndividualCompetencies[5]/5,
                     CompVal6 = IndividualCompetencies[6]/5,
                     CompVal7 = IndividualCompetencies[7]/5,
                     CompVal8 = IndividualCompetencies[8]/5,
                     CompVal9 = IndividualCompetencies[9]/5,
                     CompVal10 = IndividualCompetencies[10]/5,
                     CompVal11 = IndividualCompetencies[11]/5,
                     CompVal12 = IndividualCompetencies[12]/5,
                     CompVal13 = IndividualCompetencies[13]/5,
                     CompVal14 = IndividualCompetencies[14]/5,
                     CompVal15 = IndividualCompetencies[15]/5,
                     CompVal16 = IndividualCompetencies[16]/5,
                     RefVal1 = CompetenceMeans[dat$cluster,1]/5,
                     RefVal2 = CompetenceMeans[dat$cluster,2]/5,
                     RefVal3 = CompetenceMeans[dat$cluster,3]/5,
                     RefVal4 = CompetenceMeans[dat$cluster,4]/5,
                     RefVal5 = CompetenceMeans[dat$cluster,5]/5,
                     RefVal6 = CompetenceMeans[dat$cluster,6]/5,
                     RefVal7 = CompetenceMeans[dat$cluster,7]/5,
                     RefVal8 = CompetenceMeans[dat$cluster,8]/5,
                     RefVal9 = CompetenceMeans[dat$cluster,9]/5,
                     RefVal10 = CompetenceMeans[dat$cluster,10]/5,
                     RefVal11 = CompetenceMeans[dat$cluster,11]/5,
                     RefVal12 = CompetenceMeans[dat$cluster,12]/5,
                     RefVal13 = CompetenceMeans[dat$cluster,13]/5,
                     RefVal14 = CompetenceMeans[dat$cluster,14]/5,
                     RefVal15 = CompetenceMeans[dat$cluster,15]/5,
                     RefVal16 = CompetenceMeans[dat$cluster,16]/5,
                     Cluster1 = Clusterframe_[3,1],
                     Cluster2 = Clusterframe_[3,2],
                     Cluster3 = Clusterframe_[3,3],
                     Cluster4 = Clusterframe_[3,4],
                     Cluster5 = Clusterframe_[3,5],
                     Cluster6 = Clusterframe_[3,6],
                     Cluster7 = Clusterframe_[3,7],
                     Cluster8 = Clusterframe_[3,8],
                     Cluster9 = Clusterframe_[3,9],
                     Cluster10 = Clusterframe_[[10]][3],
                     Cluster11 = Clusterframe_[[11]][3])
exportJSON <- toJSON(result)
write(exportJSON, "output.json")