 #  #  #  # LIBRARIES #  #  #  #
knitr::opts_chunk$set(echo = TRUE)
library("dplyr")
library("ggplot2")
library("sf")
library("rpart")
library("ggcorrplot")
library("DALEX")
library("rpart.plot")
library("spdep")
library("car")
library("GWmodel")
library("rgdal")
library("lmtest")
#install.packages("fields")
library("fields")
library("effects")
library("readxl")

#  #  #  # IMPORT #  #  #  #
baltimore <- read_xlsx("C:/R/Econometrie spatiale/Baltimore/baltimore/baltim.xlsx")
map <- read_sf("C:/R/Econometrie spatiale/Baltimore/baltimore/baltim.shp")
map2 <- readOGR(dsn = "C:/R/Econometrie spatiale/Baltimore/baltimore", layer = "baltim")

#valeurs manquantes

table(is.na(baltimore))
sum(is.na(baltimore))

#  #  #  # ANALYSE DESCRIPTIVE #  #  #  #
balt <- subset(baltimore, select=-c(STATION))

ggplot(baltimore) +
  geom_point(aes(x = AGE, y = PRICE))

#1
ggplot(balt) + geom_density(aes(x = PRICE))
#2
ggplot(balt) +
  geom_point(aes(x = LOTSZ, y = PRICE, color = as.factor(DWELL))) +
  ggtitle("Variation du prix avec la taille du terrain")
#3
ggplot(data=balt, aes(x = as.factor(NROOM), y = PRICE)) +
  geom_boxplot() +
  geom_jitter(col="steelblue",alpha=0.5)+
  xlab("NBATH")
ggtitle("Variation du prix avec le nombre de salles de bain")
#4
ggplot(balt) +
  geom_point(aes(x = NROOM, y = PRICE)) +
  ggtitle("Variation du prix avec le nombre de chambres")
#5
ggplot(map) + 
  geom_sf() +
  geom_point(data = baltimore, aes(x = X, y = Y, color = NSTOR))+
  scale_colour_gradient(low = "deepskyblue", high = "red")
table(baltimore$NSTOR)          
#6
ggplot(map) +
  geom_sf() +
  geom_point(aes(x = X, y = Y, color = SQFT))+ 
  scale_colour_gradient(low = "deepskyblue", high = "red")
#7
ggplot(map) +
  geom_sf() +
  geom_point(aes(x = X, y = Y, color = PRICE))+
  scale_colour_gradient(low = "deepskyblue", high = "red")
#8
ggplot(map) + geom_sf() + geom_point(aes(x = X, y = Y, color = PRICE))+
  scale_colour_gradient(low = "deepskyblue", high = "red")
#9
ggplot(map) + geom_sf() + 
  geom_point(aes(x = X, y = Y, color = AGE))+
  scale_colour_gradient(low = "deepskyblue", high = "red")




#  #  #  # CORRELATION #  #  #  #
corr <- round(cor(balt), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Test de Geary
TgearyHN<-geary.test(baltimore$PRICE,listwknn)

#  #  #  # ARBRE DE DECISION #  #  #  #
arbre <-  rpart (PRICE ~. - X - Y , data = balt)
rpart.plot(arbre , box.palette = "RdBu", type =5)
data.frame(arbre$variable.importance)

arbre2 <-  rpart (PRICE ~. , data = balt)
rpart.plot(arbre2, box.palette = "RdBu", type =5)


#transformation en facteur 
baltimore$DWELL <- as.factor(baltimore$DWELL)
baltimore$PATIO <- as.factor(baltimore$PATIO)
baltimore$FIREPL <- as.factor(baltimore$FIREPL)
baltimore$AC <- as.factor(baltimore$AC)
baltimore$CITCOU <- as.factor(baltimore$CITCOU)
baltimore$BMENT <- as.factor(baltimore$BMENT)



#  #  #  # ESTIMATION DU MODELE PAR MCO #  #  #  #
mco <- lm(PRICE ~.-STATION, data = baltimore)
summary(mco)
vif(mco)
plot(mco)

baltimore$Bmentbis <- NA
baltimore$Bmentbis[baltimore$BMENT == "1"] <- "0"
baltimore$Bmentbis[baltimore$BMENT == "0"] <- "0"
baltimore$Bmentbis[baltimore$BMENT == "2"] <- "0"
baltimore$Bmentbis[baltimore$BMENT == "3"] <- "1"
baltimore$Bmentbis <- as.factor(baltimore$Bmentbis)

baltimore$NroomBis <- NA
baltimore$NroomBis[baltimore$NROOM == "3"] <- "3"
baltimore$NroomBis[baltimore$NROOM == "4"] <- "4"
baltimore$NroomBis[baltimore$NROOM == "5"] <- "5"
baltimore$NroomBis[baltimore$NROOM == "6"] <- "6"
baltimore$NroomBis[baltimore$NROOM == "7"] <- "7"
baltimore$NroomBis[baltimore$NROOM == "8"] <- "7"
baltimore$NroomBis[baltimore$NROOM == "9"] <- "7"
baltimore$NroomBis[baltimore$NROOM == "10"] <- "7"
baltimore$NroomBis <- as.numeric(baltimore$NroomBis)

mco2 <- lm(PRICE ~ NROOM + DWELL + NBATH + PATIO +FIREPL + AC + Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT + X + Y , data = baltimore)
summary(mco2)
vif(mco2)
plot(mco2)

test_model <-lm(PRICE ~ AGE,data = baltimore)
raintest(test_model)

#Construction de polynomes de degrés > 2 pour 
#tester une relation non linéaire avec Price
baltimore$age_poly2 <- baltimore$AGE^(2)
baltimore$age_poly3 <- baltimore$AGE^(3)
baltimore$age_poly4 <- baltimore$AGE^(4)
baltimore$age_poly5 <- baltimore$AGE^(5)

model_poly2 <- lm(PRICE ~ AGE + NROOM + DWELL + NBATH + PATIO +FIREPL + AC + 
                    Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                    X + Y + age_poly2 , data=baltimore)

model_poly3 <- lm(PRICE ~ AGE+NROOM + DWELL + NBATH + PATIO +FIREPL + AC + 
                    Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                    X + Y + age_poly3 , data=baltimore)

model_poly4 <- lm(PRICE ~ AGE+NROOM + DWELL + NBATH + PATIO +FIREPL + AC + 
                    Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                    X + Y + age_poly4 , data=baltimore)

model_poly5 <- lm(PRICE ~ AGE+NROOM + DWELL + NBATH + PATIO +FIREPL + AC + 
                    Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                    X + Y + age_poly5 , data=baltimore)

summary(model_poly2)
summary(model_poly3) 
summary(model_poly4) 
summary(model_poly5) 

MSE2 <-  mean(model_poly2$residuals^2)
MSE3 <-  mean(model_poly3$residuals^2)
MSE4 <-  mean(model_poly4$residuals^2)
MSE5 <-  mean(model_poly5$residuals^2)

MSEd <- data.frame(modele = seq_len(4),
                   MSE = c(MSE2, MSE3, MSE4, MSE5))

ggplot(MSEd, aes(x = modele, y = MSE)) +
  geom_point(col="tomato4",pch=19)+
  geom_line(col="tomato4")+
  xlab("modeles")+
  ylab("MSE")+
  ggtitle("MSE pour les differents modeles")

#On retient le polynome de degré 3
modele <- AGE+NroomBis + NBATH + PATIO +FIREPL + 
          Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
          X + Y + age_poly3

# K plus proches voisins
map_crd<-coordinates(map2)
points_shp<-SpatialPoints(map_crd)
shp.knn<-knearneigh(points_shp,k=12)
shp_k12_nb<-knn2nb(shp.knn)
shp_K12_listW <- nb2listw(shp_k12_nb, style="W")
wknn <- nb2mat(shp_k12_nb, zero.policy=T, style="W") 

#Transformation de la matrice de pondÃ©ration en objet listw
colnames(wknn) <- rownames(wknn) <- map2@data[,"PRICE"]
listwknn <- mat2listw(wknn, style="W") # pour normaliser

#  #  #  # MODELISATION #  #  #  #

###MCO###
mco_final <- lm(PRICE ~ AGE+NroomBis + NBATH + PATIO +FIREPL + 
                  Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                  X + Y + age_poly3 , data=baltimore)
summary(mco_final)
plot(mco_final)

#Autocorrelation des residus
dwtest(mco_final)
### Test de Moran adapte sur les residus
lm.morantest(mco_final,listwknn)

#test de normalité sur les résidus
shapiro.test(mco_final$residuals)

#visualisation des résidus via un histogramme
ggplot() + geom_histogram(aes(x=mco_final$residuals)) 

### Test LM-Error et LM-Lag
lm.LMtests(mco_final,listwknn,test="LMerr")
lm.LMtests(mco_final,listwknn,test="LMlag")
lm.LMtests(mco_final,listwknn,test="RLMerr")
lm.LMtests(mco_final,listwknn,test="RLMlag")


######SEM######
sem<-errorsarlm(PRICE ~ AGE+NroomBis + NBATH + PATIO +FIREPL + 
                  Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                  X + Y + age_poly3, data=baltimore, listwknn)
summary(sem)
moran.test(sem$residuals,listwknn)

res_sem <- sem$residuals
predict_sem <- sem$fitted.values

ggplot(map) +
  geom_sf() +
  geom_point(aes(x = X, y = Y, color = res_sem)) +
  scale_colour_gradient(low = "deepskyblue", high = "red")

ggplot() + geom_point(aes(x = predict_sem, y = res_sem))+ stat_smooth(method="lm",se=FALSE)

######SAR######
sar<-lagsarlm(PRICE ~ AGE+NroomBis + NBATH + PATIO +FIREPL + 
                Bmentbis + NSTOR + GAR + CITCOU + LOTSZ + SQFT +
                X + Y + age_poly3, data=baltimore, listwknn)
summary(sar)
moran.test(sar$residuals,listwknn)

res_sar <- sar$residuals
predict_sar <- sem$fitted.values

ggplot(map) +
  geom_sf() +
  geom_point(aes(x = X, y = Y, color = res_sar)) +
  scale_colour_gradient(low = "deepskyblue", high = "red")

ggplot() + geom_point(aes(x = predict_sar, y = res_sar))+ stat_smooth(method="lm",se=FALSE)

###SDM###
sardm<-lagsarlm(modele, data=baltimore, listwknn, type="mixed")
summary(sardm)
res_sardm <- sardm$residuals
predict_sardm <- sardm$fitted.values

ggplot(map) +
  geom_sf() +
  geom_point(aes(x = X, y = Y, color = res_sardm)) +
  scale_colour_gradient(low = "deepskyblue", high = "red")

### Test de l'hypothse de facteur commun
# sardm : Mole non contraint
# sem : Modele contraint
FC.test<-LR.sarlm(sardm,sem)
print(FC.test)



