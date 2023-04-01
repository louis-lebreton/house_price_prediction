## Analyse & Modélisation des prix de propriétés vendues aux USA entre mai et juillet 2014
## Etat : Washington ; Comté : King
#######################################################################################################################

## Definition de l'emplacement du repertoire de travail
setwd("C:/Users/lebre/OneDrive/Bureau/MRL/Projet")
getwd()

## Packages
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggcorrplot) # pour réaliser matrice de corrélation
library(openxlsx) # pour enregister des fichiers excel
library(lmtest) # pour bptest
library(geojsonio) # pour ouverture des fichiers geojson
library(broom) # pour utiliser la fonction tidy
library(viridis) # pour palettes de couleurs


#######################################################################################################################
# Création de la base de données ######################################################################################
#######################################################################################################################

# description du data frame
df <- read.table(file="data_house_price.csv",header=TRUE,sep=",")
#View(df)
sapply(df,class)
dim(df) # nombre de lignes et de colonnes
colnames(df)
head(df)
tail(df)

sum(is.na(df)) # 0 valeurs manquantes


# conversion de la date
df$date <- ymd_hms(as.character(df$date))
# ajout de l'âge de la propriété lors de la vente
df$age <- as.integer(format(df$date, "%Y")) - df$yr_built
# ajout du jour de la semaine
df$weekday <- weekdays(df$date)
df$weekday <- factor(df$weekday, levels = c('lundi', 'mardi', 'mercredi', 'jeudi','vendredi','samedi','dimanche'))
# conversion des variables en square feet en m2
df$m2_living <- df$sqft_living*0.0929
df$m2_lot <- df$sqft_lot*0.0929
df$m2_above <- df$sqft_above*0.0929
df$m2_basement <- df$sqft_basement*0.0929

# suppression des prix aberrants
df <- df[(df$price>8000)&(df$price<10000000),]

# prix m2
df$pricem2 <- df$price/df$m2_living

# création d'un data frame des variables numériques
df_num <- df[,c("price","m2_living","bedrooms","bathrooms","floors", "waterfront","view","condition","age")]

unique(df$city) # villes analysées
length(unique(df$city)) # 44 villes analysées
unique(df$statezip) # Etats analysées : seulement Washington 
unique(df$country) # Pays analysé : seulement les USA

# exportation en fichier Excel
write.xlsx(df,'new_data.xlsx')

########################################################################################################################
# Exploration & statistiques descriptives ##############################################################################
########################################################################################################################

# exportation du summary en fichier excel
summary(df)
summary_df <- data.frame(unclass(summary(df)), check.names = FALSE, stringsAsFactors = FALSE)
write.xlsx(summary_df, file = "summary.xlsx")

# Nombre de ventes par jour sur la période
ggplot(df, aes(x = date)) + 
  geom_bar(fill='darkblue') +
  xlab("Date") +
  ylab("Nombre de propriétés vendues par jour")+
  labs(title = "Nombre de propriétés vendues par jour",
       subtitle = "Année 2014")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))
  

# Nombre de ventes par jour de la semaine
ggplot(df, aes(x = weekday)) + 
  geom_bar(fill='#25745D') +
  xlab("Jour de la semaine") +
  ylab("Nombre de propriétés vendues par jour")+
  labs(title = "Nombre de propriétés vendues par jour de la semaine"
       ,subtitle = "De Mai à Juillet 2014")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))

# Nombre de ventes par ville
df_villes <- df %>% group_by(city) %>% summarise(count=n()) %>% arrange(desc(count))
ggplot(df_villes, aes(x = reorder(city, count), y = count)) + 
  geom_bar(stat="identity", fill='#742542') +
  coord_flip()+
  xlab("Ville") +
  ylab("Nombre de propriétés vendues par ville")+
  labs(title = "Nombre de propriétés vendues par ville"
       ,subtitle = "De Mai à Juillet 2014")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))

df_villes <- aggregate(price ~ city, data = df, mean)
# Prix moyen par ville
ggplot(df_villes, aes(x = reorder(city, price), y = price)) + 
  geom_bar(stat="identity", fill='#157989') +
  coord_flip()+
  xlab("Ville") +
  ylab("Prix moyen des propriétés par ville (en dollars courants)")+
  labs(title = "Prix moyen des propriétés vendues par ville",subtitle = "De Mai à Juillet 2014")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12))

df_villes <- aggregate(cbind(pricem2,price,bedrooms,bathrooms,floors,waterfront,view,condition,yr_built,age,m2_living) ~ city, data = df, mean)

# Prix/m2 moyen par ville
ggplot(df_villes, aes(x = reorder(city, pricem2), y = pricem2)) + 
  geom_bar(stat="identity", fill='#09aec8') +
  coord_flip()+
  xlab("Ville") +
  ylab("Prix moyen au m2 des propriétés par ville (en dollars courants)")+
  labs(title = "Prix moyen au m2 des propriétés vendues par ville",subtitle = "De Mai à Juillet 2014")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12))

# boxplots
par(mfrow=c(2,5))
boxplot(df_num$m2_living,main="m2_living",col="#DDB9FF")
boxplot(df_num$m2_lot,main="m2_lot",col="#DDB9FF")
boxplot(df_num$m2_above,main="m2_above",col="#DDB9FF")
boxplot(df_num$m2_basement,main="m2_basement",col="#DDB9FF")
boxplot(df_num$age,main="age",col="#FFDEB9")
boxplot(df_num$bedrooms,main="Bedrooms",col="#B9CFFF")
boxplot(df_num$bathrooms,main="Bathrooms",col="#B9CFFF")
boxplot(df_num$floors,main="floors",col="#F95151")
boxplot(df_num$condition,main="condition",col="#F95151")
boxplot(df_num$view, main="view",col="#F95151")

par(mfrow=c(1,1))
boxplot(df_num$pricem2,main="pricem2",col="#09aec8")

# données un format long
df_long <- gather(df_num)
# histogrammes des différentes variables
ggplot(df_long, aes(x = value)) +
  geom_histogram(fill="#283954") +
  facet_wrap(~ key, scales = "free")

# matrice de corrélation
cor_mat <- cor(df_num)

ggcorrplot(cor_mat, hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 6,
           title="Matrice de corrélation",
           ggtheme = ggplot2::theme_gray,
           colors = c("red", "white", "blue"))


########################################################################################################################
# Régressions linéaires simples ########################################################################################
########################################################################################################################

## price x age ##

reg <- lm(df_num$price~df_num$m2_living)

summary(reg) # Description de la regression

reg$coefficients # coefficients B0 et B1
pval <- summary(reg)$coefficients[8]
(pval<0.05) # p-value < 0.05 donc significatif
# intervalles de confiance à 95 %
confidence1 <- round(as.numeric(confint(reg)[2]),2)
confidence2 <- round(as.numeric(confint(reg)[4]),2)

# Analyse de la variance
anova(reg)

# Analyse des résidus : Hypothèses de Gauss-Markov vérifiées ? 
res <- resid(reg)
acf(res) # indépendance des résidus ?
mean(res) # nullité de l'ésperance des résidus 
plot(df_num$price,axes=TRUE,rstudent(reg),xlab="Prix",ylab="Résidus studentisés",main="Graphique des résidus studentisés : Prix x m2_living")
bptest(reg) # Breusch-Pagan Test
# homoscédasticité : rejeté
shapiro.test(res) # p-value<0.05 donc normalité des résidus vérifiée

ggplot(df_num,aes(y=price,x=m2_living))+
  geom_point(col="darkblue")+
  geom_smooth(formula = y ~ x,method="lm",se=TRUE,col='red')+
  labs(x="m2 habitable",y="Prix en dollars",
    title = paste("Prix x m2 habitable : prix = ", round(coefficients(reg)[1],2), " + m2_habitable * ", round(coefficients(reg)[2],2), "+/- [",confidence1,";",confidence2,"] (95%)\n"))+
  xlim(min(df_num$m2_living), 700)+ # 4 valeurs extremes non représentées
  ylim(min(df_num$price), 5000000)

## price x bathrooms ##

reg <- lm(df_num$price~df_num$bathrooms)

summary(reg) # Description de la regression

reg$coefficients # coefficients B0 et B1
pval <- summary(reg)$coefficients[8]
(pval<0.05) # p-value < 0.05 donc significatif
# intervalles de confiance à 95 %
confidence1 <- round(as.numeric(confint(reg)[2]),2)
confidence2 <- round(as.numeric(confint(reg)[4]),2)

# Analyse de la variance
anova(reg)

# Analyse des résidus : Hypothèses de Gauss-Markov vérifiées ? 
res <- resid(reg)
acf(res) # indépendance des résidus 
mean(res) # nullité de l'ésperance des résidus 
plot(df_num$price,axes=TRUE,rstudent(reg),xlab="Prix",ylab="Résidus studentisés",main="Graphique des résidus studentisés : Prix x Nombre de salles de bains")
bptest(reg) # Breusch-Pagan Test
# homoscédasticité : acceptée
shapiro.test(res) # p-value<0.05 donc normalité des résidus vérifiée

ggplot(df_num,aes(y=price,x=bathrooms))+
  geom_point(col="#4D305B")+
  geom_smooth(formula = y ~ x,method="lm",se=TRUE,col='red')+
  labs(x="Nombre de salles de bains",y="Prix en dollars",
    title = paste("Prix x Nombre de salles de bains : prix = ", round(coefficients(reg)[1],2), " + bathrooms * ", round(coefficients(reg)[2],2), "+/- [",confidence1,";",confidence2,"] (95%)\n"))+
  xlim(min(df_num$bathrooms), max(df_num$bathrooms))+
  ylim(min(df_num$price), max(df_num$price))


########################################################################################################################
# Régression linéaire multiple #########################################################################################
########################################################################################################################

# regression multiple avec 7 variables explicatives
# Suppression de la variable "bathrooms" car elle est trop corrélée à la variable "m2_living"
reg_multiple = lm(price ~  m2_living + bedrooms + floors + waterfront +view + condition + age,data = df_num)
summary(reg_multiple)

## Pour sélectionner les variables les plus pertinentes, on utilise la méthode forward stepwise
## Rappel : Le critère BIC pénalise davantage les modèles plus complexes que le critère AIC

## La Méthode forward stepwise avec le critère AIC : Critère d'information d'Akaike

# modèle vide
model <- lm(price ~ 1, data = df_num)

# variables explicatives
var_list <- c("waterfront", "bedrooms", "m2_living", "floors","view","condition","age") 

while (length(var_list) > 0) { # tant que var_list est non vide
  AIC_vec <- c() # vecteur de critères AIC
  for (i in 1:length(var_list)) {
    # formule : price ~ i eme var
    formule <- as.formula(paste("price ~", paste(c(attr(terms(model), "term"), var_list[i]), collapse = "+")))
    
    modele_temporaire <- lm(formule , data = df_num) # régression pour chaque variable
    AIC_vec[i] <- AIC(modele_temporaire) # ajout du AIC de chaque régression pour chaque regression
  }
  
  min_AIC_indice <- which.min(AIC_vec) # quelle regression a le plus petit AIC ?

  if (AIC_vec[min_AIC_indice] < AIC(model)) { # cette AIC minimum est-il inferieur au AIC de notre modèle actuel ? 
    # si oui: on remplace notre ancien modèle par le nouveau modèle qui est meilleur
    model <- update(model, formula = as.formula(paste("price ~", paste(c(attr(terms(model), "term"), var_list[min_AIC_indice]), collapse = "+"))))
    var_list <- var_list[-min_AIC_indice] # supprimons donc la variable utilisée du vecteur var_list
  } else {
    # si non: fin de la boucle et on garde le dernier modèle actualisé
    break
  }
}

length(coef(model))-1 # 7 variables expicatives
coef(model)
model
summary(model)

## La Méthode forward stepwise avec le critère BIC : Critère d'information bayésien

# modèle vide
model <- lm(price ~ 1, data = df_num)

# variables explicatives
var_list <- c("waterfront", "bedrooms", "m2_living", "floors","view","condition","age") 

while (length(var_list) > 0) { # tant que var_list est non vide
  BIC_vec <- c() # vecteur de critères BIC
  for (i in 1:length(var_list)) {
    # formule : price ~ i eme var
    formule <- as.formula(paste("price ~", paste(c(attr(terms(model), "term"), var_list[i]), collapse = "+")))
    
    modele_temporaire <- lm(formule , data = df_num) # régression pour chaque variable
    BIC_vec[i] <- BIC(modele_temporaire) # ajout du BIC de chaque régression pour chaque regression
  }
  
  min_BIC_indice <- which.min(BIC_vec) # quelle regression a le plus petit BIC ?
  
  if (BIC_vec[min_BIC_indice] < BIC(model)) { # cette BIC minimum est-il inferieur au BIC de notre modèle actuel ? 
    # si oui: on remplace notre ancien modèle par le nouveau modèle qui est meilleur
    model <- update(model, formula = as.formula(paste("price ~", paste(c(attr(terms(model), "term"), var_list[min_BIC_indice]), collapse = "+"))))
    var_list <- var_list[-min_BIC_indice] # supprimons donc la variable utilisée du vecteur var_list
  } else {
    # si non: fin de la boucle et on garde le dernier modèle actualisé
    break
  }
}

length(coef(model))-1 # 7 variables
coef(model)
model # modèle final
summary(model)

# data frame inventé de 3 maisons : modeste, moyenne, luxueuse
df_invent <- data.frame(name=c("Maison moeste","Maison moyenne","Maison luxueuse"),
                        waterfront = c(0, 0, 1),
                        bedrooms = c(1, 3, 7),
                        bathrooms = c(1, 2, 5),
                        floors = c(1, 2, 3),
                        view = c(0, 2, 4),
                        condition = c(1, 2, 5),
                        age = c(5, 15, 40),
                        m2_living=c(80,150,300))

predictions <- predict(model,df_invent)
predictions # predictions par notre modèle des prix de ces 3 maisons inventés
df_invent$prix_predit <- predictions
write.xlsx(df_invent, file = "df_invent.xlsx") # exportation du tableau

########################################################################################################################
# Cartes ###############################################################################################################
########################################################################################################################

# df : représentation spatiale (latitude + longitude) des villes
geo <- geojson_read("cities_wa.geojson",  what = "sp")

geo_tidy <- tidy(geo,region="NAME") # conversion du geojson en un data frame de longitude/latitude

# fond de carte obtenu (villes de l'Etat de Whashington)
ggplot() +
  geom_polygon(data = geo_tidy, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

# join: geo tidy + la base de donnees
geo_tidy = geo_tidy %>%
  left_join(. , df_villes, by=c("id"="city"))
geo_tidy <- na.omit(geo_tidy) # on supprime les villes qui ne nous interessent pas

geo_tidy_agg <- geo_tidy %>% # pour ajouter les noms de villes en etiquette au milieu de la surface/polygone
  group_by(id) %>% 
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

# Choisir une variable à représenter parmi : price,price,bedrooms,bathrooms,floors,waterfront,view,condition,yr_built,age,m2_living
var="price"
ggplot() +
  geom_polygon(data = geo_tidy, aes_string(fill = var, x = "long", y = "lat", group = "group"),size=1, alpha=1) +
  geom_text(data = geo_tidy_agg, aes(x = long, y = lat, label = id), size = 3, color = "black") + # etiquettes
  theme_void()+
  coord_map()+
  # choix de couleurs
  #scale_discrete_manual("fill", values = c("#FFB200", "#EEACF0", "#B8F0AC","#2DB2BB"),na.value="#555555")+
  scale_fill_viridis(name="Prix au m2",option = "plasma",direction = -1) + # ou direction = -1
  #scale_fill_gradient2(name="Prix m2",low = "green", mid = "#F7F9A8", high = "brown", midpoint = mean(df$price))+
  # legende
  labs(
    title = "Moyenne du prix au m2 des propriétés vendues",
    subtitle = "Comté de King (Washington) - Année 2014")+
  # theme de legende
  theme(text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        
        plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=0, unit = "cm") ),
        
        legend.position = c(0.85, 0.2))




