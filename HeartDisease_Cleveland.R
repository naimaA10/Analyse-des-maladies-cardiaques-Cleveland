#Heart diseases Cleveland Ohio

heart <-read.table("heart.csv", header = TRUE, sep = ",", dec = ".")
colnames(heart)<-c("age", 
                   "sexe",
                   "type_douleur",
                   "pression_arterielle",
                   "cholesterol",
                   "sucre",# > 120 mg/dl (1 = true, 0 = false)
                   "resting_electrocardiographic",
                   "freq_max",
                   "angine_exercice",
                   "ST_depression_exercise",
                   "slope",
                   "ca",
                   "thal",
                   "target")

hist(heart$type_douleur)
mean(heart$cholesterol)
by(heart$cholesterol,INDICES = heart$target,FUN = mean)
by(heart$ST_depression_exercise,INDICES = heart$angine_exercice,FUN = mean)

#Nettoyage des donnees : 

##Suppression de colonnes 
heart$slope<-NULL
heart$ca<-NULL
heart$thal<-NULL
heart$num<-NULL

#Voir combien de NA par colonne:
sapply(heart,function(x) sum(is.na(x)))
#Enlève les lignes avec des NA (normalement pas besoin ici)
diseases_clean_cleveland<-na.omit(diseases_clean_cleveland)

for (i in 1:length(diseases_clean_cleveland$cholesterol)) {
  if(diseases_clean_cleveland$cholesterol[i]==0){
    diseases_clean_cleveland$cholesterol[i]<-NA
  }
  
}
mediane<-median(diseases_clean_cleveland$cholesterol,na.rm=TRUE)
for (i in 1:length(diseases_clean_cleveland$cholesterol)) {
  if(is.na(diseases_clean_cleveland$cholesterol[i])){
    diseases_clean_cleveland$cholesterol[i]<-mediane
  }
  
}



#Vérification :
sapply(diseases_clean_cleveland,function(x) sum(is.na(x)))



#Transformation de l'age en facteur ? 2 classes
age2_cleveland<-ifelse(diseases_clean_cleveland$age<54,"inferieur_54","superieur_54")
age2_cleveland<-factor(age2_cleveland, labels = c("<54"
                                                  ,">54"))
diseases_clean_cleveland<-data.frame(diseases_clean_cleveland,age2_cleveland)
qualite_cleveland<- c()

#Transformation de l'a qualité de lelectro en facteur ? 3 classes
for (i in 1:302) {
  print(diseases_clean_cleveland$resting_electrocardiographic[i]==2)
  if (diseases_clean_cleveland$resting_electrocardiographic[i]==0){
    qualite_cleveland<- c(qualite_cleveland,"Normal" )
    
  }
  else if (diseases_clean_cleveland$resting_electrocardiographic[i]==1){
    qualite_cleveland<- c(qualite_cleveland,"ST anormal" )
    
  }
  else{
    qualite_cleveland<- c(qualite_cleveland,"Hypertrophie" )
    
  }
}
qualite_cleveland<-factor(qualite_cleveland, labels = c("Hypertrophie"
                                                        ,"Normal", "ST anormal"))
diseases_clean_cleveland<-data.frame(diseases_clean_cleveland,qualite_cleveland)



#On peut calculer les moyennes maintenant : 
mean(diseases_clean_cleveland$age)
mean(diseases_clean_cleveland$cholesterol)


#Idees de questions :

#Tests : 

shapiro.test(diseases_clean_cleveland$age)
shapiro.test(diseases_clean_cleveland$pression_arterielle)
shapiro.test(diseases_clean_cleveland$freq_max)#TEST NON SIGNIFICATIF P-VALUE >0.05!
hist(diseases_clean_cleveland$freq_max)
shapiro.test(diseases_clean_cleveland$cholesterol)
hist(diseases_clean_cleveland$cholesterol)
hist(diseases_clean_cleveland$cholesterol)#enlever les 0 
shapiro.test(diseases_clean_cleveland$ST_depression_exercise)

#Peu de varaibles (une) sui(ven)t une loi normale : dans la population entière,
#Chaque VA suit une loi normale. Mais comme on travaille uniqumenet sur les malades,
#on est que en bout de file de la loi normale

#Population:patients atteints d'une maladie cardiaque

#La pression arterielle est-elle la m?me chez les 2 sexes ?=> Comparaison de 2 moyennes
#Independance entre angine pdt test exercice physique et electrocardiogramme au repos ?=>Test d'ind?pendance Chi2
#L'age est-il le meme dans chacun des types de douleurs ?=>ANOVA ? 1 facteur (4 niveaux)

#Naima:
#Test de conformité ? une moyenne théorique : cholestérol avec mu0 =1.7g/L => 170mg/mL
#Chi2 : type de douleurs et frq card max à l'effort (à transformer en VA qualitative)


#Emma:
#Test de comparaison de 2 moyennes  : X : freq_card maximale Facteur : age ? transformer en VA qualitative inf ? 59 et sup ? 59

#Question : la freq maximale cardiaque est-elle la m?me chez les 2 groupes d'age 
#H0: muInf = muSup : les frequences cardiaques sont identiques
#H1:muInf != muSup : les frequences cardiaques sont diff?rentes

#Test bilat?ral de comparaison de 2 moyennes sur grands ?chantillons. 
#CA : X suit une loi normale dans les 2 niveaux du facteur : 
by(diseases_clean_cleveland$freq_max,INDICES = diseases_clean_cleveland$age2_cleveland,FUN = shapiro.test)
boxplot(diseases_clean_cleveland$freq_max~diseases_clean_cleveland$age2_cleveland,outline=FALSE)
#Test non significatif (p-value>0.05)
#X ne suit pas une loi normale dans les 2 niveaux du facteur

#Egalit? des varainces : 
var.test(formula = freq_max~age2_cleveland,data = diseases_clean_cleveland)
#Test non significatif (p-value>0.05)
#Variances sont ?gales

#T-test : on peut donc faire un test de Student (epsilon)
t.test(formula = freq_max~age2_cleveland,data = diseases_clean_cleveland, var.equal = TRUE)
#TEST SIGNIFICATIF !!!!!!!!!!!! : p-value <0.05 !!!!!!!
#Au risque de 5%, les moyennes sont différentes. 
#De plus, les patients de moins de 54 ans ont une freq max plus ?lev?e.
#Chez les patients à risque, on pourrait regarder leur 



##########################################################################

#ANOVA : X:pression art?rielle 
#F = qualité de l'electrocardiogramme 
#Test de comparaison de 3 moyennes =>Analyse de la variance

for (lg3 in unique(diseases_clean_cleveland[, "resting_electrocardiographic"])) {
  hist(diseases_clean_cleveland[diseases_clean_cleveland[, "resting_electrocardiographic"] == lg3, "pression_arterielle"], breaks = 6, 
       xlim = c(min(diseases_clean_cleveland[, "pression_arterielle"]), max(diseases_clean_cleveland[, "pression_arterielle"])),
       xlab = "pression_arterielle")
}


boxplot(diseases_clean_cleveland$pression_arterielle~diseases_clean_cleveland$qualite_cleveland, outline=FALSE, 
        main = "Pression artérielle et qualité de l'électrocardiogramme", 
        ylab = " X : Pression artérielle",
        xlab = "Qualité de l'électrocardiogramme", col = "#3A8F7D" )

#Normalit?(s) : 

by(diseases_clean_cleveland$pression_arterielle, INDICES = diseases_clean_cleveland$resting_electrocardiographic, FUN=shapiro.test)
#On dit que ok car ?chantillons suffisamment grands

#Egalit? des variances : 
bartlett.test(pression_arterielle ~ qualite_cleveland, data = diseases_clean_cleveland)
#p-value>0.05: Variances ?gales

#ANOVA en elle m?me

anova_heart_cleveland<-aov(formula = pression_arterielle~qualite_cleveland, data = diseases_clean_cleveland)
summary(anova_heart_cleveland)
#Test SIGNIFICATIF : Au moins moyennes diff?rentes  au risque 5% !!!!!!!
pairwise.t.test(x = diseases_clean_cleveland$pression_arterielle , g=diseases_clean_cleveland$qualite_cleveland)
pairwise.t.test(x = diseases_clean_cleveland$pression_arterielle , g=diseases_clean_cleveland$resting_electrocardiographic)

#significatif entre 0 et 2 ?
#Entre le groupe 0 et le groupe 2, on voit que les moyennes sont significativement différentes.
aggregate(formula = pression_arterielle~resting_electrocardiographic, data = diseases_clean_cleveland, FUN  = mean)
aggregate(formula = pression_arterielle~qualite_cleveland, data = diseases_clean_cleveland, FUN  = mean)

#La pression artérielle est supérieure chez le groupe 2. Les personnes ayant un electrocardiogramme 
#qui montre une hypertrophie ventriculaire ont une pression artérielle plus élévee. 
#On pourrait donc orienter le diagnostic vers ceci en mesurant uniqument la pression artérielle



############################################################################
#cholesterol dans les 2 niveaux angine a l'exercice ?
#X : Le taux de cholesterol

#2 populations 

#Test bilat?ral de comparaison de 2 moyennes sur grands ?chantillons. 
#CA : X suit une loi normale dans les 2 niveaux du facteur ?
by(diseases_clean_cleveland$cholesterol,INDICES = diseases_clean_cleveland$angine_exercice,FUN = shapiro.test)
boxplot(diseases_clean_cleveland$cholesterol~diseases_clean_cleveland$angine_exercice)
#Test  significatif (p-value<0.05)

#Egalit? des variances : 
var.test(formula = cholesterol~angine_exercice,data = diseases_clean_cleveland)
#Test non significatif (p-value>0.05)
#Variances sont ?gales

#T-test : 
t.test(formula = cholesterol~angine_exercice,data = diseases_clean_cleveland, var.equal = TRUE)
#NR de H0: freq card max est la m?me chez tous les patients peut importe leur age


#############################################################################
angine <- ifelse(diseases_clean_cleveland$angine_exercice ==1,"Angine","Pas d'angine" )
angine<-(factor(angine, levels = c("Angine", "Pas d'angine")))
diseases_clean_cleveland <- data.frame(diseases_clean_cleveland, angine)
#Test non paramétrique : segment st depression epdt l'exercice et angine de poitrine ou non pdt l'exercice
boxplot(diseases_clean_cleveland$ST_depression_exercise~diseases_clean_cleveland$angine,  main = "Segment ST et Angine", 
        ylab = " X : Depression du segment ST pendant l'exercice", xlab = "Angine � l'exercice ou non", col = c("#a7c4bb","#F88D23"),outline = FALSE )

by(diseases_clean_cleveland$ST_depression_exercise, INDICES = diseases_clean_cleveland$angine_exercice, FUN=shapiro.test)
#X ne suit pas une loi normale dans les 2 niveaux du factuer, on va donc faire un test non param?trique
wilcox.test(formula = ST_depression_exercise ~angine_exercice , data = diseases_clean_cleveland)
#Test Significatif p-value < 0.05
#Au risque 5%, les moyennes sont significativement diff?rentes. 
aggregate(formula =ST_depression_exercise~angine_exercice, data =diseases_clean_cleveland, FUN = mean)
#1 = yes
#Pendant l'exercice, la depression du segment ST des patients victimes d'une angine de poitrine pendant l'exercice
#est superieur ? celui des patients non vitcimes d'une angine de poitrine ? l'effort.


#La frequence cardiaque max est-elle la meme chez les 2 sexes ?=>Comparaison de 2 moyennes

#X : freq card max
#F : sexe de l'individu (2 niveaux)

by(diseases_clean_cleveland$freq_max,INDICES = diseases_clean_cleveland$sexe, FUN = shapiro.test)
#Test NS 

# pression arterielle et angine pdt test ou non ?
# ST depression selon l'age ?

#############################################################################
#TEST DE CONFORMITE:
#Question: 
#La moyenne de cholest?rol dans cet ?chantillon est-elle ?quivalente ? celle de la population?

#La variable:
#X: Cholest?rol, quantitative continue, param?tre mu0 et sigma0? inconnues

mean(diseases_clean_cleveland$cholesterol)
sd(diseases_clean_cleveland$cholesterol)
#Echantillon: n=130, m=246.6931 , s?=(51.77692)?

#Hypoth?se: H0: mu<mu0 et H1: mu>=mu0
#Test unilat?rale de conformit? ? une moyenne th?orique mu0 dans le cas des grands ?chantillon
#R?gle de d?cision: Rejet de H0 si epsiloncal= (|m-m0|)/ (racine(s?/n)) > epsilonth?o

#Calcul:
resultat <- t.test(diseases_clean_cleveland$cholesterol, mu=200)
resultat #Le test est significatif, HO est rejet?e

plot(heart$cholesterol,col="Blue",
     main="chlolest�rol dans l'�chantillon",
     xlab = "Individus",ylab = "Cholest�rol en ml/dl")
abline(h=200,col="Red")

#Conclusion: Au risque 5%, la moyennede cholest?rol dans cet ?chantillon est sup?rieur ? celle de la population

names(diseases_clean_cleveland)
#############################################################################

#TEST DU X? D'INDEPENDANCE:
#Question:
#electroradiographie est-il li? ? l'?ge?

#Ajout variable resting_electrocardiographic2 qualitative ? 2 modalit?s
resting_electrocardiographic2 <- ifelse(diseases_clean_cleveland[,"resting_electrocardiographic"] <=1, "[1]","[2]")
resting_electrocardiographic2 <- factor(resting_electrocardiographic2, levels = c("[1]","[2]"))
diseases_clean_cleveland <- data.frame(diseases_clean_cleveland, resting_electrocardiographic2)



#Les variables:
#X: Electroradiographie, qualitative ? 2 modalit?s, param?tre PI inconnu
#Y: Ages, qualitative ? 2 modalit?s, param?tre PI inconnu

#Hypoth?ses:
#H0:Electroradiographie et l'?ges sont ind?pendantes
#H1:Electroradiographie d?pend de l'?ge

#R?gle de decision: Rejet de H0 si sobs = somme((Cij-Oij)?/Cij) > X?th?o

#Calcul:
#Tableau des observations:
table(heart$sexe)
table(heart$target)
tableau <- table(heart$sexe, 
                 heart$target)
colnames(tableau)<-c("Malades", "Sains")
rownames(tableau)<-c("Femmes","Hommes")
tableau

#Ca: Cij >5:
chisq.test(tableau)$expected #OK

#Test du X?:
mon_chisq <- chisq.test(tableau)
mon_chisq #pvalue<0,05 , RH0

#Si Test significatif donc analyse des r?sidus:
chisq.test(tableau)$residuals^2 
#Ceux qui ont moins de 54 ans ont un electroradiogramme 1 et 2 plus
#important que ceux qui ont plus d 54 ans

barplot(heart$target~heart$sexe,  
     main = "L'?lectroradiogramme selon l'?ge", 
     ylab = " X : ?lectroradiogramme", xlab = "Ag? sup?rieur ou inf?rieur ? 54 ans", col = "#3A6F9D" )
barplot(heart$target~heart$sexe)
#############################################################################
vecteur<-c(24,114,72,93)
x<-matrix(vecteur, ncol = 2)
x
barplot(x,col = c("red","green"),
        beside = TRUE,
        names = c("Malades","Sains"))
legend("topleft", c(""))
