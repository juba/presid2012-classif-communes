######################################################################
####Extensions
######################################################################


library(stringr)

## Chargement des fonctions
##source("fonctions.R")


######################################################################
####Chargement des données et recodages
######################################################################

## Chargement du fichier source brut
ds <- read.csv("data/source.csv")

## Renommage des variables de base
d <- data.frame(nom=ds$Libellé.de.la.commune,dpt=ds$Code.du.département,com=ds$Code.de.la.commune)
  
## Padding des identifiants
d$dpt <- str_pad(d$dpt, width=2, pad="0", side="left")
d$com <- str_pad(d$com, width=3, pad="0", side="left")
## Identifiant INSEE
d$id.com <- paste(d$dpt, d$com, sep="")
## Rownames
rownames(d) <- paste0(d$dpt,".",d$nom)
  
## Import et renommages des variables de vote
d$inscr <- ds$Inscrits
d$voix.abst <- ds$Abstentions
d$votants <- ds$Votants
d$voix.blancs <- ds$Blancs.et.nuls
d$voix.joly <- ds$Voix
d$voix.lepen <- ds$Voix.1
d$voix.sarkozy <- ds$Voix.2
d$voix.melenchon <- ds$Voix.3
d$voix.poutou <- ds$Voix.4
d$voix.arthaud <- ds$Voix.5
d$voix.cheminade <- ds$Voix.6
d$voix.bayrou <- ds$Voix.7
d$voix.dupont_aignan <- ds$Voix.8
d$voix.hollande <- ds$Voix.9

## Suppression des Collectivités d'outre-mer et français de l'étranger
## on ne garde que la métropole
d <- d[!(d$dpt %in% c("ZA","ZB","ZC","ZD","ZM","ZN","ZP","ZS","ZW","ZX","ZZ")),]  

## Suppression des 3 communes annulées par le conseil constitutionnel
d <- d[!(rownames(d) %in% c("10.Pont-sur-Seine","31.Bourg-d'Oueil","43.Lissac")),]

## Calcul des variables de pourcentage pour chaque candidat
## Les pourcentages sont calculés sur les votants, et non sur les exprimés
d$abst <- d$voix.abst / d$inscr * 100
noms <- c("blancs","joly","lepen","sarkozy","melenchon","poutou",
          "arthaud","cheminade","bayrou","dupont_aignan","hollande") 
for (nom in noms) {
  d[,nom] <- d[,paste0("voix.",nom)] / d$votants * 100
}



######################################################################
####Classification
######################################################################


#####Calcul de la classification
######################################################################


vars <- c("abst","blancs","joly","lepen","sarkozy","melenchon","poutou",
          "arthaud","cheminade","bayrou","dupont_aignan","hollande")
tmp.brut <- d[,vars]

library(flashClust)
dist.brut <- dist(tmp.brut)
hc.brut.ward <- hclust(dist.brut,method="ward")



#####Export des données
######################################################################

save(hc.brut.ward, file="results/hc_brut_ward.rda")
save(d, file="results/d.rda")

