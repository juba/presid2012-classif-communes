######################################################################
####Extensions
######################################################################


library(stringr)
library(ggplot2)
library(rgrs)
library(grid)
library(reshape2)

## Chargement des fonctions
source("fonctions.R")

## Répertoire de sortie des graphiques
out.path <- "~/projets/data.nozav.org/images/"


######################################################################
####Chargement
######################################################################

## Chargement des données
load("results/d.rda") ## d
## Chargement du résultat de la classification
load("results/hc_brut_ward.rda") ## hc.brut.ward

## Affichage du dendrogramme
png(file=paste0(out.path,"ccp_dendro.png"),width=600,height=500)
plclust(hc.brut.ward,labels=FALSE,hang=0)
dev.off()

#####Choix du nombre de classes
######################################################################


## Le code suivant génère des graphiques décrivant les écarts à la moyenne
## pour les différentes variables pour chaque groupe, pour l'ensemble des classifications
## comprenant entre 2 et 30 groupes. Étudier tour à tour chacune de ces classifications
## peut aider à choisir le nombre de classes final retenu
##
## for (nb.classes.brut in 2:30) {
##   groupes.brut <- factor(cutree(hc.brut.ward, k=nb.classes.brut))
##   table(groupes.brut)
##   m.brut <- diffmatrix(vars, groupes.brut, seuil=0.001)
##   pdf(file=paste("tmp/diffmatrix_brut_",nb.classes.brut,".pdf",sep=""))
##   print(diffmatrix.plot(m.brut, seuil.diff=0.5, title=paste("Normée", "Ward", nb.classes.brut, sep=" - ")))
##   dev.off()
## }


## On conserve 8 classes
nb.classes.brut <- 8

## Calcul des groupes
groupes.brut <- factor(cutree(hc.brut.ward, k=nb.classes.brut))
## Effectifs
table(groupes.brut)
  


#####Description des classes
######################################################################


vars <- c("lepen","sarkozy","dupont_aignan","bayrou","hollande",
          "joly","melenchon","poutou","arthaud","cheminade",
          "blancs","abst")
labels <- c("Le Pen","Sarkozy","Dupont-Aignan","Bayrou","Hollande",
            "Joly","Mélenchon","Poutou","Arthaud","Cheminade",
            "Blancs","Abstention")

## Graphique comparant les moyennes pour chaque variable et pour chaque
## groupe. Seuls les écarts supérieurs à 0.5 points en valeur absolue
## et significatif au seuil de 0.001 sont affichés.

m.brut <- diffmatrix(vars, groupes.brut, seuil=0.001, levels=vars, labels=labels)

png(file=paste0(out.path,"ccp_diffmatrix.png"), width=600, height=500)
diffmatrix.plot(m.brut, seuil.diff=0.5,
                levels=vars, labels=labels,
                title="Caractérisation des groupes")
dev.off()

## Répartition du vote Le Pen dans les différents groupes
# grplot("lepen", groupes.brut)

## Calcul des variables centrées (on leur soustrait
## leur moyenne nationale)
dv <- d[,vars]
## Pourcentages obtenus au niveau national
moyennes <- c(17.9, 27.18, 1.79, 9.13, 28.63, 2.31, 11.1, 1.15, 0.56, 0.25, 1.92, 20.52)
dv <- sweep(dv, 2, moyennes)
dv$groupes <- groupes.brut
## Passage en format "long"
dvm <- melt(dv)

## Export d'un graphe de comparaison de densités pour chaque groupe
for (groupe in 1:nb.classes.brut) {
  png(file=paste0(out.path,"ccp_densites_groupe",groupe,".png",sep=""),width=400,height=400)
  print(groupes.density(dvm, groupe=groupe))
  dev.off()
}

## Export d'un graphe de comparaison de densités pour chaque groupe au format
## SVG
## for (groupe in 1:nb.classes.brut) {
##   svg(filename=paste("out/ccp_densites_groupe",groupe,".svg",sep=""))
##   print(groupes.density(dvm, groupe=groupe))
##   dev.off()
## }


######################################################################
####Cartographie
######################################################################

couleurs <- c("#55D7D7",
              "#BBBBBB",
              "#00E93C",
              "#FC6355",
              "#FF9C00",
              "#E14D9C",
              "#FCF457",
              "#5781FC")

## Export des cartes de répartition des communes des différents groupes
for (groupe in 1:nb.classes.brut) {
  png(file=paste0(out.path,"ccp_carte_groupe",groupe,".png",sep=""),width=400,height=400)
  print(groupe.carte(geo, groupe=groupe, couleurs))
  dev.off()
}

## Export des cartes de répartition des communes des différents groupes
## au format SVG
## for (groupe in 1:nb.classes.brut) {
##   svg(filename=paste("out/ccp_carte_groupe",groupe,".svg",sep=""))
##   print(groupe.carte(geo, groupe=groupe, couleurs))
##   dev.off()
## }



######################################################################
####Export pour Fusion tables
######################################################################

## On charge le fichier rgc.csv, qui contient des données pour les communes
## de France tirées du RGC.

rgc <- read.csv("data/rgc.csv")
  
tmp.rgc <- rgc[,c("id","long","lat")]
tmp.d <- d[,c("nom","id.com", "inscr", vars)]
tmp.d[,vars] <- round(tmp.d[,vars],1)
tmp.d$groupes <- groupes.brut

## On ajoute les écarts à la moyenne nationale 
tmp.dv <- d[,vars]
moyennes <- c(17.9, 27.18, 1.79, 9.13, 28.63, 2.31, 11.1, 1.15, 0.56, 0.25, 1.92, 20.52)
tmp.dv <- sweep(tmp.dv, 2, moyennes)
names(tmp.dv) <- paste0("ecart.", names(tmp.dv))
tmp.dv <- round(tmp.dv,1)
## On fusionne les deux
tmp.d <- cbind(tmp.d, tmp.dv)

geo <- merge(tmp.rgc, tmp.d, by.x="id", by.y="id.com", all.x=FALSE, all.y=FALSE)

## Noms des marqueurs pour les différents groupes
## geo$marker[geo$groupes==1] <- "ltblu_circle"
## geo$marker[geo$groupes==2] <- "wht_circle"
## geo$marker[geo$groupes==3] <- "grn_circle"
## geo$marker[geo$groupes==4] <- "red_circle"
## geo$marker[geo$groupes==5] <- "orange_circle"
## geo$marker[geo$groupes==6] <- "pink_circle"
## geo$marker[geo$groupes==7] <- "ylw_circle"
## geo$marker[geo$groupes==8] <- "blu_circle"

geo$marker[geo$groupes==1] <- "measle_turquoise"
geo$marker[geo$groupes==2] <- "measle_grey"
geo$marker[geo$groupes==3] <- "small_green"
geo$marker[geo$groupes==4] <- "small_red"
geo$marker[geo$groupes==5] <- "measle_brown"
geo$marker[geo$groupes==6] <- "small_purple"
geo$marker[geo$groupes==7] <- "small_yellow"
geo$marker[geo$groupes==8] <- "small_blue"

            

## Export CSV pour import dans Fusion table
write.csv(geo, file="results/export_fusion_table.csv")
  
## Pour mémoire, code HTML pour l'affichage des info-window
## dans Google Maps :
##
## <div class='googft-info-window' style='font-family: sans-serif'>
## <h3>{nom}</h3>
## <p><b>Groupe {groupes}</b></p>
## <table>
## <tr><td>Le Pen</td><td>{lepen}%</td></tr>
## <tr><td>Sarkozy</td><td>{sarkozy}%</td></tr>
## <tr><td>Dupont-Aignan</td><td>{dupont_aignan}%</td></tr>
## <tr><td>Bayrou</td><td>{bayrou}%</td></tr>
## <tr><td>Hollande</td><td>{hollande}%</td></tr>
## <tr><td>Joly</td><td>{joly}%</td></tr>
## <tr><td>Mélenchon</td><td>{melenchon}%</td></tr>
## <tr><td>Poutou</td><td>{poutou}%</td></tr>
## <tr><td>Arthaud</td><td>{arthaud}%</td></tr>
## <tr><td>Cheminade</td><td>{cheminade}%</td></tr>
## <tr><td>Blancs et nuls</td><td>{blancs}%</td></tr>
## <tr><td>Abstention</td><td>{abst}%</td></tr>
## </table>
## </div>
