######################################################################
####Extensions
######################################################################


library(stringr)
library(ggplot2)
library(questionr)
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

## Dendrogramme
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
freq(groupes.brut)

  


#####Description des classes
######################################################################



vars <- c("lepen","sarkozy","dupont_aignan","bayrou","hollande",
          "joly","melenchon","poutou","arthaud","cheminade",
          "blancs","abst")
labels <- c("Le Pen","Sarkozy","Dupont-Aignan","Bayrou","Hollande",
            "Joly","Mélenchon","Poutou","Arthaud","Cheminade",
            "Blancs","Abstention")

## Moyennes des pourcentages pour l'ensemble des communes
## Dans le même ordre que vars et labels.
tmp <- d[,vars]
moyennes.nat <- apply(tmp, 2, mean)



## Graphique comparant les moyennes pour chaque variable et pour chaque
## groupe. Seuls les écarts supérieurs à 0.5 points en valeur absolue
## et significatif au seuil de 0.001 sont affichés.

m.brut <- diffmatrix(vars, groupes.brut, seuil=0.001, levels=vars, labels=labels)

png(file=paste0(out.path,"ccp_diffmatrix.png"), width=600, height=500)
diffmatrix.plot(m.brut, seuil.diff=0.1,
                levels=vars, labels=labels,
                title="Description des groupes")
dev.off()


## Répartition du vote Le Pen dans les différents groupes
# grplot("lepen", groupes.brut)

## Calcul des variables centrées (on leur soustrait
## leur moyenne nationale)
dv <- d[,vars]
dv <- sweep(dv, 2, moyennes.nat)
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

## On charge le fichier rgc.csv, qui contient des données pour les communes
## de France tirées du RGC.

rgc <- read.csv("data/rgc.csv")
  
tmp.rgc <- rgc[,c("id","long","lat")]
tmp.d <- d[,c("nom","id.com", "inscr", vars)]
tmp.d[,vars] <- round(tmp.d[,vars],1)
tmp.d$groupes <- groupes.brut

## On ajoute les écarts à la moyenne nationale 
tmp.dv <- d[,vars]
tmp.dv <- sweep(tmp.dv, 2, moyennes.nat)
names(tmp.dv) <- paste0("ecart.", names(tmp.dv))
tmp.dv <- round(tmp.dv,1)
## On fusionne les deux
tmp.d <- cbind(tmp.d, tmp.dv)

geo <- merge(tmp.rgc, tmp.d, by.x="id", by.y="id.com", all.x=FALSE, all.y=FALSE)

couleurs <- c("#FCF457",
              "#99FF99",
              "#BBBBBB",
              "#FF99FF",
              "#99FFFF", 
              "#FC6565",
              "#5781FC",
              "#CC9966")


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


## Noms des marqueurs pour les différents groupes
geo$marker[geo$groupes==1] <- "small_yellow"
geo$marker[geo$groupes==2] <- "small_green"
geo$marker[geo$groupes==3] <- "measle_grey"
geo$marker[geo$groupes==4] <- "small_purple"
geo$marker[geo$groupes==5] <- "measle_turquoise"
geo$marker[geo$groupes==6] <- "small_red"
geo$marker[geo$groupes==7] <- "small_blue"
geo$marker[geo$groupes==8] <- "measle_brown"
            

## Export CSV pour import dans Fusion table
write.csv(geo, file="results/export_fusion_table.csv")
  
## Pour mémoire, code HTML pour l'affichage des info-window
## dans Google Maps :
##
## <div class='googft-info-window' style='font-family: sans-serif'>
## <h3>{nom}</h3>
## <p><b>Groupe {groupes}</b></p>
## <table>
## <tr><td><em>Inscrits</em></td><td style="text-align:left;">{inscr}</td><td></td></tr>
## <tr><td><em>Abstention</em></td><td style="text-align:right;">{abst}% </td><td style="text-align:right;"><small>({ecart.abst})</small></td></tr>
## <tr><td><em>Blancs et nuls</em></td><td style="text-align:right;">{blancs}% </td><td style="text-align:right;"><small>({ecart.blancs})</small></td></tr>
## <tr><td></td><td style="text-align:right;"></td></tr>
## <tr><td><em>Hollande</em></td><td style="text-align:right;">{hollande}% </td><td style="text-align:right;"><small>({ecart.hollande})</small></td></tr>
## <tr><td><em>Sarkozy</em></td><td style="text-align:right;">{sarkozy}% </td><td style="text-align:right;"><small>({ecart.sarkozy})</small></td></tr>
## <tr><td><em>Le Pen</em></td><td style="text-align:right;">{lepen}% </td><td style="text-align:right;"><small>({ecart.lepen})</small></td></tr>
## <tr><td><em>Mélenchon</em></td><td style="text-align:right;">{melenchon}% </td><td style="text-align:right;"><small>({ecart.melenchon})</small></td></tr>
## <tr><td><em>Bayrou</em></td><td style="text-align:right;">{bayrou}% </td><td style="text-align:right;"><small>({ecart.bayrou})</small></td></tr>
## <tr><td><em>Joly</em></td><td style="text-align:right;">{joly}% </td><td style="text-align:right;"><small>({ecart.joly})</small></td></tr>
## <tr><td><em>Dupont-Aignan</em></td><td style="text-align:right;">{dupont_aignan}% </td><td style="text-align:right;"><small>({ecart.dupont_aignan})</small></td></tr>
## <tr><td><em>Poutou</em></td><td style="text-align:right;">{poutou}% </td><td style="text-align:right;"><small>({ecart.poutou})</small></td></tr>
## <tr><td><em>Arthaud</em></td><td style="text-align:right;">{arthaud}% </td><td style="text-align:right;"><small>({ecart.arthaud})</small></td></tr>
## <tr><td><em>Cheminade</em></td><td style="text-align:right;">{cheminade}% </td><td style="text-align:right;"><small>({ecart.cheminade})</small></td></tr>
## </table>
## </div>    
