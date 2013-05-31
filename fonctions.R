######################################################################
####Fonctions
######################################################################

## Affiche la répartition des votes pour les différents groupes
## pour un candidat sous forme de boxplot et de violin plot

grplot <- function (varname, vgroupes, title="") {
    tmp <- data.frame(groupes=vgroupes, var=d[,varname])
    ggplot(data=tmp, aes(x=groupes, y=var))  +
        geom_hline(yintercept=median(tmp$var), colour="red") +
        geom_violin(aes(fill=groupes, colour=groupes)) +
        geom_boxplot(aes(fill=groupes), outlier.colour=NA, width=0.2) +
        labs(title=varname) + theme(legend.position="none") +
        xlab("Groupe") + ylab("Pourcentage") +
        scale_fill_brewer(palette="Set3") + scale_colour_brewer(palette="Set3")
}


## Pour chaque groupe, calcul l'écart entre la moyenne de var
## dans ce groupe et la moyenne globale de var, fait un test t
## et conserve le résultat seulement si sa significativité est
## inférieure à seuil

difftests <- function(var, groupes, seuil=0.05) {
  v <- sapply(levels(groupes), function(i) {
    var.values <- var[groupes==i]
    diff.mean <- round(mean(var.values)-mean(var),2)
    if (length(var.values)<2) p.mean <- 0
    else p.mean <- t.test(var ~ groupes==i)$p.value
    if (p.mean > seuil) diff.mean <- NA
    return(diff.mean) })        
  names(v) <- levels(groupes)
  v
}

## Construit une matrice en agrégeant les difftests pour chaque
## variable de vars

diffmatrix <- function(vars, groupes, seuil=0.05, levels, labels) {
  m <- sapply(vars, function(name) {
    difftests(d[,name], groupes, seuil=seuil)
    })
  t(m)
}

## Représentation graphique d'une matrice calculée avec diffmatrix

diffmatrix.plot <- function(m, seuil.diff=0, levels, labels, title=NULL) {
  mm <- melt(m)
  mm$Var1 <- factor(mm$Var1, levels=vars, labels=labels)
  mm$value[abs(mm$value) < seuil.diff] <- NA
  ggplot(data=subset(mm, !is.na(value))) +
    geom_tile(aes(x=Var1, y=Var2,fill=value), colour="white") +
    scale_fill_gradient2(low="blue", mid="white", high="red") +
    geom_text(aes(x=Var1, y=Var2,label=value), size=2.5) +
    scale_y_continuous(breaks=1:max(mm$Var2)) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    xlab("") + ylab("Groupe") + labs(title=title)
}

## Affiche la répartition de la densité pour chaque variable
## pour un groupe donné

groupes.density <- function(df, groupe) {
  tmp <- df[df$groupes==groupe,]
  tmp$variable <- factor(tmp$variable, levels=vars, labels=labels)
  ggplot(data=tmp) +
    geom_density(aes(x=value, fill=variable, color=variable, alpha=0.6)) +
    geom_vline(xintercept=0, linetype=2) +
    facet_grid(variable~., scales="free") +
    scale_x_continuous(limits=c(-23,23), name="Écart à la moyenne nationale") +
    scale_y_continuous(breaks=NULL, name="") +
    theme(legend.position="none", strip.text.y=element_text(size=11,angle=0)) +
    labs(title=paste("Groupe", groupe, sep=" ")) +
    theme(plot.margin=unit(c(2, 1, 2, 0), "lines")) +
    theme(axis.title.x=element_text(vjust=-1,size=16), plot.title=element_text(vjust=2, size=20))
}


## Affiche une carte de France avec la position des communes
## du groupe selon la couleur des marqueurs de Google, et
## superpose des lignes de niveau de densité.


groupe.carte <- function(geo, groupe, couleurs) {
tmp <- geo[geo$groupes==groupe,]
ggplot(data=tmp, aes(long,lat)) +
  borders("france", size=0.3, colour="grey30") +
  geom_point(color=couleurs[groupe],size=1.5) +
  geom_density2d(color="black") +
  theme_bw() +
  coord_map() +
  labs(title=paste("Groupe",groupe,sep=" ")) +
  theme(legend.position="none",
       axis.ticks = element_blank(), 
       axis.title.y = element_blank(), 
       axis.text.y =  element_blank(),
       axis.title.x = element_blank(), 
       axis.text.x =  element_blank()) 
}
