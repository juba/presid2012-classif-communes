library(flashClust)

## DONNÉES BRUTES

load("in/tmp_brut.rda")

dist.brut <- dist(tmp.brut)
hc.brut.ward <- hclust(dist.brut,method="ward")
save(hc.brut.ward, file="out/hc_brut_ward.rda")
rm(hc.brut.ward)
gc()

