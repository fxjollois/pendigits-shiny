library(FactoMineR)
library(mclust)

## Données d'origine -> transformation en liste
pen.orig.tra = readLines("data/pendigits-orig.tra", n = 856)
pen.orig = list()
i = 1
while (i < length(pen.orig.tra)) {
    l = pen.orig.tra[i]
    if (substr(l, 1, 8) == ".SEGMENT") {
        # cat(l, "\t ** ", length(pen.orig), "\n")
        trace = list()
        nouveau = list(chiffre = strsplit(pen.orig.tra[i+1], " ")[[1]][2])
        i = i + 2
        l = pen.orig.tra[i]
        while (i < length(pen.orig.tra) & substr(l, 1, 8) != ".SEGMENT") {
            if (substr(l, 1, 9) == ".PEN_DOWN") {
                i = i + 1
                l = pen.orig.tra[i]
                v = as.numeric(strsplit(trimws(l), "  ")[[1]])
                t = data.frame(x = v[1], y = v[2])
            } else {
                if (substr(l, 1, 9) == ".PEN_UP") {
                    trace[[length(trace)+1]] = t
                    t = NULL
                } else {
                    v = as.numeric(strsplit(trimws(l), "  ")[[1]])
                    t = rbind(t, v)
                }
            }
            i = i + 1
            l = pen.orig.tra[i]
        }
        i = i - 1
        nouveau$trace = trace
        pen.orig[[length(pen.orig)+1]] = nouveau
    }
    i = i + 1
}
rm(i, l, nouveau, t, trace, v)



## Données normalisées à utiliser
pen.tra = read.table("data/pendigits.tra", sep = ",")
names(pen.tra) = c(paste(c("X", "Y"), rep(1:8, each = 2), sep = ""), "chiffre")
pen.tes = read.table("data/pendigits.tes", sep = ",")
names(pen.tes) = c(paste(c("X", "Y"), rep(1:8, each = 2), sep = ""), "chiffre")
pen = rbind(pen.tra, pen.tes)

# ACP
acp = PCA(pen, quali.sup = 17, graph = F)

# mclust
resclustBIC = list()
modclustBIC = list()
resclustICL = list()
modclustICL = list()
for (c in 0:9) {
    sub = subset(pen, chiffre == c, select = -chiffre)

    resclustBIC[[c+1]] = mclustBIC(sub)
    modclustBIC[[c+1]] = Mclust(sub, x = resclustBIC[[c+1]])

    ICL = mclustICL(sub)
    resclustICL[[c+1]] = ICL
    modele = names(which.max(apply(ICL, 2, max, na.rm = TRUE)))
    nbclasses = names(which.max(apply(ICL, 1, max, na.rm = TRUE)))
    modclustICL[[c+1]] = Mclust(sub, modelNames = modele, G = nbclasses)
}
rm(c)


save.image("pendigits-resultats.RData")