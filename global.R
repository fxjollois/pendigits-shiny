load("pendigits-resultats.RData")


## Dessin d'un chiffre
dessineChiffre <- function(v, titre = "", indice = F, new = TRUE, tcol = "gray20", ep = 1) {
    vv = as.vector(as.matrix(v))
    x = vv[seq(1, 15, by = 2)]
    y = vv[seq(2, 16, by = 2)]
    if (new) 
        plot(NULL, NULL, 
             xlim = c(0, 100), ylim = c(0, 100), 
             xlab = "", ylab = "",
             bty = "n", axes = "F", main = titre)
    lines(x, y, col = tcol, type = "b", pch = "", lwd = ep)
    if (indice)
        text(x, y, labels = 1:8, cex = 1.5, font = 2)
}
# dessineChiffre(pen.tra[1,], "Exemple : ici un 8", indice = T)

## Dessin des deux
dessineOrigNorm <- function(i) {
    trace = pen.orig[[i]]$trace
    x1 = trace[[1]][,1]
    y1 = trace[[1]][,2]
    x = x1
    y = y1
    if (length(trace) == 2) {
        x2 = trace[[2]][,1]
        y2 = trace[[2]][,2]
        x = c(x1, x2)
        y = c(y1, y2)
    }
    norm <- function (v, min, max) { return ((v - min) / (max - min) * 100) }
    plot(NULL, NULL, 
         xlim = c(0, 100), ylim = c(0, 100), 
         bty = "n", axes = "F", 
         xlab = "", ylab = "",
         main = pen.orig[[i]]$chiffre)
    lines(norm(x1, min(x), max(x)), norm(y1, min(y), max(y)), col = "red", lwd = 2)
    if (length(trace) == 2) {
        lines(norm(x2, min(x), max(x)), norm(y2, min(y), max(y)), col = "green", lwd = 2)
    }
    dessineChiffre(pen[i,], indice = F, new = F)
}
# dessineOrigNorm(7)



## ?
TypeEcriture <- function(chiffre, modele) {
    sub = pen[pen$chiffre == chiffre, - 17]
    z = modele$classification 
    nb = modele$G
    centres = t(modele$parameters$mean)
    par(mfrow = c(2, nb), mar = c(0, 0, 2, 0) + 0.1)
    for (l in 1:nb)
        dessineChiffre(centres[l,], paste(chiffre, l, sep = " : classe "), T)
    for (l in 1:nb)
        plot(acp$ind$coord[pen$chiffre == chiffre, 1:2][z == l,], 
             pch = 19, cex = 0.5, xaxt = "n", yaxt = "n",
             col = rainbow(10)[chiffre + 1], xlim = c(-5, 4), ylim = c(-4, 5))
}
