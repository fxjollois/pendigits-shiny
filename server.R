library(shiny)
library(mclust)

shinyServer(function(input, output) {
    
    # Données
    output$trace <- renderPlot({
        par(mfrow = c(1, 3))
        dessineOrigNorm(1)
        dessineOrigNorm(15)
        dessineOrigNorm(9)
    })
    output$premier <- renderPlot({
        par(mfrow = c(2, 5), mar = c(0, 0, 2, 0) + 0.1)
        for (ch in 0:9) {
            ex = pen[pen$chiffre == ch,1:16][1,]
            dessineChiffre(ex, ch, indice = T)
        }
    })
    output$moyen <- renderPlot({
        cmoy = apply(pen[,1:16], 2, tapply, pen$chiffre, mean)
        par(mfrow = c(2, 5), mar = c(0, 0, 2, 0) + 0.1)
        for (i in 1:10) {
            #sub = subset(pen, subset = chiffre == i-1)
            #dessineChiffre(sub[1,], i-1, tcol = "gray90")
            #for (j in 2:nrow(sub)) {
            #    dessineChiffre(sub[j,], tcol = "gray90", new = F)
            #}
            dessineChiffre(cmoy[i,], i-1, tcol = "black", new = T, ep = 2, indice = T)
        }
    })
    
    # Visualisation
    output$acp <- renderPlot({
        plot(acp$ind$coord[,1:2], pch = 19, cex = 0.75, font.sub = 3, cex.sub = 0.8,
             col = rainbow(10)[pen$chiffre + 1], xlim = c(-5, 4), ylim = c(-4, 5),
             main = paste(round(acp$eig[2,3], 1), "% d'inertie expliquée"),
             xlab = paste(round(acp$eig[1,2], 1), "%"),
             ylab = paste(round(acp$eig[2,2], 1), "%"),
             sub = "Le premier plan factoriel représente ici 49% de l'information")
        legend("bottom", ncol = 10, pch = 19, col = rainbow(10), legend = 0:9, cex = 0.8, bty = "n")
    })
    output$acp.decoup <- renderPlot({
        par(mfrow = c(2, 5), mar = c(0, 0, 2, 0) + 0.1)
        for (ch in 0:9) {
            plot(acp$ind$coord[pen$chiffre == ch,1:2], 
                 pch = 19, cex = 0.5, xaxt = "n", yaxt = "n",
                 col = rainbow(10)[ch + 1], xlim = c(-5, 4), ylim = c(-4, 5),
                 main = ch)
        }
    })
    
    # Classification
    output$evolution <- renderPlot({
        if (input$critere == "BIC") {
            plot(resclustBIC[[as.numeric(input$chiffre) + 1]], what = "BIC")
        } else {
            plot(resclustICL[[as.numeric(input$chiffre) + 1]])
        }
    })
    output$classif <- renderPlot({
        ch = as.numeric(input$chiffre)
        if (input$critere == "BIC") {
            TypeEcriture(ch, modclustBIC[[ch + 1]])
        } else {
            TypeEcriture(ch, modclustICL[[ch + 1]])
        }
    })
})