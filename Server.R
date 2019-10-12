server <- function(input, output){
  data <- eventReactive(input$go, {
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
  
  # Récupération des valeurs fecondite
  valeurFinal <- reactive({
    valeurRecup<- melt(data(), measure.vars = input$unid )
    valeurRecup$value
  })
  
  
  output$nuagePoints <- renderPlot({
    validate(
      need(sapply(data(), class)[input$bid1] != "factor" && sapply(data(), class)[input$bid2] != "factor"
           , label = "Only Quantitative variables")
    )
    options(scipen=999)
    x.var = input$bid1; y.var = input$bid2;
    plot(x = data()[, x.var], y = data()[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    # Droite de régression linéaire (y~x) 
    abline(lm(data()[, y.var]~data()[, x.var]), col="red", lwd = 2)
    options(scipen=0)
  })
  
  output$correlation <- renderText({
    coeff.tmp <- cov(data()[, input$bid1], data()[, input$bid2])/(sqrt(var(data()[, input$bid1])*var(data()[, input$bid2])))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })

  output$nuagePointshist <- renderPlot({
    validate(
      need(sapply(data(), class)[input$bid1] != "factor" 
           && sapply(data(), class)[input$bid2] != "factor"
           , label = "Only Quantitative variables")
    )
    options(digits=1)
    V1 = data()[, input$bid1] 
    V2 = data()[, input$bid2]
    scatter.with.hist(V1, V2)
  })
  
  # Boîtes parallèles
  # ----
  output$boxplotBasic <- renderPlot({
    need(sapply(data(), class)[input$multd] != "factor" 
         , label = "Only Quantitative variables")
    
    # Reshape data()
    data.stack <- melt(data(), measure.vars = input$multd )
    # Boxplot basique
    boxplot(data.stack$value ~ data.stack$variable , col="grey",
            xlab = "Modalités", ylab = "Mesures")
  })

  output$boxplotGgplot <- renderPlot({
    # Reshape data()
    data.stack <- melt(data(), measure.vars = input$multd )
    # Boxplot élaborée
    qplot(x = data.stack[,1], y = data.stack[,2], 
          xlab = "Modalités", ylab = "Mesures",
          geom=c("boxplot", "jitter"), fill=data.stack[,1]) +
      theme(legend.title=element_blank())
  })
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = data()[,input$bid1], fill = data()[,input$bid2])) + geom_bar()
  })

  output$barplotDodgeBi <- renderPlot({
    # Diagramme de profils entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = data()[,input$bid1], fill = data()[,input$bid2])) + geom_bar(position = "dodge")
  })

  # Calcul et affichage le rapport de corrélation
  # ---
  # output$correlation <- renderText({
  #   # Calcul de la variance expliquée
  #   tmp.mean.y = mean(as.vector(as.matrix(data())))
  #   tmp.mean.yr = apply(data(), MARGIN = 2, mean)
  #   tmp.nl = rep(nrow(data()), 4)
  #   sE2 = (1/sum(tmp.nl))*sum(tmp.nl*(tmp.mean.yr-tmp.mean.y)^2)
  #   # Calcul de la variance résiduelle
  #   tmp.var.yr = apply(data(), MARGIN = 2, var)
  #   sR2 = (1/sum(tmp.nl))*sum(tmp.nl*tmp.var.yr)
  #   # Calcul du rapport de corrélation
  #   rCor = sqrt(sE2/(sE2+sR2))
  #   paste("\n\nRapport de corrélation =", round(rCor, digits = 2))
  #   print(sE2)
  #   print(sR2)
  # })
  
  
  output$table <- renderDataTable({data()})
  
  output$checkbox <- renderUI({
    choice <- colnames(data())
    if(input$dimensionSize == 'Unidimentional')
      choices <- radioButtons(inputId = "unid", label = "Select variable", choices = choice)
    else 
      if(input$dimensionSize == 'Bidimentional'){
        choices <- list(
          selectInput(inputId = "bid1", label = "Select first variable", choices = choice),
          selectInput(inputId = "bid2", label = "Select second variable", choices = choice)
        )
      }
    else
      choices <- checkboxGroupInput("multd","Select variable(s)", choices = choice)
    list(hr(),choices)
  })
  # Commande pour le calcul du summary
  output$summary <- renderPrint({ t(summary(data())) })
  
  
  
  # Commande pour l'affichage de la boîte à moustaches des radio boutons
  output$Boite <- renderPlot({
    data.stack <- melt(data(), measure.vars = input$unid )
    
    # Boîte à moustaches
    boxplot( data.stack$value, col = grey(0.8), las = 1)
  })
  
  
  rv <- reactiveValues(hist_isFreq = TRUE, 
                       hist_yLabel = "Effectifs", 
                       hist_col = "blue")
  # On observe les clicks
  observeEvent(input$effectifs,{
    rv$hist_isFreq <- TRUE; 
    rv$hist_yLabel <- "Effectifs"; 
    rv$hist_col <- "blue";
  })
  observeEvent(input$frequences,{
    rv$hist_isFreq <- FALSE; 
    rv$hist_yLabel <- "Densité de fréquences";
    rv$hist_col <- "green"
  })
  
  # Histogramme
  # ----
  output$hist <- renderPlot({
    hist(valeurFinal, freq = rv$hist_isFreq, cex.axis = 1.5, cex.main = 1.5,
         main = "Histogramme de l'indice", col = rv$hist_col,
         xlab = "Indice de x", ylab = rv$hist_yLabel, las = 1,
         breaks = seq(0.8, 3, by = 0.2), right = FALSE, cex.lab = 1.5)
  })
  
  
}



