ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Choose CSV File",accept = c("text/plain", ".csv")),
      actionButton(inputId = "go", label = "Load", icon("download")),
      fluidRow(
      column(3,
             # Buton de mise à jour de la liste rv
             actionButton(inputId = "frequences", label = "fréquences")),
      column(6,
             # Buton de mise à jour de la liste rv
             actionButton(inputId = "effectifs", label = "effectifs"))),
      hr(),
      selectInput("dimensionSize", "Select Dimension",c("Unidimentional","Bidimentional","Multidimentional")),
      uiOutput(outputId = "checkbox", label = "Select Columns")
      #,submitButton("Apply Changes", icon("redo-alt"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("table", dataTableOutput("table"), style = "font-size: 85%"),
        tabPanel("Nuage de points", 
                 fluidRow(
                   column(8, offset = 1, plotOutput("nuagePoints"))
                 ),
                 fluidRow(
                   column(4, offset = 3, textOutput("correlation"))
                  )
        ),
        tabPanel("Nuage de points et Histogrammes",plotOutput("nuagePointshist")),
        tabPanel("Boîtes parallèles", 
                 fluidRow(
                   column(6, plotOutput("boxplotBasic")),
                   column(6, plotOutput("boxplotGgplot"))
                 )),
        tabPanel("Diag. Barres (2 var.)", 
                 fluidRow(
                   column(6, plotOutput("barplotBi")),
                   column(6, plotOutput("barplotDodgeBi"))
                 )
        ),
        tabPanel("Summary",
                 fluidRow(
                    column(12,verbatimTextOutput(outputId = "summary"))
                   
                 )
                 
                 ),
        tabPanel("histograme",
                 fluidRow(
                   column(6,
                          # Zone d'affichage de l'histogramme
                          plotOutput(outputId = "hist"))
                 )),
        tabPanel("Boite à Moustache",
                 fluidRow(
                   column(12, 
                          # Zone d'affichage de la boîte à moustaches
                          plotOutput(outputId = "Boite"))
                   
                 )
                 
        )
                #  fluidRow(
                #    column(4, offset = 4, textOutput("correlation"))
                #  )
      ),
      
      
      
      style = "font-size: 75%"
    )
  )
)