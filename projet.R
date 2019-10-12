library(shiny)
library(ggplot2)
library(UsingR)
library(reshape)


source('UI.R', local = TRUE)
source('Server.R')

shinyApp(ui = ui, server = server)
