options(shiny.trace = F)  

#test package dependency

#---base libraries
library(shiny)
library(shinydashboard)  
library(rhandsontable)       
library(shinysky) 
library(shinyBS)
library(shinydashboardPlus)  
library(shinycssloaders) 
library(shinyEffects)   
library(lubridate)  
library(plotly)  

#financial libraries
library(quantmod)	
library(lubridate)	
library(e1071)	
library(rpart)	
library(rpart.plot)	
library(ROCR)	

#shadow boxes
setShadow = shinyEffects::setShadow 

#source scrips
source("modules/ML_Comp.R",local=TRUE)   

 
#how to install base libraries which require devtools
#devtools::install_github(c('jrowen/rhandsontable','AnalytixWare/ShinySky','ropensci/plotly','RinteRface/shinydashboardPlus@v0.7.5'))"
#make sure the shinydashboarddPlus is version 0.7.5 as specified to be able to use gradient box