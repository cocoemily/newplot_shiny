#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(shiny)){install.packages("shiny")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggrepel)){install.packages("ggrepel")}
if(!require(rjson)){install.packages("rjson")}
if(!require(DT)){install.packages("DT")}
if(!require(shinythemes)){install.packages("shinythemes")}
if(!require(gtools)){install.packages("gtools")}
if(!require(ggthemes)){install.packages("ggthemes")}
if(!require(here)){install.packages("here")}
if(!require(shinyFiles)){install.packages("shinyFiles")}

library(shiny)
library(tidyverse)
library(ggrepel)
library(rjson)
library(DT)
library(shinythemes)
library(gtools)
library(ggthemes)
library("shinyFiles")


theme_set(theme_bw())

source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)
