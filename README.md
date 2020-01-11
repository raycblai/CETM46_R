# CETM46_R
CETM46 R + Shiny + Dashboard by Raymond Lai

Instruction

1. To run the R script locally, please make sure you download the app.R file, two csv files (winemag-data_first150k.csv, winemag-data-130k-v2.csv) which are the dataset for the wine.

2. Open the R Studio.

3. Please ensure you set the "working directory" with app.R and 2 csv files.

4. Please ensure you install the following R packages before running the app.R 

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(rpart)
library(tidyr)
library(tidyverse)
library(googleVis)
library(leaflet)
library(maps)
library(jpeg)
library(plotly)
library(DT)

5. Press the "Run app" button on the top right corner of the frame containing the R codes

The testing browser will be launched and Wine Recommendation Application will be running inside the browser.

--
Raymond Lai