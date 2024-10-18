library(shiny)
library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggiraph)
library(sf)
options(scipen=999)


county_shapes <- tigris::counties(state = "PA", class = "sf")%>%
  mutate(county_name=toupper(NAME))%>%
  select(county_name,geometry)

download.file("https://www.pa.gov/content/dam/copapwp-pagov/en/dos/resources/voting-and-elections/voting-and-election-statistics/currentvotestats.xls", destfile = "temp_report1.xls", mode = "wb")
county_voters<-read_excel("temp_report1.xls", sheet = "Reg Voter", skip = 1)%>%
  select(CountyName,DemVoters=Dem,RepVoters=Rep,NoAffVoters=`No Aff`,OtherVoters=Other,TotalVoters=`Total Count of All Voters`)%>%
  mutate(CountyName=toupper(CountyName))%>%
  filter(CountyName!="TOTALS:")

static_file<-county_shapes%>%
  full_join(county_voters, by=c("county_name"="CountyName"))

# st_write(static_file, "static.gpkg")
# st_write(static_file, "static.shp")