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


pa_counties <- tigris::counties(state = "PA", class = "sf")%>%
  mutate(county_name=toupper(NAME))%>%
  select(county_name,geometry)

download.file("https://www.pa.gov/content/dam/copapwp-pagov/en/dos/resources/voting-and-elections/voting-and-election-statistics/currentvotestats.xls", destfile = "temp_report1.xls", mode = "wb")
county_data<-read_excel("temp_report1.xls", sheet = "Reg Voter", skip = 1)%>%
  select(CountyName,DemVoters=Dem,RepVoters=Rep,NoAffVoters=`No Aff`,OtherVoters=Other,TotalVoters=`Total Count of All Voters`)

download.file("https://www.pavoterservices.pa.gov/2024%20General%20Daily%20Mail%20Ballot%20Report.xlsx", destfile = "temp_report2.xlsx", mode = "wb")
map_data <- read_excel("temp_report2.xlsx", sheet = "Total", skip = 1)%>%
  mutate(across(2:9, as.numeric))%>%
  filter(CountyName!="TOTAL")%>%
  left_join(county_data,by=c("CountyName"))%>%
  clean_names()%>%
  mutate(county_name=toupper(county_name))%>%
  full_join(pa_counties,by=c("county_name"))%>%
  mutate(total_ballots_returned=ifelse(total_ballots_returned<5,0,total_ballots_returned),
         dem_ballots_returned=ifelse(dem_ballots_returned<5,0,dem_ballots_returned),
         rep_ballots_returned=ifelse(rep_ballots_returned<5,0,rep_ballots_returned))%>%
  mutate(ind_voters=no_aff_voters+other_voters)%>%
  mutate(dem_split=dem_ballots_returned/total_ballots_returned)%>%
  mutate(rep_split=rep_ballots_returned/total_ballots_returned)%>%
  mutate(dem_adv=dem_ballots_returned-rep_ballots_returned)%>%
  mutate(early_voters_perc=total_ballots_returned/total_voters)%>%
  mutate(return_rate=total_ballots_returned/total_applications_approved)%>%
  mutate(return_rate_dem=dem_ballots_returned/dem_applications_approved)%>%
  mutate(return_rate_rep=rep_ballots_returned/rep_applications_approved)%>%
  mutate(return_rate_ind=oth_ballots_returned/oth_applications_approved)%>%
  mutate(return_rate_adv=return_rate_dem-return_rate_rep)

splitPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = dem_split,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Dem Votes: ", scales::comma(dem_ballots_returned), "<br>",
                                                            "Rep Votes: ", scales::comma(rep_ballots_returned), "<br>",
                                                            "All Votes: ", scales::comma(total_ballots_returned), "<br>",
                                                            "Dem Split: ", scales::percent(dem_split, accuracy=.1), "<br>",
                                                            "Rep Split: ", scales::percent(rep_split, accuracy=.1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#009bda", midpoint = 0.5, name = "Dem Adv")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = splitPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))




demAdvPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = dem_adv,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Dem Votes: ", scales::comma(dem_ballots_returned), "<br>",
                                                            "Rep Votes: ", scales::comma(rep_ballots_returned), "<br>",
                                                            "Dem Advantage: ", scales::comma(dem_adv))),
                      color = "black", size = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#009bda", midpoint = 0, name = "Dem Adv")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = demAdvPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

totalVoterPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = total_voters,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Dem Registered Voters: ", scales::comma(dem_voters), "<br>",
                                                            "Rep Registered Voters: ", scales::comma(rep_voters), "<br>",
                                                            "Total Registered Voters: ", scales::comma(total_voters))),
                      color = "black", size = 0.2) +
  scale_fill_gradient(low = "yellow", high = "#9139fe", name = "Number of Registered Voters")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = totalVoterPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

evPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = early_voters_perc,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Early Vote Ballots Returned: ", scales::comma(total_ballots_returned), "<br>",
                                                            "Registered Voters: ", scales::comma(total_voters), "<br>",
                                                            "Percent Voted Early: ", scales::percent(early_voters_perc, accuracy=.1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "green", name = "Number of Registered Voters")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = evPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

returnRatePlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = return_rate,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Early Vote Ballots Returned: ", scales::comma(total_ballots_returned), "<br>",
                                                            "Early Vote Ballot Applications: ", scales::comma(total_applications_approved), "<br>",
                                                            "Return Rate: ", scales::percent(return_rate, accuracy=.1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "orange", name = "Number of Registered Voters")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = returnRatePlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

returnRateDPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = return_rate,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Dem Early Vote Ballots Returned: ", scales::comma(dem_ballots_returned), "<br>",
                                                            "Dem Early Vote Ballot Applications: ", scales::comma(dem_applications_approved), "<br>",
                                                            "Dem Return Rate: ", scales::percent(return_rate_dem, accuracy=.1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "#009bda", name = "Number of Registered Voters")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = returnRateDPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

returnRateRPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = return_rate,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Rep Early Vote Ballots Returned: ", scales::comma(rep_ballots_returned), "<br>",
                                                            "Rep Early Vote Ballot Applications: ", scales::comma(rep_applications_approved), "<br>",
                                                            "Rep Return Rate: ", scales::percent(return_rate_rep, accuracy=.1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "red", name = "Number of Registered Voters")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = returnRateRPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))

returnRateAdvPlot <- ggplot() +
  geom_sf_interactive(data = map_data, aes(geometry = geometry,
                                           fill = return_rate_adv,
                                           tooltip = paste0(county_name, "<br>",
                                                            "Dem Return Rate: ", scales::percent(return_rate_dem, accuracy = .1), "<br>",
                                                            "Rep Return Rate: ", scales::percent(return_rate_rep, accuracy = .1), "<br>",
                                                            "Dem Return Rate Advantage: ", scales::percent(return_rate_adv, accuracy = .1))),
                      color = "black", size = 0.2) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#009bda", midpoint = 0, name = "Dem Adv")+
  theme_void() +
  theme(legend.position = "none")

girafe(ggobj = returnRateAdvPlot, options = list(
  opts_tooltip(css = "font-family: sans-serif; color: white; background-color: black;")
))
