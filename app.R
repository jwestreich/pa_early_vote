library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
options(scipen=999)

df <- read_csv("https://raw.githubusercontent.com/jwestreich/pa_early_vote/refs/heads/main/data.csv") %>%
  mutate(date = mdy(date)) %>%
  mutate(votes_total = votes_d + votes_r + votes_i) %>%
  mutate(split_d = votes_d / votes_total,
         split_r = votes_r / votes_total,
         split_i = votes_i / votes_total) %>%
  mutate(edge_d = returned_d - returned_r) %>%
  mutate(firewall = votes_d - votes_r) %>%
  mutate(firewall_label = format(firewall, big.mark = ","),
         edge_d_label = paste0(edge_d * 100, "%"))

df_long <- df %>%
  select(date, votes_d, votes_r, votes_i, split_d, split_r, split_i)%>%
  pivot_longer(cols = c(votes_d, votes_r, votes_i, split_d, split_r, split_i),
               names_to = c(".value", "party"),
               names_pattern = "(votes|split)_(d|r|i)")%>%
  mutate(party=ifelse(party=="d", "Democrat", party),
         party=ifelse(party=="r", "Republican", party),
         party=ifelse(party=="i", "Independent", party),
  )%>%
  select(date, party, split, votes)%>%
  mutate(split=ifelse(is.na(split),.00000000000000001,split))

ui <- fluidPage(
  titlePanel("Pennsylvania Early Vote Tracker"),
  mainPanel(
    tags$h3("Dem Firewall Tracker"),
    tags$h4("How many more votes have been returned from Democrats than Republicans"),
    plotlyOutput("firewallPlot", height = "600px"),
    
    tags$h3("Dem Return Rate Edge Tracker"),
    tags$h4("Percentage point difference between Democrat ballot return rate and Republican ballot return rate"),
    plotlyOutput("edgedPlot", height = "600px"),
    
    tags$h3("Vote By Mail Split Tracker"),
    plotlyOutput("vbmSplitPlot", height = "600px"),
    
    tags$div(
      style = "text-align: left; margin-top: 20px;",
      "Thank you to ", 
      tags$a(href = "https://twitter.com/blockedfreq", "Joshua Smithley", target = "_blank")
    ),
    tags$div(
      style = "text-align: left; margin-top: 10px;",
      "Data available at ", 
      tags$a(href = "https://www.pa.gov/en/agencies/vote/elections/elections-data.html", "Pennsylvania Agency of Voting & Elections", target = "_blank")
    ),
    tags$div(
      style = "text-align: left; margin-top: 10px;",
      "Website created by ", 
      tags$a(href = "https://jwestreich.github.io/Jays-Portfolio/", "Jay Westreich", target = "_blank")
    )
  )
)


server <- function(input, output) {
  output$firewallPlot <- renderPlotly({
    p1 <- ggplot(data = df, aes(x = date)) +
      geom_bar(aes(y = firewall), stat = "identity", fill = "blue", width = .9) +
      geom_bar(aes(y = firewall, text = paste("Date:", format(date, "%b %d"), "<br>Firewall:", firewall_label)), 
               stat = "identity", fill = "blue", width = .9) +
      geom_line(aes(y = firewall_target), color = "black", linetype = "dashed", size = 1.5) +
      scale_y_continuous(limits = c(0, 500000), breaks = seq(0, 500000, 100000), labels = scales::comma) +
      labs(x = "Date", y = "Harris Firewall") +
      annotate("text", x = min(df$date)+10, y = 420000, label = "Firewall Needed to Feel\nDecent on ED: 390,000", hjust = 0, size = 5) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    ggplotly(p1, tooltip = c("text"))
  })
  
  output$edgedPlot <- renderPlotly({
    p2 <- ggplot(data = df, aes(x = date)) +
      geom_line(aes(y = edge_d), color = "orange", size = 1.5) +
      geom_line(aes(y = edge_d, text = paste("Date:", format(date, "%b %d"), 
                                             "<br>D Return Rate: ", scales::percent(returned_d, accuracy = 0.1),
                                             "<br>R Return Rate: ", scales::percent(returned_r, accuracy = 0.1),
                                             "<br>D Return Edge: ", scales::percent(edge_d, accuracy = 0.1))), 
                color = "orange", size = 1.5) +
      scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 0.05), labels = scales::percent) +
      labs(x = "Date", y = "D Return Rate Edge") +
      theme_classic() +
      theme(legend.position = "none",
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    ggplotly(p2, tooltip = c("text"))
  })
  
  output$vbmSplitPlot <- renderPlotly({
    p3 <- ggplot(df_long, aes(x = date, y = split, fill = party, 
                              text = paste0("Party: ", party, "<br>",
                                            "Date: ", format(date, "%b %d"), "<br>",
                                            "Vote By Mail Split: ", scales::percent(split, accuracy = 0.1), "<br>",
                                            "Number of Votes : ", scales::comma(votes)))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("Republican" = "lightcoral", "Independent" = "lightgray", "Democrat" = "lightblue")) +
      scale_x_date(date_labels = "%b %d") +
      labs(x = "Date", y = "Vote By Mail Split", fill = "Party") +
      theme_classic() +
      theme(axis.text.x = element_text(color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.text = element_text(size = 12), 
            legend.title = element_text(size = 14))
    
    ggplotly(p3, tooltip = "text")
  })
  
}

shinyApp(ui = ui, server = server)
