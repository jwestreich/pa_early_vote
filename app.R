library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

df <- read_csv("https://raw.githubusercontent.com/jwestreich/pa_early_vote/main/data.csv") %>%
  mutate(date = mdy(date)) %>%
  mutate(votes_total = votes_d + votes_r + votes_i) %>%
  mutate(split_d = votes_d / votes_total,
         split_r = votes_r / votes_total,
         split_i = votes_i / votes_total) %>%
  mutate(edge_d = returned_d - returned_r) %>%
  mutate(firewall = votes_d - votes_r) %>%
  mutate(firewall_label = format(firewall, big.mark = ","),
         edge_d_label = paste0(edge_d * 100, "%"))

ui <- fluidPage(
  titlePanel("Pennsylvania Early Voting Firewall Tracker"),
  mainPanel(
    plotlyOutput("firewallPlot", height = "600px"),
    tags$div(
      style = "text-align: left; margin-top: 20px;",
      "Thank you to ", 
      tags$a(href = "https://twitter.com/blockedfreq", "Joshua Smithley", target = "_blank")
    ),
    tags$div(
      style = "text-align: left; margin-top: 10px;",
      "Data available at ", 
      tags$a(href = "https://www.pa.gov/en/agencies/vote/elections/elections-data.html", "Pennsylvania Agency of Voting & Elections", target = "_blank")
    )
  )
)


server <- function(input, output) {
  output$firewallPlot <- renderPlotly({
    p <- ggplot(data = df, aes(x = date)) +
      geom_line(aes(y = firewall), color = "blue", size = 1.5) +
      geom_line(aes(y = firewall, text = paste("Date:", date, "<br>Firewall:", firewall_label, "<br>D Return Edge:", edge_d_label)), 
                color = "blue", size = 1.5) +
      geom_line(aes(y = firewall_target), color = "black", linetype = "dashed", size = 1.5) +
      scale_y_continuous(limits = c(0, 500000), breaks = seq(0, 500000, 100000), labels = scales::comma) +
      labs(x = "Date", y = "Harris Firewall") +
      annotate("text", x = max(df$date) - 4, y = 420000, label = "Firewall Needed to Feel\nDecent on ED: 390,000", hjust = 1, size = 5) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    ggplotly(p, tooltip = c("text"))
  })
}

shinyApp(ui = ui, server = server)