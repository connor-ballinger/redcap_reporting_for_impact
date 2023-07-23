

library(shiny)
library(REDCapR)
library(tidyverse)
library(labelled)
library(ggeasy)
library(here)
library(DT)
library(plotly)

theme_set(theme_bw() + theme(legend.position = "bottom"))

plot_survey <- function(data, question) {
  ggplot(data = data) +
    geom_bar(aes(.data[[question]]), fill = "#23285E") +
    labs(title = var_label(data[, question]), x = "", y = "") +
    scale_x_discrete(drop = FALSE, na.translate = FALSE) +
    easy_labs()
}

# set up redcap api
# api key is stored in .Renviron for security

url <- "https://redcap.hmri.org.au/api/"
token <- Sys.getenv("redcap_token_workshops")

raw <- redcap_read(redcap_uri = url,
                   token = token,
                   export_survey_fields = TRUE)$data

labels <- redcap_read(redcap_uri = url,
                      token = token,
                      export_survey_fields = TRUE,
                      raw_or_label = "label",
                      raw_or_label_headers = "label")$data

# load(file = here("raw.RData"))

# labels disappear during any manipulation - apply labels at the end.

df <- raw |> 
  mutate(across(starts_with("rate"), ~ factor(.x, levels = 1:5, ordered = TRUE))) |> 
  rename(datetime = where(is.POSIXct)) |> 
  mutate(datetime = as_datetime(datetime),
         date = date(datetime))


ui <- fluidPage(
  
  titlePanel("Impact Workshops Feedback"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons(inputId = "choose_workshops", 
                   label = "Choose Workshops",
                   choices = list(
                     "All" = 1,
                     "Most Recent" = 2
                   )
      )
    ),
    mainPanel(
      
      width = 10,
      
      tabsetPanel(
        
        tabPanel("Figures", 
                 fluidRow(
                   column(width = 5,
                          plotOutput("plot_recommend")),
                   column(width = 5,
                          plotOutput("plot_relevant"))
                   ),
                 fluidRow(
                   column(width = 5, 
                          plotOutput("plot_level")),
                   column(width = 5, 
                          plotOutput("plot_apply"))
                   ),
                 fluidRow(
                   column(width = 5,
                          plotOutput("plot_preso"))
                   )
        ),
        
        tabPanel("Learnings", DT::DTOutput("learnings", width = "80%")),
        tabPanel("Dislikes", DT::DTOutput("dislikes", width = "80%")),
        tabPanel("Suggestions", DT::DTOutput("suggestions", width = "80%"))
      )
      
    )
  )
)


server <- function(input, output) {
  
  relevant_data <- reactive({
    
    if (input$choose_workshops == 2) {
      relevant_data = filter(df, date == max(date))
    } else {
      relevant_data = df
    }
    
  })
  
  #var_label(relevant_data) <- c(names(labels))
  
  output$plot_recommend <- renderPlot({
    plot_survey(relevant_data(), "rate_recommend")
  })
  
  output$plot_relevant <- renderPlot({
    plot_survey(relevant_data(), "rate_relevant")
  })
  
  output$plot_level <- renderPlot({
    plot_survey(relevant_data(), "rate_level")
  })
  
  output$plot_apply <- renderPlot({
    plot_survey(relevant_data(), "rate_apply")
  })
  
  output$plot_preso <- renderPlot({
    plot_survey(relevant_data(), "rate_presentation")
  })
  
  output$learnings <- DT::renderDT({
    l = relevant_data() |> 
      select(record_id, date, three_learnings) |> 
      filter(!is.na(three_learnings))
    
    expr = datatable(l, rownames = FALSE)
  })
  
  output$dislikes <- DT::renderDT({
    d = filter(relevant_data(), !is.na(list_dislikes)) |> 
      select(record_id, date, list_dislikes)
    
    expr = datatable(d, rownames = FALSE)
  })
  
  output$suggestions <- DT::renderDT({
    s <- filter(relevant_data(), !is.na(suggestions)) |> 
      select(record_id, date, suggestions)
    
    expr = datatable(s, rownames = FALSE)
    # HTML(paste(s$record_id, s$suggestions, "<br>"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
