

library(shiny)
library(REDCapR)
library(tidyverse)
library(labelled)
library(ggeasy)

theme_set(theme_bw() + theme(legend.position = "bottom"))

plot_survey <- function(data, question) {
  ggplot(data = data) +
    geom_bar(aes(.data[[question]]), fill = "#23285E") +
    #labs(title = var_label(data[, question]), x = "", y = "") +
    scale_x_discrete(drop = FALSE, na.translate = FALSE) +
    easy_labs()
}

# set up redcap api
url <- "https://redcap.hmri.org.au/api/"
token <- Sys.getenv("redcap_token_workshops") 
# api key is stored in .Renviron for security

# get data
raw <- redcap_read(redcap_uri = url, 
                   token = token, 
                   export_survey_fields = TRUE)$data

labels <- redcap_read(redcap_uri = url, 
                      token = token, 
                      export_survey_fields = TRUE,
                      raw_or_label = "label",
                      raw_or_label_headers = "label")$data

# labels disappear during any manipulation - apply labels at the end.

df <- raw |> 
  mutate(across(starts_with("rate"), ~ factor(.x, levels = 1:5, ordered = TRUE))) |> 
  rename(datetime = where(is.POSIXct))

rate_vector <- grep("rate", names(df), value = TRUE)


ui <- fluidPage(
  
  titlePanel("Impact Workshops Feedback"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "choose_workshops", 
                   label = "Choose Workshops",
                   choices = list(
                     "All" = 1,
                     "Most Recent" = 2
                   )
      )
    ),
    mainPanel(
      
      uiOutput("plots")
      
      # plotOutput("survey_plots")#,
      # textOutput("survey_feedback")
    )
  )
)


server <- function(input, output) {
  
  relevant_data <- reactive({
    
    if (input$choose_workshops == 2) {
      relevant_data = filter(df, date = max(as_date(datetime)))
    } else {
      relevant_data = df
    }
    
  })
  
  #var_label(relevant_data) <- c(names(labels))
  
  output$survey_plots <- renderPlot({
    
    for (i in rate_vector) {
      plot = plot_survey(relevant_data(), i)
      print(plot)
    }
    
    #plot_survey(relevant_data(), "rate_relevant")
    
  })
  
  # output$survey_feedback <- renderText({
  #   
  #   expr = na.omit(relevant_data$three_learnings);
  #   sep = '<br /> <br />'
  #   
  # })
}

#     cat(na.omit(relevant_data$list_dislikes), sep = '<br /> <br />')
# cat(na.omit(relevant_data$suggestions), sep = '<br /> <br />')

# Run the application 
shinyApp(ui = ui, server = server)
