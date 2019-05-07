library(shiny)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(tidyr)

load("df_probs_no_draw.Rda")
load('end_period_noDraws.Rda')

ui <- fluidPage(
  titlePanel("Probability Computation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "team1",
                  label = "Winning team:",
                  choices = c("home", 'away')),
      
      selectInput(inputId = "period1",
                  label = "Period:",
                  choices = c("1", '2','3')),
      
      sliderInput(inputId = "Diff",
                  label = "Difference",
                  min = 1,
                  max = 20,
                  value = 5)
    ),
    
    mainPanel(
      tableOutput("values"),
      plotOutput("BarPlot"),
      plotOutput("ScatterPlot")  
      
    )
  )
)

server <- function(input, output) {
  
  sliderValues <- reactive({
    data.frame(
      Attribute= c("Team","Period","Difference","Probability"),
      Value=as.character(c(input$team1,input$period1, input$Diff,
                           round(df_prob1%>%filter(period==as.numeric(input$period1), difference==as.numeric(input$Diff), team==input$team1)%>%select(probability), 3))),
      stringsAsFactors = FALSE)
    
  })
  
  output$values <- renderTable({
    sliderValues()
  })
  output$BarPlot <- renderPlot({
    game_id2=end_period_noDraws%>%
      filter(Period==as.numeric(input$period1), Diff.Points==as.numeric(input$Diff))%>%
      select(game.id)
    first_period12=end_period_noDraws%>%
      filter(Period==as.numeric(input$period1), Diff.Points==as.numeric(input$Diff))%>%
      select(result)
    last_period12=semi_join(end_period_noDraws, game_id2, by='game.id')%>%
      filter(Period==4)%>%
      select(result)
    result_table11=as.matrix(table(first_period12$result,last_period12$result))
    frame_=filter(as.data.frame(result_table11), Var1==input$team1)%>%select(Var2, Freq)
    ggplot(frame_, aes(x=Var2, y=Freq))+geom_col()+geom_text(aes(label=Freq), vjust=0)+xlab('Winning Team')+ylab('Number of Games')+ggtitle('Outcomes of games for given parameters')
  })
  output$ScatterPlot <- renderPlot({
    df_prob1%>%filter(period==as.numeric(input$period1), team==input$team1)%>%
      ggplot(aes(difference, probability))+ geom_point()+geom_line()+geom_vline(xintercept = as.numeric(input$Diff), linetype="dashed", color = "red")+geom_hline(yintercept =as.numeric(df_prob1%>%filter(period==as.numeric(input$period1), difference==as.numeric(input$Diff), team==input$team1)%>%select(probability)), linetype="dashed", color = "red")+ggtitle('Probability distribution for the given Winning team and Period')
    
  })
}

shinyApp(ui, server)