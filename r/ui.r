library(shiny)

shinyUI(fluidPage(
  titlePanel("Выборы. ПСЕВДОСЛУЧАЙНЫЕ ДАННЫЕ!!!"),
  
  sidebarLayout(
    sliderInput("alpha",
                "Вероятность ошибки (выход за интервалы):",
                value = .05,
                min = .01,
                max = .3,
                locale = 'ru',
                format = "#%")
  ,
  
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Главное", 
                                 textOutput("lastupdate"),
                                 tableOutput("turnout"),
                                 tableOutput("props"),
                                 textOutput("sec_turn"),
                                 plotOutput("turnoutplot",
                                            width = "100px",
                                            height = "250px", 
                                            inline = F),
                                 plotOutput("propplot",
                                            width = "400px",
                                            height = "250px", 
                                            inline = F)),
                       tabPanel("Районы")
                        )
            )
  )
  )
  )

