library(shiny)

shinyUI(fluidPage(
  titlePanel("Выборы в Псковской области"),
  
  
  sidebarLayout(

    sliderInput("alpha",
                "Доверительная вероятность:",
                value = .05,
                min = .01,
                max = .3,
                locale = 'ru',
                format = "#%"),
    
#   actionButton("update", "Обновить"),
#   
#   selectInput("hourplottype",
#               "Тип почасового графика:",
#               c("Доля от проголосовавших" = 'area',
#                 "Доля от зарегистрированных" = 'path'))
#   ,
  
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Главное", 
                                 textOutput("lastupdate"),
#                                  plotOutput("hours_props_area",
#                                             width="600px",
#                                             height="200px"),
#                                  textOutput("totxt"),
                                 br(),
                                 tableOutput("turnout"),
                                 tableOutput("props"),
#                                  textOutput("sec_turn"),
#                                  plotOutput("turnoutplot",
#                                             width = "100px",
#                                             height = "250px", 
#                                             inline = F),
                                 tableOutput("props_refuse"),

                                 plotOutput("propplot",
                                            width = "400px",
                                            height = "250px", 
                                            inline = F)),
                       tabPanel("Районы",
                                tableOutput("districts_turnout")),
                       tabPanel("Участки (доли)",
                                dataTableOutput('stations_datatable')),
                       tabPanel("Участки (абс.)",
                                dataTableOutput('stations_count_datatable')),

                       tabPanel("Главы районов",
                                plotOutput("heads",
                                           width = "400px",
                                           height = "400px"),
                                h2("Пустошкинский район"),
                                tableOutput("pustosh_count"),
                                tableOutput("pustosh"),
                                h2("Новоржевский район"),
                                tableOutput("novorzh")
                                ),
                       tabPanel("Главы по УИКам",
                                h2("Пустошкинский район"),
                                dataTableOutput("head_stations_pust"),
                                h2("Новоржевский район"),
                                dataTableOutput("head_stations_novor")
                                ),
tabPanel("Пустошка (отк.)",
        dataTableOutput("head_refuse_pust"),
        tableOutput("head_refuse_pust_count")
                        )
            )
  )
  )))

