source('read_config.r')
source('connect_db.r')

library(dplyr)
library(shiny)
library(ggplot2)
library(scales)
library(lubridate)


config <- read_config('config.yml')
ep_db <- connect_db(config)
data_sql <- tbl(ep_db, 'data')
stations_sql <- tbl(ep_db, 'stations')
governors_sql <- tbl(ep_db, 'governors')
checks_sql <- tbl(ep_db, 'checks1')

shinyServer(function(input, output) {
  
  lastupdate <- reactive({
    checks <- checks_sql %>% collect
    last_check <- checks$time[dim(checks)[1]]
    last_check <- format(dmy_hms(last_check),
                         format("%d.%m.%Y %H:%M:%S"))
    paste("Последний смс-отчёт принят:", last_check)
  })
  
  totalturnout <- reactive({
    
    stations_turnout <- data_sql %>% 
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position == 2) %>%
      collect %>%
      group_by(station) %>% 
      summarize(stationturnout = sum(value)) %>%
      inner_join(stations_sql %>%
                   select(station, voters) %>%
                   collect, 'station') %>%
      group_by(station) %>%
      summarize(voters = sum(voters),
                turnout = sum(stationturnout),
                prop = turnout / voters) %>%
      mutate(propdelta = weighted.mean(prop, voters) - prop)
    
    turnout <- weighted.mean(stations_turnout$prop, 
                             stations_turnout$voters)
    
    sterr <- sqrt((sum(abs(stations_turnout$propdelta)) * 
                     (600 - nrow(stations_turnout)))/ 
                    (nrow(stations_turnout) * 599))
    lmerr <- abs(sterr * qnorm(1 - input$alpha / 2))
    
    r <- data.frame(явка = c(sum(stations_turnout$turnout),
                             turnout,
                          ifelse(turnout - lmerr < 0, 0, turnout - lmerr),
                          turnout + lmerr))
    r[["явка"]][2:4] <- round(r[["явка"]][2:4] * 100, 2)
    row.names(r) <- c("Всего насчитано",
                      "Явка по выборке", 
                      "Нижний предел", "Верхний предел")
    as.data.frame(t(r))
  })
  
  props <- reactive({
    candids <- data_sql %>%
      collect %>%
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position > 2 & 
               position < 8) %>%
      group_by(station, position) %>%
      summarize(value = sum(value)) %>%
      group_by(station) %>%
      mutate(answers = sum(value),
             prop = ifelse(answers == 0, 0, value / answers), # Only zeros in sms
             station_varn = ifelse(answers == 0, 0,
                                   (prop * (1 - prop)) / answers)) %>%
      group_by(position) %>%
      mutate(propdelta = weighted.mean(prop, answers) - prop) %>%
      summarize(totalprop = weighted.mean(prop, answers),
                varc = (sum(abs(propdelta)) * (600 - 150)) / (150 * 599),
                station_varn_sum = sum(station_varn),
                vari = sqrt(varc + station_varn_sum),
                answers = sum(value)) %>%
      mutate(candid = position - 2) %>%
      inner_join(governors_sql %>% 
                   collect, "candid") %>%
      select(candidate, prop = totalprop, vari, answers)
    
    candids %>%
      mutate(lmerr = abs(vari * qnorm(1 - input$alpha / 2)),
             cimax = lmerr + prop,
             cimin = prop - lmerr,
             cimax = ifelse(cimax > 1, 1, cimax),
             cimin = ifelse(cimin < 0, 0, cimin)) %>%
      select(Кандидат = candidate, Посчитано = answers, 
             Доля = prop,
             Нижний = cimin, Верхний = cimax)

  })
  
  sec_turn <- reactive({
    sec_turn <- data_sql %>%
      collect %>%
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position > 2 & 
               position < 8) %>%
      group_by(position) %>%
      summarize(votes = sum(value)) %>%
      mutate(totalvotes = sum(votes)) %>%
      filter(votes == max(votes))
    r <- with(sec_turn, 
         prop.test(votes, 
                   totalvotes, 
                   alternative = 'gre',
                   conf.level = input$alpha))
    paste("Вероятность второго тура на текущий момент", 
          round(r$p.value * 100, 1), "%", sep = "")
  })
  
  output$turnoutplot <- renderPlot({
    r <- totalturnout()
    colnames(r) <- c('voters', 'prop', 'cimin', 'cimax')
    ggplot(r, aes(x = as.factor(1), y = prop,
                  ymax = cimax, ymin = cimin)) + 
      geom_bar(stat = 'identity', width = .5,
               color = 'black', fill = 'orange',
               alpha = .8) +
      geom_errorbar(width = .25, alpha = .6) +
      scale_y_continuous("Явка %", limits = c(0, 100)) +
      scale_x_discrete("", breaks = NULL)
      
    
  })
  
  output$propplot <- renderPlot({
    d <- props()
    colnames(d) <- c("cand", "count", "prop", "low", "high")
    ggplot(d, aes(cand, prop, ymax = high, ymin = low)) +
      geom_bar(stat = 'identity', width = .5,
               color = 'black', fill = 'orange',
               alpha = .8) + 
      geom_errorbar(width = .25, alpha = .6) +
      scale_y_continuous("Голосов опрошенных", limits = c(0, 1),
                         labels = percent) + xlab('')
  })
  
  
  
  output$turnout <- renderTable({
    totalturnout()
  })
  
  output$props <- renderTable({
    props()
  })
  
  output$sec_turn <- renderText({
    sec_turn()
    })
})