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
districts_sql <- tbl(ep_db, 'districts')
heads_sql <- tbl(ep_db, 'heads')
update_period <- 30000
update_info <- function(up_per, ses) {
  invalidateLater(up_per, ses)
}

totalvoters <- 246301

data_sql <- data_sql %>%
  collect %>%
  left_join(tbl(ep_db, 'sms') %>%
               select(sms = id, time) %>%
               collect, 'sms') %>%
  filter(day(time) == 14 &
           hour > 7)

shinyServer(function(input, output, session) {
  
  lastupdate <- reactive({
    update_info(update_period, session)
    checks <- checks_sql %>% collect
    last_check <- checks$time[dim(checks)[1]]
    last_check <- format(dmy_hms(last_check),
                         format("%d.%m.%Y %H:%M:%S"))
    paste("Последний смс-отчёт принят:", last_check)
  })
  
  hours_turnout <- reactive({
    update_info(update_period, session)
    #     stations_sql %>%
    #       filter(reserve != 1) %>%
    #       summarize(voters = sum(voters)) # 246301
    
    data_sql %>% 
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position == 2) %>%
      collect %>%
      group_by(hour) %>%
      summarize(value = sum(value),
                prop = value / totalvoters)%>%
      ungroup %>% summarize(prop = sum(prop))
    
  })
  
  props_refuse <- reactive({
    t <- data_sql %>% 
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position > 2 & position < 9) %>%
      select(position, value) %>%
      group_by(position) %>%
      summarize(value = sum(value)) %>%
      mutate(voters = sum(value),
             prop = ifelse(voters == 0, 0, round(100 * value / voters, 2)),
             candid = position - 2)
    
    r <- governors_sql %>%
                select(-party) %>%
                collect
    r <- rbind(r, data.frame(candidate = "отказы", candid = 6))
    
    t %>% inner_join(r, 'candid') %>%
      select(Кандидат = candidate, Избирателей = value, Доля = prop)
  })
  
  hours_props <- reactive({
   data_sql %>% 
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position > 2 & position < 8) %>%
      select(position, value, hour) %>%
      collect %>%
      group_by(hour, position) %>%
      summarize(value = sum(value)) %>%
      group_by(hour) %>%
      mutate(voters = sum(value),
             prop = ifelse(voters == 0, 0, value / voters),
             candid = position - 2) %>%
      left_join(governors_sql %>%
                   select(-party) %>%
                   collect, 'candid') %>%
      select(-position, -candid)
  })

  
  stations_turnout_r <- reactive({
    data_sql %>% 
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1 &
               position == 2) %>%
      collect %>%
      group_by(station) %>% 
      summarize(stationturnout = sum(value)) %>%
      inner_join(stations_sql %>%
                   select(station, voters, district) %>%
                   collect, 'station') %>%
      group_by(district, station) %>%
      mutate(voters = sum(voters),
             prop = stationturnout / voters) %>%
      ungroup %>%
      mutate(propdelta = weighted.mean(prop, voters) - prop)
  })
  
  
  totalturnout <- reactive({
    
    stations_turnout <- stations_turnout_r()
    stations_turnout %>%
      summarize(count = sum(stationturnout),
                total = totalvoters,
                prop = round(count / total * 100, 2)) %>%
      select(Явка = count, Доля = prop)
  })
  
  stations_props_r <- reactive({
     data_sql %>%
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
      ungroup %>%
      inner_join(stations_sql %>%
                   collect %>%
                   select(district, station), 'station')
    
  })
  
  props <- reactive({
    
    candids <- stations_props_r() %>%
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
      select(candidate, prop = totalprop, vari, answers) %>%
      mutate(lmerr = abs(vari * qnorm(1 - input$alpha / 2)),
             cimax = lmerr + prop,
             cimin = prop - lmerr,
             cimax = ifelse(cimax > 1, 1, cimax),
             cimin = ifelse(cimin < 0, 0, cimin)) %>%
      select(Кандидат = candidate, Избирателей = answers, 
             prop) %>%
      mutate(Доля = round(prop * 100, 2)) %>% select(-prop)
    
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
    paste("Вероятность второго тура по текущим результатам", 
          round(r$p.value * 100, 1), "%", sep = "")
  })
  
  districts_turnout <- reactive({
    stations_turnout <- stations_turnout_r()
    stations_turnout %>%
      select(station, turnout = stationturnout,
             voters, district) %>%
      group_by(district) %>%
      summarize(prop = sum(turnout) / sum(voters)) %>%
      inner_join(districts_sql %>%
                   select(district, name) %>%
                   collect, 'district') %>%
      select(district = name, prop)
    
  })
  
  districts_props <- reactive({
    stations_props_r() %>%
      select(district, station, position, value) %>%
      group_by(district, position) %>%
      summarize(votes = sum(value)) %>%
      group_by(district) %>%
      mutate(answers = sum(votes),
             prop = ifelse(answers == 0, 0,votes / answers),
             candid = position -2) %>%
      ungroup %>%
      select(district, candid, prop) %>%
      inner_join(districts_sql %>% collect %>%
                   select(-districtvoters), 'district') %>%
      left_join(governors_sql %>% collect %>% # Why inner_join doesn't work?
                  select(-party), 'candid') %>%
      select(district = name, candidate, prop)
    
  })
  
  districts_all <- reactive({
    props <- districts_props()
    turnout <- districts_turnout()
    turnout$candidate <- 'явка'
    t <- rbind(props, turnout[,c("district", "candidate", "prop")])
    t
  })
  
  stations_all <- reactive({
    props <- stations_props_r()
    turnout <- stations_turnout_r()
    props %>%
      mutate(candid = position - 2) %>%
      left_join(governors_sql %>% collect %>%
                  select(-party), 'candid') %>%
      select(district, station, variable = candidate, value, prop) %>%
      rbind(turnout %>%
              mutate(variable = "явка") %>%
              select(district, station, variable, value = stationturnout,
                     prop)) %>%
      inner_join(districts_sql %>% collect %>%
                   select(-districtvoters), 'district') %>%
      select(district = name, station, variable, value, prop)
  })
  
  heads <- reactive({
    data_sql %>% collect %>%
      filter(position > 8 & position < 14) %>%
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1) %>%
      mutate(variable = position - 8) %>%
      ungroup %>%
      inner_join(stations_sql %>% collect %>%
                   select(district, station), 'station') %>%
      group_by(district, variable) %>%
      summarize(value = sum(value)) %>%
      group_by(district) %>%
      mutate(totalvalue = sum(value),
             prop = ifelse(totalvalue == 0, 0, value / totalvalue)) %>%
      left_join(heads_sql %>% collect %>%
                  select(-party), c('district', 'variable')) %>%
      inner_join(districts_sql %>% collect %>%
                   select(-districtvoters), 'district') %>%
      ungroup %>%
      select(district = name, candidate, count = value, prop)
  })
  
  heads_stations <- reactive({
    
    data_sql %>% collect %>%
      filter(position > 8 & position < 14) %>%
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1) %>%
      mutate(variable = position - 8) %>%
      ungroup %>%
      inner_join(stations_sql %>% collect %>%
                   select(district, station), 'station') %>%
      group_by(district, station, variable) %>%
      summarize(value = sum(value)) %>%
      group_by(district, station) %>%
      mutate(totalvalue = sum(value),
             prop = ifelse(totalvalue == 0, 0, value / totalvalue)) %>%
      left_join(heads_sql %>% collect %>%
                  select(-party), c('district', 'variable')) %>%
      inner_join(districts_sql %>% collect %>%
                   select(-districtvoters), 'district') %>%
      ungroup %>%
      select(district = name, station, candidate,  prop, value) 
      })

  
  heads_refuse <- reactive({
    data_sql %>% collect %>%
      filter(position > 8) %>%
      group_by(station, hour) %>%
      filter(min_rank(desc(sms)) == 1) %>%
      mutate(variable = position - 8) %>%
      group_by(station, variable) %>%
      summarize(value = sum(value)) %>%
      inner_join(stations_sql %>% collect %>%
                   select(district, station, voters), 'station') %>%
      left_join(heads_sql %>% collect %>%
                  select(-party), c('district', 'variable')) %>%
      group_by(district, station) %>%
      mutate(prop = value / sum(value)) %>%
      inner_join(districts_sql %>% collect %>%
                   select(-districtvoters), 'district') %>%
      ungroup %>%
      select(district = name, station, candidate, count = value, prop)
  })
  
  output$head_refuse_pust <- renderDataTable({
    t <- heads_refuse() %>%
      filter(district == "Пустошкинский") %>%
      select(-count, -district) %>%
      mutate(prop = round(prop * 100, 2)) %>%
      dcast(station ~ candidate, value.var='prop')
    colnames(t)[1] <- "УИК"
    colnames(t)[length(colnames(t))] <- "отказы"
    t
  })
  
  
  output$head_refuse_pust_count <- renderTable({
    t <- heads_refuse() %>%
      filter(district == "Пустошкинский") %>%
      select(-prop, -district) %>%
      dcast(station ~ candidate, value.var='count')
    colnames(t)[1] <- "УИК"
    colnames(t)[length(colnames(t))] <- "отказы"
    t
  })
  
  output$head_stations_novor <- renderDataTable({
    heads_stations() %>%
      filter(district == "Новоржевский") %>%
      mutate(prop = round(prop * 100, 2)) %>%
      select(УИК = station, candidate, prop) %>%
      dcast(УИК ~ candidate, value.var = "prop")
    })
  
  output$head_stations_pust <- renderDataTable({
    heads_stations() %>%
      filter(district == "Пустошкинский") %>%
      mutate(prop = round(prop * 100, 2)) %>%
      select(УИК = station, candidate, prop) %>%
      dcast(УИК ~ candidate, value.var = "prop")
  })
  
  
  
  output$hours_props_area <- renderPlot({
        
    tto <- totalturnout()
    tto <- tto$Явка
    hp <- hours_props()
    hp$tto <- tto
# #     
#     ggplot(hours_props(), aes(hour, prop, fill = candidate)) + 
#       geom_area(alpha = .8) + geom_area(show_guide = F, color = 'black') +
#       scale_y_continuous("Голосов опрошенных", limits = c(0, 1),
#                          labels = percent) +
#       scale_x_continuous("Час", limits = c(8,20), breaks=8:20) +
#       scale_fill_brewer("Кандидат", palette="Set1")
#     
    hp$prop <- hp$value / hp$tto
    
    ggplot(hp, aes(hour, prop, colour = candidate, group = candidate)) + 
      geom_path(size = 1) +
      scale_y_continuous("От общего числа избирателей",
                         labels = percent, limits = c(0, .1)) +
      scale_x_continuous("Час", limits = c(8,20), breaks=8:20)  + 
      scale_color_brewer("Кандидат", palette="Set1")

  })
  
  output$stations_datatable <- renderDataTable({
    library(reshape2)
    d <- stations_all() %>% select(-value)
    d$prop <- ifelse(d$prop > 0 & d$prop <= 1, 
                     round(d$prop * 100, 2), d$prop)
    t <- dcast(d, district + station ~ variable, value.ar = 'prop')
    colnames(t)[1:2] <- c("Район", "УИК")
    t
  })
  

output$stations_count_datatable <- renderDataTable({
  library(reshape2)
  d <- stations_all() %>% select(-prop)
  t <- dcast(d, district + station ~ variable, value.ar = 'value')
  colnames(t)[1:2] <- c("Район", "УИК")
  t
})



  output$districts_turnout <- renderTable({
    library(reshape2)
    t <- districts_all()
    t$prop <- t$prop * 100
    t <- dcast(t, district ~ candidate, value.var = 'prop')
    row.names(t) <- t$district
    t$district <- NULL
    t
    
  })
  
  output$propplot <- renderPlot({
    
    d <- props()
    colnames(d) <- c("cand", "count", "prop")
    d$prop <- d$prop / 100
    ggplot(d, aes(cand, prop)) +
      geom_bar(stat = 'identity', width = .5,
               color = 'black', fill = 'orange',
               alpha = .8) + 
      #       geom_errorbar(width = .25, alpha = .6) +
      scale_y_continuous("Голосов опрошенных", limits = c(0, 1),
                         labels = percent) + xlab('')
  })
  
  output$lastupdate <- renderText({
    lastupdate()
  })
  
  output$totxt <- renderText({
    stations_turnout <- stations_turnout_r()
    stations_turnout %>%
      summarize(count = sum(stationturnout),
                prop = count / sum(voters)) -> df
    paste(round(100*df$prop, 2), "%")
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
  
  output$novorzh <- renderTable({
    heads() %>%
      filter(district == "Новоржевский") %>%
      mutate(prop = round(prop * 100, 2)) %>%
      select(Кандидат = candidate,
             Доля = prop)
  })
  
  output$pustosh <- renderTable({
    heads() %>%
      filter(district == "Пустошкинский") %>%
      mutate(prop = round(prop * 100, 2)) %>%
      select(Кандидат = candidate,
             Доля = prop)
  })
  

output$pustosh_count <- renderTable({
  heads() %>%
    filter(district == "Пустошкинский") %>%
    select(Кандидат = candidate,
           Избирателей = count)
})


  output$heads <- renderPlot({
    ggplot(heads(), aes(candidate, prop)) + 
      geom_bar(stat='identity', width = .5,
               color = 'black', fill = 'orange',
               alpha = .8) + 
      facet_wrap(~district, scales = "free_x", ncol = 1) +
      scale_y_continuous("Голосов опрошенных", limits = c(0, 1),
                         labels = percent) + xlab('')
    
  })
  
  output$props_refuse <- renderTable({
    props_refuse()
  })
})

