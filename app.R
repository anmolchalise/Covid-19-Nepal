## app.R ##
library(shinydashboard)
library(httr)
# httr::set_config(config(ssl_verifypeer = FALSE))
httr::set_config(httr::config(ssl_verifypeer = 0L))
library(jsonlite)
library(plotly)
library(dplyr)

ui <- dashboardPage(
  title = "Covid-19 Nepal",
  dashboardHeader(title = span(
    "Covid-19", img(src = "nepal.gif", width = 20)
  )),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("fad fa-home")
      ),
      menuItem("Province 1", tabName = "province1", icon = icon("th")),
      menuItem("Province 2", tabName = "province2", icon = icon("calendar")),
      menuItem(
        "Bagmati",
        tabName = "bagmati",
        icon = icon("fas fa-chart-pie")
      ),
      menuItem(
        "Gandaki",
        tabName = "gandaki",
        icon = icon("fas fa-chart-line")
      ),
      menuItem(
        "Lumbini",
        tabName = "lumbini",
        icon = icon("fas fa-chart-bar")
      ),
      menuItem(
        "Karnali",
        tabName = "karnali",
        icon = icon("bar-chart-o")
      ),
      menuItem("Sudurpaschim", tabName = "sudurpaschim", icon = icon("table"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        # infoBoxes with fill=FALSE
        
        tags$div(class = "header d-flex",
                 tags$h2("Last 24 hours Data"),
                 tags$h4(textOutput("latestDate")),),
        br(),
        fluidRow(
          infoBoxOutput("newCases", width = 3),
          infoBoxOutput("recovered", width = 3),
          infoBoxOutput("death", width = 3),
          infoBoxOutput("todayPCR", width = 3),
        ),
        br(),
        br(),
        br(),
        br(),
        h2("Total Cases"),
        h4("Overall Covid-19 cases of Nepal"),
        br(),
        fluidRow(
          infoBoxOutput("totalInfected", width = 3),
          infoBoxOutput("totalRecovered", width = 3),
          infoBoxOutput("totalDeath", width = 3),
          infoBoxOutput("totalCases", width = 3),
        ),
        br(),
        h1("Graphical Representation", align = "center"),
        br(),
        
        # Graph here
        fluidRow(box(
          plotlyOutput("mainBar", height = 1000),  width = 12
        ), ),
        fluidRow(box(plotlyOutput("currentpie"), width = 6), 
                 box(plotlyOutput("overallpie"), width = 6),
        ),
      ),
      
      # Province tab content
      tabItem(
        tabName = "province1",
        h1("Province 1 Data representation"),
        p("The overall Covid-19 data of province 1 is represented here"),
        br(),
        h2("Graphical Representation", align = "center"),
        br(),
        fluidRow(box(
          plotlyOutput("province1bar", height = 700), width = 12
        )),
        fluidRow(box(
          plotlyOutput("province1line", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("province1pie"), width = 6), box(plotlyOutput("province1agebar"), width = 6)),
        
      ),
      tabItem(
        tabName = "province2",
        h1("Province 2 tab content"),
        fluidRow(box(plotlyOutput("province2bar"), width = 12)),
        fluidRow(box(
          plotlyOutput("province2line", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("province2pie"), width = 6), box(plotlyOutput("province2agebar"), width = 6)),
      ),
      tabItem(
        tabName = "bagmati",
        h2("Bagmati tab content"),
        fluidRow(box(plotlyOutput("bagmatibar"), width = 12)),
        fluidRow(box(
          plotlyOutput("bagmatiline", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("bagmatipie"), width = 6), box(plotlyOutput("bagmatiagebar"), width = 6)),
      ),
      tabItem(
        tabName = "gandaki",
        h2("Gandaki tab content"),
        fluidRow(box(plotlyOutput("gandakibar"), width = 12)),
        fluidRow(box(
          plotlyOutput("gandakiline", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("gandakipie"), width = 6), box(plotlyOutput("gandakiagebar"), width = 6)),
      ),
      tabItem(
        tabName = "lumbini",
        h2("Lumbini tab content"),
        fluidRow(box(plotlyOutput("lumbinibar"), width = 12)),
        fluidRow(box(
          plotlyOutput("lumbiniline", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("lumbinipie"), width = 6), box(plotlyOutput("lumbiniagebar"), width = 6)),
      ),
      tabItem(
        tabName = "karnali",
        h2("Karnali tab content"),
        fluidRow(box(plotlyOutput("karnalibar"), width = 12)),
        fluidRow(box(
          plotlyOutput("karnaliline", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("karnalipie"), width = 6), box(plotlyOutput("karnaliagebar"), width = 6)),
      ),
      tabItem(
        tabName = "sudurpaschim",
        h2("Sudurpaschim tab content"),
        fluidRow(box(plotlyOutput("sudurpaschimbar"), width = 12)),
        fluidRow(box(
          plotlyOutput("sudurpaschimline", height = 700), width = 12
        )),
        fluidRow(box(plotlyOutput("sudurpaschimpie"), width = 6), box(
          plotlyOutput("sudurpaschimagebar"), width = 6
        )),
      )
    )
  )
)

server <- function(input, output) {
  # districtUrl = "https://portal.edcd.gov.np/rest/api/fetchCasesByDistrict?filter=casesBetween&"
  overViewUrl  <-
    "https://covid19.mohp.gov.np/covid/api/confirmedcases"
  # ageAndSexUrl = paste0(
  #   "https://portal.edcd.gov.np/rest/api/fetch?filter=casesBetween&type=aggregate&sDate=2020-01-01&eDate=",
  #   Sys.Date(),
  #   "&disease=COVID-19"
  # )
  
  confirmedCases <- function() {
    result <- GET(overViewUrl)
    plainResult <- (content(result))
    return(plainResult)
  }
  overAllData <- confirmedCases()
  
  getDistrictData <- function() {
    # path <-
    #   paste0("sDate=2020-01-01&eDate=",
    #          Sys.Date(),
    #          "&disease=COVID-19")
    # dUrl <- paste0(districtUrl, path)
    # res <- httr::GET(dUrl)
    districtData <-
      read.csv("C:/Users/Anmol/Desktop/Covid-19-Nepal/districtData.csv")
    return (districtData)
  }
  
  districtData <- getDistrictData()
  
  for (i in districtData[2]) {
    districtData[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
  }
  
  individualProvince <- split(districtData, districtData$Province)
  
  # All data
  getAgeAndSexData <- function() {
    plainResult <-
      read_json(path = "C:/Users/Anmol/Desktop/Covid-19-Nepal/covidnepal.json", simplifyVector = TRUE)
    return(plainResult)
  }
  ageAndSexData <- getAgeAndSexData()
  for (i in ageAndSexData[2]) {
    ageAndSexData[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
  }
  ageAndSexData[is.na(ageAndSexData)] <- "Undefined"
  
  ageSexProvince <- split(ageAndSexData, ageAndSexData$Province)
  
  dfProvince1 <-
    subset(ageSexProvince$"Province 1",
           select = c(Period, District, Sex, Age, Value))
  dfProvince2 <-
    subset(ageSexProvince$"Province 2",
           select = c(Period, District, Sex, Age, Value))
  dfBagmati <-
    subset(ageSexProvince$Bagmati,
           select = c(Period, District, Sex, Age, Value))
  dfGandaki <-
    subset(ageSexProvince$Gandaki,
           select = c(Period, District, Sex, Age, Value))
  dfLumbini <-
    subset(ageSexProvince$Lumbini,
           select = c(Period, District, Sex, Age, Value))
  dfKarnali <-
    subset(ageSexProvince$Karnali,
           select = c(Period, District, Sex, Age, Value))
  dfSudurpaschim <-
    subset(ageSexProvince$Sudurpaschim,
           select = c(Period, District, Sex, Age, Value))
  
  
  groupProvinceBySex <- function (dataFrameofProvince) {
    dataFrameBySex <-
      split(dataFrameofProvince, dataFrameofProvince$Sex)
    return (dataFrameBySex)
  }
  
  getTotalSumOfValue <- function(df) {
    return(sum(as.numeric(df$Value)))
  }
  
  getAggregateValueByPeriod <- function (dfProvisionValue) {
    totalGenderByPeriod <-
      subset(dfProvisionValue,
             select = c(Period, Value))
    totalGenderByPeriod <-
      totalGenderByPeriod[order(totalGenderByPeriod$Period),]
    totalGenderByPeriod <-
      aggregate(list(Value = as.numeric(totalGenderByPeriod$Value)), by = (list(Period = totalGenderByPeriod$Period)), sum)
    return (totalGenderByPeriod)
  }
  
  getAggregateValueByAge <- function (dfProvisionValue) {
    totalGroupByAge <-
      subset(dfProvisionValue,
             select = c(Age, Value))
    totalGroupByAge <-
      aggregate(list(Value = as.numeric(totalGroupByAge$Value)), by = (list(Age = totalGroupByAge$Age)), sum)
    return (totalGroupByAge)
  }
  
  
  ####################################################################################
  
  # Renderinf Section Current Information
  output$latestDate <- renderText({
    paste0('Date:    ', overAllData$nepal$date)
  })
  output$newCases <- renderInfoBox({
    infoBox(
      "New Cases",
      paste0(overAllData$nepal$today_newcase),
      icon = icon("far fa-virus"),
      color = "blue",
      fill = TRUE
    )
  })
  output$recovered <- renderInfoBox({
    infoBox(
      "Recovered",
      paste0(overAllData$nepal$today_recovered),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",
      fill = TRUE
    )
  })
  output$death <- renderInfoBox({
    infoBox(
      "Death",
      paste(overAllData$nepal$today_death),
      icon = icon("far fa-sad-tear"),
      color = "red",
      fill = TRUE
    )
  })
  output$todayPCR <- renderInfoBox({
    infoBox(
      "Today's PCR",
      paste(as.numeric(overAllData$nepal$today_pcr)),
      icon = icon("fas fa-user-check"),
      color = "aqua",
      fill = TRUE
    )
  })
  # Total Information
  output$totalCases <- renderInfoBox({
    infoBox(
      "Total Cases",
      paste(
        as.numeric(overAllData$nepal$extra1) + as.numeric(overAllData$nepal$extra2) + as.numeric(overAllData$nepal$deaths)
      ),
      icon = icon("fas fa-viruses"),
      color = "purple",
      fill = TRUE
    )
  })
  output$totalRecovered <- renderInfoBox({
    infoBox(
      "Total Recovered",
      paste(overAllData$nepal$extra1),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",
      fill = TRUE
    )
  })
  output$totalDeath <- renderInfoBox({
    infoBox(
      color = "red",
      "Total Death",
      paste(overAllData$nepal$deaths),
      icon = icon("far fa-sad-cry"),
      fill = TRUE
    )
  })
  output$totalInfected <- renderInfoBox({
    infoBox(
      "Total Infected",
      paste(overAllData$nepal$extra2),
      icon = icon("fal fa-head-side-cough"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Graph Info here
  output$mainBar <- renderPlotly({
    fig <- plot_ly(
      x = districtData$District,
      y = as.numeric(districtData$Value),
      type = "bar",
      # comment below
      # color = districtData$District,
      # name = districtData$District,
      
      # marker = list(color = c(1:as.numeric(districtData$Value)))
    )
    fig <- fig %>% layout(
      title = "All Districts Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  
  output$currentpie <- renderPlotly({
    currentPieData <- list("New Case", "Recovered", "Death")
    currentValue <-
      list(
        overAllData$nepal$today_newcase,
        overAllData$nepal$today_recovered,
        overAllData$nepal$today_death
      )
    fig <-
      plot_ly(
        labels = ~ currentPieData,
        values = ~ currentValue,
        marker = list(colors = list("orange", "green", "maroon")),
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Current 24 hours Pie Chart')
    fig
  })
  
  output$overallpie <- renderPlotly({
    currentPieData <- list("Total Infected", "Recovered", "Death")
    currentValue <-
      list(overAllData$nepal$extra2,
           overAllData$nepal$extra1,
           overAllData$nepal$deaths)
    fig <-
      plot_ly(
        labels = ~ currentPieData,
        values = ~ currentValue,
        marker = list(colors = list("orange", "green", "maroon")),
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Value in Pie Chart')
    fig
  })
  
  
  
  # Province 1
  output$province1bar <- renderPlotly({
    fig <- plot_ly(
      x =  individualProvince$"Province 1"$District,
      y = as.numeric(individualProvince$"Province 1"$Value),
      type = "bar",
      # comment below
      color = individualProvince$"Province 1"$District,
      
    )
    fig <-
      fig %>% layout(
        title = "BarGraph: Infected People in Province 1",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$province1line <- renderPlotly({
    data <- groupProvinceBySex(dfProvince1)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Province 1",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$province1agebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfProvince1)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Province 2",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  
  output$province1pie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfProvince1)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  # Province 2
  output$province2bar <- renderPlotly({
    fig <- plot_ly(
      x =  individualProvince$"Province 2"$District,
      y = as.numeric(individualProvince$"Province 2"$Value),
      color = individualProvince$"Province 2"$District,
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "BarGraph: Infected People in Province 2",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$province2line <- renderPlotly({
    data <- groupProvinceBySex(dfProvince2)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Province 2",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$province2agebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfProvince2)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Province 2",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  
  output$province2pie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfProvince2)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  # Bagmati
  output$bagmatibar <- renderPlotly({
    fig <- plot_ly(
      x = individualProvince$"Bagmati"$District,
      y = as.numeric(individualProvince$"Bagmati"$Value),
      color = individualProvince$"Bagmati"$District,
      type = "bar",
    )
    fig <- fig %>% layout(
      title = "Bagmati Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  output$bagmatiline <- renderPlotly({
    data <- groupProvinceBySex(dfBagmati)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Bagmati",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$bagmatiagebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfBagmati)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Bagmati",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$bagmatipie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfBagmati)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  
  # Gandaki
  output$gandakibar <- renderPlotly({
    fig <- plot_ly(
      x = individualProvince$"Gandaki"$District,
      y = as.numeric(individualProvince$"Gandaki"$Value),
      color = individualProvince$"Gandaki"$District,
      type = "bar",
    )
    fig <- fig %>% layout(
      title = "Gandaki Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  output$gandakiline <- renderPlotly({
    data <- groupProvinceBySex(dfGandaki)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Gandaki",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$gandakiagebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfGandaki)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Gandaki",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$gandakipie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfGandaki)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  
  # Lumbini
  output$lumbinibar <- renderPlotly({
    fig <- plot_ly(
      x = individualProvince$"Lumbini"$District,
      y = as.numeric(individualProvince$"Lumbini"$Value),
      color = individualProvince$"Lumbini"$District,
      type = "bar",
    )
    fig <- fig %>% layout(
      title = "Lumbini Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  output$lumbiniline <- renderPlotly({
    data <- groupProvinceBySex(dfLumbini)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Lumbini",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$lumbiniagebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfLumbini)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Lumbini",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$lumbinipie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfLumbini)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  
  # Karnali
  output$karnalibar <- renderPlotly({
    fig <- plot_ly(
      x = individualProvince$"Karnali"$District,
      y = as.numeric(individualProvince$"Karnali"$Value),
      color = individualProvince$"Karnali"$District,
      type = "bar",
    )
    fig <- fig %>% layout(
      title = "Karnali Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  output$karnaliline <- renderPlotly({
    data <- groupProvinceBySex(dfKarnali)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Karnali",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$karnaliagebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfKarnali)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Karnali",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$karnalipie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfKarnali)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
  
  
  # Sudurpaschim
  output$sudurpaschimbar <- renderPlotly({
    fig <- plot_ly(
      x = individualProvince$"Sudurpaschim"$District,
      y = as.numeric(individualProvince$"Sudurpaschim"$Value),
      color = individualProvince$"Sudurpaschim"$District,
      type = "bar",
    )
    fig <- fig %>% layout(
      title = "Sudurpaschim Infected Values",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Infected Values")
    )
    fig
  })
  
  output$sudurpaschimline <- renderPlotly({
    data <- groupProvinceBySex(dfSudurpaschim)
    male <- getAggregateValueByPeriod(data$Male)
    female <- getAggregateValueByPeriod(data$Female)
    
    fig <-
      plot_ly(
        x = male$Period,
        y = male$Value,
        name = 'Male',
        type = 'scatter',
        mode = 'lines'
      )
    fig <-
      fig %>% add_trace(
        x = female$Period,
        y = female$Value,
        name = 'Female',
        mode = 'lines'
      )
    fig <-
      fig %>% layout(
        title = "LineGraph: Male and Female Infection in Sudurpaschim",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Infected Values")
      )
  })
  
  output$sudurpaschimagebar <- renderPlotly({
    ageGroupValue <- getAggregateValueByAge(dfSudurpaschim)
    fig <- plot_ly(
      x =  ageGroupValue$Value,
      y = ageGroupValue$Age,
      color = c(ageGroupValue$Age),
      type = "bar",
    )
    fig <-
      fig %>% layout(
        title = "Histogram: Age Group Infected People in Sudurpaschim",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Infected Values")
      )
    fig
  })
  
  output$sudurpaschimpie <- renderPlotly({
    provincePieData <- list("Male", "Female")
    bothGender = groupProvinceBySex(dfSudurpaschim)
    value <-
      list(getTotalSumOfValue(bothGender$Male),
           getTotalSumOfValue(bothGender$Female))
    
    fig <-
      plot_ly(
        labels = ~ provincePieData,
        values = ~ value,
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Total Male and Female Pie Chart')
    fig
  })
}


shinyApp(ui, server)