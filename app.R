## app.R ##
library(shinydashboard)
library(httr)
# httr::set_config(config(ssl_verifypeer = FALSE))
httr::set_config(httr::config(ssl_verifypeer = 0L))
library(jsonlite)
library(plotly)
library(dplyr)
library(ggplot2)

library("cartography")
library("sf")
library(stringr)
library(ggplot2)

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
        icon = icon("fas fa-vihara")
      ),
      menuItem(
        "Karnali",
        tabName = "karnali",
        icon = icon("fas fa-water")
      ),
      menuItem("Sudurpaschim", tabName = "sudurpaschim", icon = icon("table")),
      
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        # infoBoxes with fill=FALSE
        
        tags$div(
          class = "header d-flex",
          tags$h2("Lastest 24 hours "),
          tags$h4("Covid-19 Cases in Nepal"),
          tags$h4(textOutput("latestDate")),
          
        ),
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
        h1("Bar Graph Representation", align = "center"),
        br(),
        
        # Graph here
        fluidRow(box(
          plotlyOutput("mainBar", height = 1000),  width = 12
        ),),
        
        br(),
        h1("Pie-Chart Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("currentpie"), width = 6),
                 box(plotlyOutput("overallpie"), width = 6),),
        
        fluidRow(box(
          dateRangeInput(
            "mainLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "40%",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("mainline", height = 700),
          width = 12
        )),
      ),
      
      # Province tab content
      tabItem(
        tabName = "province1",
        h1("Province No. 1 Covid-19 Information"),
        h4(
          "Overall Province No. 1 Covid-19 cases Data Analysis and Visualization"
        ),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(
          plotlyOutput("province1bar", height = 700), width = 12
        )),
        
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        
        fluidRow(box(
          dateRangeInput(
            "province1LineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "40%",
          ),
          
          br(),
          br(),
          br(),
          
          plotlyOutput("province1line", height = 700),
          width = 12
        )),
        
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("province1pie"), width = 6), box(plotlyOutput("province1agebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("province1FirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("province1SecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "province2",
        h1("Province No. 2 Covid-19 Information"),
        h4(
          "Overall Province No. 2 Covid-19 cases Data Analysis and Visualization"
        ),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("province2bar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "province2LineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "40%",
            format = "yyyy-mm-dd",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("province2line", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("province2pie"), width = 6), box(plotlyOutput("province2agebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("province2FirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("province2SecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "bagmati",
        h1("Bagmati Covid-19 Information"),
        h4("Overall Bagmati Covid-19 cases Data Analysis and Visualization"),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("bagmatibar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "bagmatiLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "30%",
            format = "yyyy-mm-dd",
          ),
          
          br(),
          br(),
          br(),
          plotlyOutput("bagmatiline", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("bagmatipie"), width = 6), box(plotlyOutput("bagmatiagebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("bagmatiFirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("bagmatiSecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "gandaki",
        h1("Gandaki Covid-19 Information"),
        h4("Overall Gandaki Covid-19 cases Data Analysis and Visualization"),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("gandakibar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "gandakiLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "30%",
            format = "yyyy-mm-dd",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("gandakiline", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("gandakipie"), width = 6), box(plotlyOutput("gandakiagebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("gandakiFirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("gandakiSecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "lumbini",
        h1("Lumbini Covid-19 Information"),
        h4("Overall Lumbini Covid-19 cases Data Analysis and Visualization"),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("lumbinibar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "lumbiniLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "30%",
            format = "yyyy-mm-dd",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("lumbiniline", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("lumbinipie"), width = 6), box(plotlyOutput("lumbiniagebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("lumbiniFirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("lumbiniSecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "karnali",
        h1("Karnali Covid-19 Information"),
        h4("Overall Karnali Covid-19 cases Data Analysis and Visualization"),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("karnalibar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "karnaliLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "30%",
            format = "yyyy-mm-dd",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("karnaliline", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("karnalipie"), width = 6), box(plotlyOutput("karnaliagebar"), width = 6)),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("karnaliFirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("karnaliSecondVarient"), width = 6)),
      ),
      tabItem(
        tabName = "sudurpaschim",
        h1("Sudurpaschim Covid-19 Information"),
        h4(
          "Overall Sudurpaschim Covid-19 cases Data Analysis and Visualization"
        ),
        br(),
        h2("Bar Graph Representation", align = "center"),
        br(),
        fluidRow(box(plotlyOutput("sudurpaschimbar"), width = 12)),
        br(),
        h2("Line Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(
          dateRangeInput(
            "sudurpaschimLineDate",
            h3("Date range"),
            min    = "2020-01-13",
            max    = "2021-06-25",
            start    = "2020-01-13",
            end    = "2021-06-25",
            width = "30%",
            format = "yyyy-mm-dd",
          ),
          br(),
          br(),
          br(),
          plotlyOutput("sudurpaschimline", height = 700),
          width = 12
        )),
        br(),
        h2("Pie-Chart and Horizontal Bar Graph Representation", align = "center"),
        br(),
        br(),
        fluidRow(box(plotlyOutput("sudurpaschimpie"), width = 6), box(
          plotlyOutput("sudurpaschimagebar"), width = 6
        )),
        br(),
        br(),
        h2("First Varient Vs Second Varient", align = "center"),
        br(),
        fluidRow(box(h4('Date From: 2020-01-13 to 2021-03-07'), br(),  plotlyOutput("sudurpaschimFirstVarient"), width = 6), box(h4('Date From: 2021-03-08 to 2021-06-25'), br(), plotlyOutput("sudurpaschimSecondVarient"), width = 6)),
      ),
      
      tabItem(
        tabName = "map",
        h1("Nepal Covid-19 Infected Information"),
        h4("Overall Nepal Covid-19 cases Data Analysis and Visualization"),
        br(),
        
        fluidRow(plotOutput("provinceMap", height = 1000)),
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
    districtData <-
      read.csv("C:/Users/Anmol/Desktop/R-Covid19/districtData.csv")
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
      read_json(path = "C:/Users/Anmol/Desktop/R-Covid19/covidnepal2.json", simplifyVector = TRUE)
    return(plainResult)
  }
  ageAndSexData <- getAgeAndSexData()
  
  for (i in ageAndSexData[2]) {
    ageAndSexData[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
  }
  ageAndSexData[is.na(ageAndSexData)] <- "Undefined"
  
  allData <- ageAndSexData
  
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
  
  getAggregateValueByPeriod <-
    function (dfProvisionValue,
              startDate = "2020-01-13",
              endDate = "2021-06-25") {
      totalGenderByPeriod <-
        subset(dfProvisionValue,
               select = c(Period, Value))
      if (startDate < endDate) {
        totalGenderByPeriod <-
          filter(totalGenderByPeriod,
                 Period >= startDate &
                   Period <= endDate)
      }
      totalGenderByPeriod <-
        totalGenderByPeriod[order(totalGenderByPeriod$Period), ]
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
  
  getVarientData <- function (df) {
    df <- subset(df, select = c(Period, District, Value))
    
    firstVarient <-  df %>% filter(as.Date(df$Period) < as.Date("2021-03-08"))
    secondVarient <- df %>% filter(as.Date(df$Period) >= as.Date("2021-03-08"))
    
    firstVarient <- aggregate(list(Value = as.numeric(firstVarient$Value)), by = (list(District = firstVarient$District)), sum)
    secondVarient <- aggregate(list(Value = as.numeric(secondVarient$Value)), by = (list(District = secondVarient$District)), sum)
    
    return(list(firstVarient, secondVarient))
  }
  
  ####################################################################################
  
  # Renderinf Section Current Information
  output$latestDate <- renderText({
    paste0('Date:    ', overAllData$nepal$date)
  })
  output$newCases <- renderInfoBox({
    infoBox(
      h4("New Cases"),
      paste0(overAllData$nepal$today_newcase),
      icon = icon("far fa-virus"),
      color = "blue",
      fill = TRUE
    )
  })
  output$recovered <- renderInfoBox({
    infoBox(
      h4("Recovered"),
      paste0(overAllData$nepal$today_recovered),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",
      fill = TRUE
    )
  })
  output$death <- renderInfoBox({
    infoBox(
      h4("Death"),
      paste(overAllData$nepal$today_death),
      icon = icon("far fa-sad-tear"),
      color = "red",
      fill = TRUE
    )
  })
  output$todayPCR <- renderInfoBox({
    infoBox(
      h4("Today's PCR"),
      paste(as.numeric(overAllData$nepal$today_pcr)),
      icon = icon("fas fa-user-check"),
      color = "aqua",
      fill = TRUE
    )
  })
  # Total Information
  output$totalCases <- renderInfoBox({
    infoBox(
      h4("Total Cases"),
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
      h4("Total Recovered"),
      paste(overAllData$nepal$extra1),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",
      fill = TRUE
    )
  })
  output$totalDeath <- renderInfoBox({
    infoBox(
      color = "red",
      h4("Total Death"),
      paste(overAllData$nepal$deaths),
      icon = icon("far fa-sad-cry"),
      fill = TRUE
    )
  })
  output$totalInfected <- renderInfoBox({
    infoBox(
      h4("Total Infected"),
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
      title = "All Districts Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    
    fig <- fig %>% config(displaylogo = FALSE)
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
        marker = list(colors = list("orange", "green", "red")),
        type = 'pie'
      )
    fig <- fig %>% layout(title = 'Current 24 hours Pie Chart')
    fig <- fig %>% config(displaylogo = FALSE)
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
        marker = list(colors = list("orange", "green", "red")),
        type = 'pie'
      )
    fig <-
      fig %>% layout(title = 'Total Cases Pie Chart Representation')
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  
  output$mainline <- renderPlotly({
    inputdate = input$mainLineDate
    data <- groupProvinceBySex(allData)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    
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
        title = "LineGraph: Male and Female Infection",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
        title = "Covid-19 Cases in Province No. 1",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$province1line <- renderPlotly({
    inputdate = input$province1LineDate
    
    data <- groupProvinceBySex(dfProvince1)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    
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
        title = "Covid-19 Male and Female Infection Cases in Province 1",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
        title = "Age Group wise Infected Cases in Province 1",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <-
      fig %>% layout(title = 'Sex-wise infected cases in Province No. 1')
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  
  output$province1FirstVarient <- renderPlotly({
    data = getVarientData(dfProvince1)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Province No. 1",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$province1SecondVarient <- renderPlotly({
    data = getVarientData(dfProvince1)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Province No. 1",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$province2line <- renderPlotly({
    inputdate = input$province2LineDate
    data <- groupProvinceBySex(dfProvince2)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Province 2",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$province2FirstVarient <- renderPlotly({
    data = getVarientData(dfProvince2)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Province No. 2",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$province2SecondVarient <- renderPlotly({
    data = getVarientData(dfProvince2)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Province No. 2",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
      title = "Bagmati Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$bagmatiline <- renderPlotly({
    inputdate = input$bagmatiLineDate
    data <- groupProvinceBySex(dfBagmati)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Bagmati",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$bagmatiFirstVarient <- renderPlotly({
    data = getVarientData(dfBagmati)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Bagmati",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$bagmatiSecondVarient <- renderPlotly({
    data = getVarientData(dfBagmati)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Bagmati",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
      title = "Gandaki Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$gandakiline <- renderPlotly({
    inputdate = input$gandakiLineDate
    data <- groupProvinceBySex(dfGandaki)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Gandaki",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  
  output$gandakiFirstVarient <- renderPlotly({
    data = getVarientData(dfGandaki)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Gandaki",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$gandakiSecondVarient <- renderPlotly({
    data = getVarientData(dfGandaki)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Gandaki",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
      title = "Lumbini Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$lumbiniline <- renderPlotly({
    inputdate = input$lumbiniLineDate
    data <- groupProvinceBySex(dfLumbini)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Lumbini",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$lumbiniFirstVarient <- renderPlotly({
    data = getVarientData(dfLumbini)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Lumbini",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$lumbiniSecondVarient <- renderPlotly({
    data = getVarientData(dfLumbini)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Lumbini",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
      title = "Karnali Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$karnaliline <- renderPlotly({
    inputdate = input$karnaliLineDate
    data <- groupProvinceBySex(dfKarnali)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Karnali",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$karnaliFirstVarient <- renderPlotly({
    data = getVarientData(dfKarnali)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Karnali",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$karnaliSecondVarient <- renderPlotly({
    data = getVarientData(dfKarnali)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Karnali",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
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
      title = "Sudurpaschim Covid-19 Infected Cases",
      xaxis = list(title = "Districts"),
      yaxis = list(title = "Covid-19 Infected Cases")
    )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$sudurpaschimline <- renderPlotly({
    inputdate = input$sudurpaschimLineDate
    data <- groupProvinceBySex(dfSudurpaschim)
    male <-
      getAggregateValueByPeriod(data$Male,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
    female <-
      getAggregateValueByPeriod(data$Female,
                                as.character(inputdate[1]),
                                as.character(inputdate[2]))
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
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
    fig
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
        title = "Age Group Infected People in Sudurpaschim",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displaylogo = FALSE)
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
    fig <- fig %>% config(displaylogo = FALSE)
    fig
  })
  
  output$sudurpaschimFirstVarient <- renderPlotly({
    data = getVarientData(dfSudurpaschim)
    firstVarient <- as.data.frame(data[1])
    fig <- plot_ly(
      x =  firstVarient$District,
      y = as.numeric(firstVarient$Value),
      type = "bar",
      color = firstVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 First Varient Cases in Sudurpaschim",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  output$sudurpaschimSecondVarient <- renderPlotly({
    data = getVarientData(dfSudurpaschim)
    secondVarient <- as.data.frame(data[2])
    fig <- plot_ly(
      x =  secondVarient$District,
      y = as.numeric(secondVarient$Value),
      type = "bar",
      color = secondVarient$District,
    )
    fig <-
      fig %>% layout(
        title = "Covid-19 Second Varient Cases in Sudurpaschim",
        xaxis = list(title = "Districts"),
        yaxis = list(title = "Covid-19 Infected Cases")
      )
    fig <- fig %>% config(displayModeBar = FALSE)
    fig
  })
  
  
  
  # Map
  provinceMapData <-
    st_read("hermes_NPL_new_wgs/hermes_NPL_new_wgs_1.shp")
  districtDataForProvince <- getDistrictData()
  districtDataForProvince <-
    subset(districtDataForProvince,
           select = c(Province, Value))
  districtDataForProvince <-
    aggregate(list(Value = as.numeric(districtDataForProvince$Value)), by = (list(ProvinceName = districtDataForProvince$Province)), sum)
  districtDataForProvince$PROVINCE <- c(3, 4, 6, 5, 1, 2, 7)
  mapDataForProvince <-
    merge(provinceMapData, districtDataForProvince, by = "PROVINCE")
  provinceMap <-
    cbind(mapDataForProvince, st_coordinates(st_centroid(mapDataForProvince$geometry)))
  mapDataForProvince <- as.data.frame(mapDataForProvince)
  provinceRangeValue <- seq(from = 10000, to = 295258, by = 10000)
  
  output$provinceMap <- renderPlot({
    map <- ggplot(data = provinceMap, colour = Value) +
      geom_sf(aes(fill = Value), color = "grey", size = 0.2) +
      geom_text(data = provinceMap,
                aes(
                  x = X,
                  y = Y,
                  label = (paste(Value)),
                  vjust = +1,
                )) +
      geom_text(
        data = provinceMap,
        aes(
          x = X,
          y = Y,
          label = (paste(PR_NAME)),
          vjust = -1,
        ),
        size = 4,
        fontface = "bold"
      ) +
      scale_fill_gradient(
        high = "red",
        low = "#ffffb3",
        breaks = provinceRangeValue,
        name = "Infected Cases Range"
      ) +
      guides(fill = guide_colorbar(barheight = 40)) +
      ggtitle("Covid-19 Province-wise Infected Cases") + xlab("Longitude") + ylab("Latitude")
    map
  })
}


shinyApp(ui, server)