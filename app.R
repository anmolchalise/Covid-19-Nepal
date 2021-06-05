## app.R ##
library(shinydashboard)
library(httr)
httr::set_config(config(ssl_verifypeer = FALSE))
library(jsonlite)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = span(
    "Covid-19", img(src = "nepal.gif", width = 20)
  )),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem("Province 1", tabName = "province1", icon = icon("th")),
      menuItem("Province 2", tabName = "province2", icon = icon("calendar")),
      menuItem(
        "Bagmati",
        tabName = "bagmati",
        icon = icon("bar-chart-o")
      ),
      menuItem(
        "Gandaki",
        tabName = "gandaki",
        icon = icon("bar-chart-o")
      ),
      menuItem(
        "Lumbini",
        tabName = "lumbini",
        icon = icon("bar-chart-o")
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
          # box(
          #   title = "Title 6", width = 2, background = "maroon",
          #   "A box with a solid maroon background"
          # ),
          infoBoxOutput("totalInfected", width = 3),
          infoBoxOutput("totalRecovered", width = 3),
          infoBoxOutput("totalDeath", width = 3),
          infoBoxOutput("totalCases", width = 3),
        ),
        br(),
        h1("Graphical Representation", align = "center"),
        br(),
        
        # Graph here
        fluidRow(box(plotOutput("plot1", height = 1000),  width = 12), )
      ),
      
      # Province tab content
      tabItem(
        tabName = "province1",
        h1("Province 1 Data representation"),
        p("The overall Covid-19 data of province 1 is represented here"),
        br(),
        h2("Graphical Representation", align = "center"),
        br(),
        fluidRow(box(plotOutput("province1bar"), width = 12)),
        fluidRow(box(plotOutput("province1line"), width = 12)),
        fluidRow(box(plotOutput("province1pie"), width = 12)),
        
      ),
      tabItem(tabName = "province2",
              h1("Province 2 tab content"),
              fluidRow(box(
                plotOutput("province2bar"), width = 12
              )),),
      tabItem(tabName = "bagmati",
              h2("Bagmati tab content"),
              fluidRow(box(
                plotOutput("bagmatibar"), width = 12
              )),),
      tabItem(tabName = "gandaki",
              h2("Gandaki tab content")),
      tabItem(tabName = "lumbini",
              h2("Lumbini tab content")),
      tabItem(tabName = "karnali",
              h2("Karnali tab content")),
      tabItem(tabName = "sudurpaschim",
              h2("Sudurpaschim tab content"))
    )
  )
)

server <- function(input, output) {
  districtUrl = "https://portal.edcd.gov.np/rest/api/fetchCasesByDistrict?filter=casesBetween&"
  overViewUrl  <-
    "https://covid19.mohp.gov.np/covid/api/confirmedcases"
  ageAndSexUrl = paste0(
    "https://portal.edcd.gov.np/rest/api/fetch?filter=casesBetween&type=aggregate&sDate=2020-01-01&eDate=",
    Sys.Date(),
    "&disease=COVID-19"
  )
  
  confirmedCases <- function() {
    result <- httr::GET(overViewUrl)
    plainResult <- (content(result))
    return(plainResult)
  }
  overAllData <- confirmedCases()
  
  getDistrictData <- function() {
    path <-
      paste0("sDate=2020-01-01&eDate=",
             Sys.Date(),
             "&disease=COVID-19")
    dUrl <- paste0(districtUrl, path)
    res <- httr::GET(dUrl)
    return (content(res))
  }
  
  getAgeAndSexData <- function() {
    result <- httr::GET(ageAndSexUrl)
    plainResult <- (content(result))
    return(plainResult)
  }
  
  ageAndSexData <- getAgeAndSexData()
  ageAndSexJson <- fromJSON(toJSON(ageAndSexData))
  for (i in ageAndSexJson[2]) {
    ageAndSexJson[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
  }
  dfAgeSex <-
    data.frame(
      Province = matrix(unlist(ageAndSexJson[1])),
      District = matrix(unlist(ageAndSexJson[2])),
      Sex = matrix(unlist(ageAndSexJson[3])),
      # Filter null age
      # Age = matrix(unlist(ageAndSexJson[4])),
      Value = matrix(unlist(ageAndSexJson[5]))
    )
  ageSexProvince <- split(dfAgeSex, dfAgeSex$Province)
  # bgSex <- split(ageSexProvince$"Province 1", ageSexProvince$"Province 1"$Sex)

  
  districtData <- getDistrictData()
  # table = toJSON(districtData, pretty = TRUE)
  
  districtJson <- fromJSON(toJSON(districtData))
  for (i in districtJson[2]) {
    districtJson[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
  }
  
  df <-
    data.frame(
      Province = matrix(unlist(districtJson[1])),
      District = matrix(unlist(districtJson[2])),
      Value = matrix(unlist(districtJson[3]))
    )
  
  individualProvince <- split(df, df$Province)
  # write.csv(df, "districtData.csv", row.names = FALSE)
  
  # Current Information
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
  output$plot1 <- renderPlot({
    par(mar = c(10, 4, 4, 4))
    barplot(
      ylim = range(pretty(c(
        0, as.numeric(df$Value)
      ))),
      # gap.barplot(x,gap=c(2,4)),
      ylab = "Infected Values",
      # xlab="Number of Districts",
      height = as.numeric(df$Value),
      names = df$District,
      main = "Overall District Infected",
      col = c(1:77),
      las = 3,
    )
  })
  
  # Province 1
  output$province1bar <- renderPlot({
    par(mar = c(10, 4, 4, 4))
    barplot(
      ylab = "Infected Values",
      height = as.numeric(individualProvince$"Province 1"$Value),
      names = individualProvince$"Province 1"$District,
      main = "BarGraph: Infected People in Province 1",
      col = c(1:77),
      las = 3,
    )
  })
  
  output$province1line <- renderPlot({
    plot(
      individualProvince$"Province 1"$Value,
      # x = individualProvince$"Province 1"$Discrict,
      type = "o",
      col = "red",
      xlab = "Month",
      ylab = "Values",
      main = "Province 1 Line Graph"
    )
  })
  
  output$province1pie <- renderPlot({
    pie(
      as.numeric(individualProvince$"Province 1"$Value),
      labels = individualProvince$"Province 1"$District,
      main = "Pie Chart of Total Reserve Use",
      col = rainbow(length(
        individualProvince$"Province 1"$Value
      )),
    )
  })
  
  # Province 2
  output$province2bar <- renderPlot({
    barplot(
      ylab = "Infected Values",
      # xlab = "Districts of Province 2",
      height = as.numeric(individualProvince$"Province 2"$Value),
      names = individualProvince$"Province 2"$District,
      main = "Province 2 District Infected",
      col = c(1:77),
      las = 2,
    )
  })
  
  # Bagmati
  output$bagmatibar <- renderPlot({
    par(mar = c(9, 5, 4, 4))
    barplot(
      ylim = range(pretty(c(
        0, as.numeric(individualProvince$"Bagmati"$Value)
      ))),
      ylab = "Infected Values",
      xlab = "Districts of Bagmati",
      height = as.numeric(individualProvince$"Bagmati"$Value),
      names = individualProvince$"Bagmati"$District,
      main = "Bagmati District Infected",
      col = c(1:77),
      las = 3,
    )
  })
  
  
  
}


shinyApp(ui, server)