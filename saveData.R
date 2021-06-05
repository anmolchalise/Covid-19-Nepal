library(httr)
httr::set_config(config(ssl_verifypeer = FALSE))
library(jsonlite)


districtUrl = "https://portal.edcd.gov.np/rest/api/fetchCasesByDistrict?filter=casesBetween&"
url  <- "https://covid19.mohp.gov.np/covid/api/confirmedcases"

confirmedCases <- function() {
  result <- GET(url)
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
  res <- GET(dUrl)
  return (content(res))
}

districtData <- getDistrictData()
# table = toJSON(districtData, pretty = TRUE)

table <- fromJSON(toJSON(districtData))
df <-
  data.frame(
    Province = matrix(unlist(table[1])),
    District = matrix(unlist(table[2])),
    Value = matrix(unlist(table[3]))
  )
write.csv(df, "districtData.csv", row.names = FALSE)
