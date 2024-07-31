library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("INVEST Data Availability Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cohortYear", "Enter Cohort Year:", value = 1984, min = 1890, max = 2024)
    ),
    mainPanel(
      tableOutput("dataAvailability"),
      tableOutput("genotypeData") # Include the genotype data output here
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$dataAvailability <- renderTable({
    # Define data sources and their availability
    dataSources <- c("Birth register", "HILMO inpatient", "HILMO outpatient", "Kela medication", "CENSUS", "FOLK", "GPA 9th grade (age 16)", "Parents info 2000-09", "Parents info Census (assumed age 25)")
    availableFrom <- c(1987, 1970, 1998, 1993, 1970, 1986, 1989, 2000, 1970)
    availableUntil <- c(2021, 2020, 2020, 2020, 1985, 2021, 2021, 2009, 1985)
    cohortYear <- input$cohortYear
    ageStart <- availableFrom - cohortYear
    ageEnd <- availableUntil - cohortYear
    # Calculate availability with specific conditions
    availability <- vector("character", length(dataSources))
    for (i in 1:length(dataSources)) {
      if (dataSources[i] == "GPA 9th grade (age 16)") {
        availability[i] <- ifelse(ageStart[i] <= 16 & ageEnd[i] >= 16, "Available", "Not Available")
      } else if (dataSources[i] == "Birth register") {
        availability[i] <- ifelse(cohortYear > 1986, "Available", "Not Available")
      } else if (dataSources[i] == "Parents info Census (assumed age 25)") {
        # For "Parents info Census," add 25 years to AgeStart and AgeEnd
        ageStart[i] <- ageStart[i] + 25
        ageEnd[i] <- ageEnd[i] + 25
        availability[i] <- ifelse(ageStart[i] >= 0 | ageEnd[i] >= 0, "Available", "Not Available")
        } else {
        # For all other data sources, data is available if AgeStart or AgeEnd >= 0
        availability[i] <- ifelse(ageStart[i] >= 0 | ageEnd[i] >= 0, "Available", "Not Available")
      }
    }
    dataAvailability <- data.frame(
      DataSource = dataSources,
      AvailableFrom = availableFrom,
      AvailableUntil = availableUntil,
      AgeStart = ageStart,
      AgeEnd = ageEnd,
      Availability = availability
    )
    dataAvailability
  })
  
  # Genotype data output
  output$genotypeData <- renderTable({
    # Manually entered genotype data
    genotype_data <- data.frame(
      YearOfBirth = c(1905:1998),
      NGenotyped = c(8,4, 6, 5, 16, 22, 18, 30, 28, 45, 31, 43, 60, 57, 65, 64, 40, 39, 120, 131, 147, 139, 160, 330, 385, 416, 346, 391, 456, 533, 563, 581, 609, 772, 771, 685, 982, 695, 770, 742, 953, 1032, 1173, 936, 966, 916, 912, 955, 841, 915, 909, 942, 858, 863, 882, 828, 833, 874, 803, 825, 787, 788, 753, 698, 617, 585, 563, 492, 375, 432, 430, 476, 459, 281, 274, 275, 314, 267, 173, 157, 159, 153, 154, 65, 74, 86, 70, 18, 20, 7, 17, 7, 10, 13)
    )
    
    # Filter genotype data for the selected cohort year
    selectedGenotypeData <- subset(genotype_data, YearOfBirth == input$cohortYear)
    return(selectedGenotypeData)
  })
}

# Run the app
shinyApp(ui = ui, server = server)