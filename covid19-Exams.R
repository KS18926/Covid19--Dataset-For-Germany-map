# installing COVID19
#install.packages('COVID19') the # is used for commenting 

# importing libraries using the function 'library'
library(COVID19) # to import a package the function 'library' is use
library(ggplot2) # ggplot2 to used to visual a graph or for plotting
library(plotly)
library(magrittr)
# fetching covid19 data for germany
# after loading our dataset, we realize that the germany_df has 
#1138 observations or entries and 47 variables or columns
germany_df= covid19(country = 'Germany')

# lets explore the structure of the data
str(germany_df)


# Check for missing values in each column
# this code simply means: sum all the 'NA'in the respective columns in the germany_df
missing_values = colSums(is.na(germany_df))

# Print the count of missing values for each column. 
#this is to populate the sum of 'NA'values in each column or variable
print(missing_values)

# Filter out rows with missing values in 'date' or 'confirmed'
# this is to select or filter out missing values in the date and the confirmed columns
# Filter out rows with missing values in 'date' or 'confirmed'
cleaned_data <- germany_df[complete.cases(germany_df[c("date", "confirmed")]), ]

# Create an interactive line plot for confirmed cases over time
plot_confirmed_cases <- plot_ly(cleaned_data, x = ~date, y = ~confirmed, type = 'scatter', mode = 'lines') %>%
  layout(title = "COVID-19 Confirmed Cases Over Time in Germany",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Confirmed Cases"))

# Print the interactive plot
print(plot_confirmed_cases)




# Filter out rows with missing values in 'date' or 'deaths'
cleaned_data2 = germany_df[complete.cases(germany_df[c("date", "deaths")]), ]

# Create an interactive time series plot for death cases
plot_deaths = plot_ly(cleaned_data2, x = ~date, y = ~deaths, type = 'scatter', mode = 'lines') %>%
  layout(title = "COVID-19 Death Cases Over Time in Germany",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Death Cases"))

# Print the interactive plot for death cases
print(plot_deaths)


# Filter out rows with missing values in 'date' or 'contact_tracing'
cleaned_data3 = germany_df[complete.cases(germany_df[c("date", "contact_tracing")]), ]

# Create an interactive time series plot for contact tracing
plot_contact_tracing =plot_ly(cleaned_data, x = ~date, y = ~contact_tracing, type = 'scatter', mode = 'lines') %>%
  layout(title = "Contact Tracing Over Time in Germany",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Contact Tracing"))

# Print the interactive plot for contact tracing
print(plot_contact_tracing)

 cleaned_data4 = germany_df[complete.cases(germany_df[c("date", "people_vaccinated")]), ]
# Create an interactive time series plot for people fully vaccinated
plot_people_fully_vaccinated <- plot_ly(cleaned_data4, x = ~date, y = ~people_fully_vaccinated, type = 'scatter', mode = 'lines') %>%
  layout(title = "People Fully Vaccinated Over Time in Germany",
         xaxis = list(title = "Date"),
         yaxis = list(title = "People Fully Vaccinated"))

# Print the interactive plot for people fully vaccinated
print(plot_people_fully_vaccinated)

# Filter out rows with missing values in 'date' or 'recovered'
cleaned_data5 = germany_df[complete.cases(germany_df[c("date", "recovered")]), ]
# Create an interactive time series plot for recovered cases
plot_recovered = plot_ly(cleaned_data5, x = ~date, y = ~recovered, type = 'scatter', mode = 'lines') %>%
  layout(title = "Recovered Cases Over Time in Germany",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Recovered Cases"))

# Print the interactive plot for recovered cases
print(plot_recovered)

# Install required packages if not already installed
# install.packages("shiny")
# install.packages("plotly")

library(shiny)
library(plotly)
library(dplyr)

# Assuming you have already loaded germany_df and the necessary libraries

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Visualization in Germany"),
  mainPanel(
    plotlyOutput("covidMap"),
    plotlyOutput("confirmedTimeSeries"),
    plotlyOutput("recoveredTimeSeries"),
    plotlyOutput("testTimeSeries"),
    plotlyOutput("vaccinatedTimeSeries"),
    plotlyOutput("contactTracingTimeSeries"),
    plotlyOutput("elderlyProtectionTimeSeries")
  )
)

# Server
server <- function(input, output) {
  # Filter data for map
  map_data <- germany_df %>%
    filter(!is.na(confirmed)) %>%
    select(latitude, longitude, confirmed)
  
  # Output for plotly map
  output$covidMap <- renderPlotly({
    # Create an interactive map
    map <- plot_geo(map_data, locationmode = "country names") %>%
      add_markers(
        x = ~longitude,
        y = ~latitude,
        size = ~confirmed,
        sizemode = "diameter",
        sizeref = 0.1,
        color = I("blue"),
        text = ~paste("Confirmed Cases: ", confirmed)
      ) %>%
      layout(
        title = "COVID-19 Confirmed Cases Map in Germany",
        showlegend = FALSE
      )
    
    # Return the map
    map
  })
  
  # Filter data for time series plot
  time_series_data <- germany_df %>%
    filter(!is.na(date) & !is.na(confirmed)) %>%
    select(date, confirmed, recovered, tests, people_fully_vaccinated, contact_tracing, elderly_people_protection)
  
  # Output for time series plots
  output$confirmedTimeSeries <- renderPlotly({
    plot_confirmed <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~confirmed,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'red')
    ) %>%
      layout(
        title = "COVID-19 Confirmed Cases Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Confirmed Cases")
      )
    plot_confirmed
  })
  
  output$recoveredTimeSeries <- renderPlotly({
    plot_recovered <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~recovered,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'green')
    ) %>%
      layout(
        title = "Recovered Cases Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Recovered Cases")
      )
    plot_recovered
  })
  
  output$testTimeSeries <- renderPlotly({
    plot_test <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~tests,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'blue')
    ) %>%
      layout(
        title = "Test Cases Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Test Cases")
      )
    plot_test
  })
  
  output$vaccinatedTimeSeries <- renderPlotly({
    plot_vaccinated <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~people_fully_vaccinated,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'purple')
    ) %>%
      layout(
        title = "People Fully Vaccinated Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "People Fully Vaccinated")
      )
    plot_vaccinated
  })
  
  output$contactTracingTimeSeries <- renderPlotly({
    plot_contact_tracing <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~contact_tracing,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'orange')
    ) %>%
      layout(
        title = "Contact Tracing Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Contact Tracing")
      )
    plot_contact_tracing
  })
  
  output$elderlyProtectionTimeSeries <- renderPlotly({
    plot_elderly_protection <- plot_ly(
      time_series_data,
      x = ~date,
      y = ~elderly_people_protection,
      type = 'scatter',
      mode = 'lines',
      marker = list(color = 'brown')
    ) %>%
      layout(
        title = "Elderly People Protection Over Time in Germany",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Elderly People Protection")
      )
    plot_elderly_protection
  })
}

# Run the Shiny app
shinyApp(ui, server)

