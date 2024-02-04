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
cleaned_data= germany_df[complete.cases(germany_df[c("date", "confirmed")]), ]

# plotting daily cases over time
graph=ggplot(cleaned_data,aes(x=date, y=confirmed))+ 
  geom_line()+ labs(title = "COVID-19 Confirmed Cases Over Time in Germany",
                                x = "Date",
                                y = "Confirmed Cases")



print(graph)




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


# Install and load the shiny package
install.packages("shiny")
library(shiny)

# Assume cleaned_data is your dataset with columns "date," "deaths," "recovered," "people_vaccinated," and "confirmed"

# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Data Over Time"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Choose a number:", min = 1, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$plot <- renderPlot({
    # Plot selected columns based on the chosen number
    selected_column <- switch(input$slider,
                              "deaths" = cleaned_data$deaths,
                              "recovered" = cleaned_data$recovered,
                              "people_vaccinated" = cleaned_data$people_vaccinated,
                              "confirmed" = cleaned_data$confirmed)
    
    plot(cleaned_data$date, selected_column, type = "l", col = "blue", 
         main = paste("COVID-19", input$slider, "Over Time"),
         xlab = "Date", ylab = input$slider)
  })
}

# Run the app
shinyApp(ui, server)



