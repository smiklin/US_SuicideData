#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(usmap)

data <- read.csv("USrates1900clean.csv")
map_data <- read.csv("state_rates.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("US Suicide Rates",
                 tabPanel("Plot",
                          fluidPage(
                              # Sidebar with a slider input for number of bins 
                              sidebarLayout(
                                  sidebarPanel(
                                      checkboxInput("flips", 
                                                    "Multiple variables: Flip color and line style?", 
                                                    value = FALSE, 
                                                    width = NULL),
                                      
                                      sliderInput("years",
                                                  "Years:",
                                                  min = 1900,
                                                  max = 2018,
                                                  value = c(1900,2018),
                                                  sep = ""),
                                      
                                      checkboxGroupInput("sex",
                                                         "Sex:",
                                                         choices = list("Both" = "Both", 
                                                                        "Male" = "Male", 
                                                                        "Female" = "Female"),
                                                         selected = "Both"),
                                      
                                      checkboxGroupInput("race",
                                                         "Race:",
                                                         choices = list("All" = "All", 
                                                                        "White" = "White", 
                                                                        "Black or African American" = "Black or African American",
                                                                        "American Indian or Alaska Native" = "American Indian or Alaska Native",
                                                                        "Asian or Pacific Islander" = "Asian or Pacific Islander",
                                                                        "Not White" = "Not White"),
                                                         selected = "All"),
                                      
                                      checkboxGroupInput("age",
                                                         "Age Groups:",
                                                         choices = list("Overall rate" = "All.ages", 
                                                                        "Age adjusted rate" = "Age.adjusted.rate",
                                                                        "5-14 years" = "05-14 years", 
                                                                        "15-24 years" = "15-24 years",
                                                                        "25-34 years" = "25-34 years",
                                                                        "35-44 years" =  "35-44 years",
                                                                        "45-54 years" = "45-54 years",
                                                                        "55-64 years" = "55-64 years",
                                                                        "65-74 years" = "65-74 years",
                                                                        "75-84 years" = "75-84 years",
                                                                        "85+ years" = "85+ years"),
                                                         selected = "All.ages"),
                                      
                                      checkboxGroupInput("means",
                                                         "Means:",
                                                         choices = list("All" = "All", 
                                                                        "Firearms" = "Firearms",
                                                                        "Other" = "Other"),
                                                         selected = "All")
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      br(),
                                      plotOutput("ratePlot"),
                                      br(),
                                      textOutput("infoText")
                                      )
                              )
                          )
                 ),
    
                 tabPanel("Map",
                          fluidPage(
                              # Sidebar with a slider input for number of bins 
                              sidebarLayout(
                                  sidebarPanel(
                                      radioButtons("sex2",
                                                   "Sex:",
                                                   choices = list("Both" = "Both", 
                                                                  "Male" = "Male", 
                                                                  "Female" = "Female"),
                                                   selected = "Both"), 
                                      
                                      radioButtons("which_rate",
                                                   "Rate:",
                                                   choices = list("Crude Rate" = "Crude", 
                                                                  "Age Adjusted Rate" = "Adjusted"),
                                                   selected = "Adjusted"),
                                      
                                      helpText("Input up to six years that you want",
                                               "to compare side-by-side, ranging from",
                                               "1968-2018"),
                                      numericInput("year1", 
                                                   "Year 1", 
                                                   value = 1968),
                                      numericInput("year2", 
                                                   "Year 2", 
                                                   value = 1978),
                                      numericInput("year3", 
                                                   "Year 3", 
                                                   value = 1988),
                                      numericInput("year4", 
                                                   "Year 4", 
                                                   value = 1998),
                                      numericInput("year5", 
                                                   "Year 5", 
                                                   value = 2008),
                                      numericInput("year6", 
                                                   "Year 6", 
                                                   value = 2018)
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(br(),
                                            plotOutput("rateMap")
                                          )
                                      )
                                  )
                          ),
                 tabPanel("Data",
                          br(),
                          p("The data for 1900-1998 was collected from files available on the ",
                            tags$a(href="https://www.cdc.gov/nchs/nvss/mortality_historical_data.htm", "CDC website."),
                            "Note that rates are missing if there were fewer than 20 deaths in a specific population
                            in a given year, as these are deemed unreliable. Some data (e.g. Race, Means) has not been collected
                            throughout the time period, now was data consistently collected across the states, especially in the
                            early years. Further, death classification criteria have likely changed over time, and the data
                            was recorded using different ICD designations, approximately as follows:"),
                          tags$ul(
                              tags$li("1900‐1909: ICD-1 (155-163)"),
                              tags$li("1910‐1919: ICD-2 (155-163)"),
                              tags$li("1920‐1929: ICD-3 (165-174)"),
                              tags$li("1930‐1938: ICD-4 (163-171)"),
                              tags$li("1939‐1948: ICD-5 (163,164)"),
                              tags$li("1949‐1949: ICD-6 (E963,E970‐E979)"),
                              tags$li("1950‐1967: ICD-7 (E963,E970-E979)"),
                              tags$li("1968‐1978: ICD-8 (E800-E807,E825-E949)"),
                              tags$li("1968‐1978: ICD-8 (E800-E807,E825-E949)"),
                              tags$li("1979‐1998: ICD-9 (E950-E959)")
                          ),
                          
                          p("The 1999-2018 data has been generated through the ",
                            tags$a(href="https://wonder.cdc.gov/ucd-icd10.html", "CDC's Underlying Cause of Death database,"),
                            "using the ICD-10 113 Causes List."),
                          
                          tags$ul(
                              tags$li("Intentional self-harm (suicide)(*U03,X60-X84,Y87.0)"),
                              tags$li("Intentional self-harm (suicide) by discharge of firearms (X72-X74)"),
                              tags$li("Intentional self-harm (suicide) by other and unspecified means and their sequelae (*U03,X60-X71, X75-X84,Y87.0)")
                          ),
                          p("The CDC calculates the age adjusted rates using the 2000 US Standard population.")
                          ),
                 tabPanel("App Info",
                          br(),
                          
                          p("You can download the complete dataset, as well as CDC source documents on the project's ",
                            tags$a(href="https://github.com/smiklin/US_SuicideData", "GitHub page.")
                          ),
                          
                          p("The data has been assembled by ",
                            tags$a(href="https://sanjamiklin.com", "Sanja Miklin."),
                            "Please contact me at 'smiklin@uchicago.edu' if you notice any errors, or have suggestions for data I could include."),
                          p("If you use the aggregated dataset or the plots generated by the application, please acknowledge my work in any presentations or publications.")
                          
                 )
                 
)
    
    

# Define server logic required to draw the plots
server <- function(input, output) {
    
    vars_set <-  reactive({
         vars <- tibble(var = c("Age", "Sex", "Race", "Means"),
                   len = c(length(input$age), length(input$sex), length(input$race), length(input$means)))
    })
    
    
    ## PLOT
    
    output$ratePlot <- renderPlot({

       df <- data %>%
           filter(Year %in% input$years[1]:input$years[2],
                  Sex %in% input$sex, 
                  Race %in% input$race, 
                  Means %in% input$means, 
                  Age %in% input$age)
       
       divs <- vars_set()
       
       if (sum(divs$len) == 4){
           df %>%
               ggplot(aes(x=Year, y=Rate, color = Sex)) +
               geom_line(color = "#008080") +
               scale_x_continuous(breaks = seq(1900, 2020, by=10)) +
               scale_y_continuous(expand = c(0, 0),limits = c(0, NA)) +
               theme_minimal() +
               theme(legend.position = "none") +
               ggtitle("US Suicide Rates per 100,000") +
               labs(caption = "Source: CDC National Vital Statistics Data")+
               theme(text = element_text(family="Avenir", color="#008080"),
                     legend.position="right")
           
       } else if (length(which(divs$len > 1)) == 1){
           
           plot_color <- divs$var[which(divs$len > 1)]
           df %>%
               ggplot(aes(x=Year, y=Rate, color = eval(parse(text = plot_color)))) +
               geom_line() +
               scale_x_continuous(breaks = seq(1900, 2020, by=10)) +
               scale_y_continuous(expand = c(0, 0),limits = c(0, NA)) +
               theme_minimal() +
               labs(color= plot_color) +
               ggtitle("US Suicide Rates per 100,000") +
               labs(caption = "Source: CDC National Vital Statistics Data") + 
               theme(text = element_text(family="Avenir", color="#008080"),
                     legend.position="right")
       } else if (length(which(divs$len > 1)) == 2){
           
           vars <- divs$var[which(divs$len > 1)]
           plot_color <- vars[1]
           plot_line <- vars[2]
           
           if (input$flips == TRUE){
               plot_color <- vars[2]
               plot_line <- vars[1]
           }
           
           df %>%
               ggplot(aes(x=Year, y=Rate, 
                          color = eval(parse(text = plot_color)),
                          linetype = eval(parse(text = plot_line)))) +
               geom_line() +
               scale_x_continuous(breaks = seq(1900, 2020, by=10)) +
               scale_y_continuous(expand = c(0, 0),limits = c(0, NA)) +
               theme_minimal() +
               labs(color= plot_color, linetype = plot_line) +
               ggtitle("US Suicide Rates per 100,000") +
               labs(caption = "Source: CDC National Vital Statistics Data") +
               theme(text = element_text(family="Avenir", color="#008080"),
                     legend.position="right")
       } else if (length(which(divs$len > 1)) > 2){
       }
       
   })

   # variables note
    output$infoText <- renderText(({
        divs <- vars_set()
        if (length(which(divs$len == 0)) > 0){
            miss <- paste(divs$var[which(divs$len == 0)],collapse = ", ")
            paste0("Please select some variables in following cattegories: ",miss,"." )
        } else if (length(which(divs$len > 1)) > 2){
           paste("Please choose fewer variables.")
            }
    }))
    
    # map
    
    output$rateMap <- renderPlot({
        
        years <- c(input$year1,input$year2,input$year3,input$year4, input$year5,input$year6)
        
        df_s<- map_data %>%
            filter(Year %in% years) %>%
            filter(Gender == input$sex2)
        
        if (input$which_rate == "Crude"){
            df_s <- df_s %>%
                select(Year, State.Code, Crude.Rate)
            labs <- "Crude suicide rates"
            
        } else if (input$which_rate == "Adjusted"){
            df_s <- df_s %>%
                select(Year, State.Code, Age.Adjusted.Rate)
            labs <- "Age adjusted suicide rates"
        }
        colnames(df_s) <- c("year","fips", "rate")
        
        if (input$sex2 == "Male"){
            labs <- paste0(labs, ", Men")
        } else if (input$sex2 == "Female"){
            labs <- paste0(labs, ", Women")
        }
        
        
        plot_usmap(regions = "states", data = df_s, values = "rate") +
            scale_fill_viridis_c(name = labs,
                                 label = scales::comma,
                                 alpha = 1,
                                 begin = 0,
                                 end = 1,
                                 direction = -1,
                                 option = "D",
                                 aesthetics = "fill")+
            facet_wrap(~year)+
            theme(legend.position = "bottom", legend.justification = "center")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

