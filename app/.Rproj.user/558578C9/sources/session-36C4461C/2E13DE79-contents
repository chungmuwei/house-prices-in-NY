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
library(shinythemes)
library(ggplot2)
library(equatiomatic)

source("./global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    #themeSelector(),
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("Predicting house price"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # select the variables to fit linear regression model
            selectInput(
              inputId = "house_vars",
              label = "Select the variables to fit the linear regression model",
              choices = full_vars,
              selected = default_vars,
              multiple = TRUE
            ),
            
            # display the input if it is selected
            conditionalPanel(
              condition = "input.house_vars.includes('Waterfront')",
              checkboxInput(
                inputId = "Waterfront",
                label = "Whether property includes waterfront"
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('New.Construct')",
              checkboxInput(
                inputId = "New.Construct",
                label = "Whether the property is a new construction"
              )
            ),
            
            conditionalPanel(
              condition = "input.house_vars.includes('Central.Air')",
              checkboxInput(
                inputId = "Central.Air",
                label = "Whether the house has central air"
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Fuel.Type')",
              selectInput(
                inputId = "Fuel.Type",
                label = "Fuel used for heating",
                choices = fuel_type
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Heat.Type')",
              selectInput(
                inputId = "Heat.Type",
                label = "Type of heating system",
                choice = heat_type
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Sewer.Type')",
              selectInput(
                inputId = "Sewer.Type",
                label = "Type of sewer system",
                choice = sewer_type
              )
            ),
            
            conditionalPanel(
              condition = "input.house_vars.includes('Lot.Size')",
              numericInput(
                inputId = "Lot.Size",
                label = "Size of lot (acres)",
                value = 0.50,
                step = 0.01,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Age')",
              numericInput(
                inputId = "Age",
                label = "age of house (years)",
                value = 20,
                step = 1,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Land.Value')",
              numericInput(
                inputId = "Land.Value",
                label = "Value of land (US dollars)",
                value = 35000,
                step = 1000,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Living.Area')",
              numericInput(
                inputId = "Living.Area",
                label = "Living area (square feet)",
                value = 1800,
                step = 100,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Pct.College')",
              numericInput(
                inputId = "Pct.College",
                label = "Percent of neighborhood that graduated college",
                value = 55,
                step = 5,
                min = 0,
                max = 100
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Fireplaces')",
              numericInput(
                inputId = "Fireplaces",
                label = "Number of fireplaces",
                value = 1,
                step = 1,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Bedrooms')",
              numericInput(
                inputId = "Bedrooms",
                label = "Number of bedrooms",
                value = 3,
                step = 1,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Bathrooms')",
              numericInput(
                inputId = "Bathrooms",
                label = "Number of bathrooms (half bathrooms have no shower or tub)",
                min = 1,
                value = 2,
                step = 0.5,
              )
            ),
            conditionalPanel(
              condition = "input.house_vars.includes('Rooms')",
              numericInput(
                inputId = "Rooms",
                label = "Number of Rooms",
                value = 7,
                step = 1,
                min = 0
              )
            ),
            # Tuning predict method arguments
            # does not affect the standard error at all !!!
            # selectInput(
            #   inputId = "interval",
            #   label = "Type of interval",
            #   choices = c("confidence", "prediction"),
            #   selected = "confidence"
            # ),
            # numericInput(
            #   inputId = "level",
            #   label = "Tolerance/confidence level",
            #   min = 0,
            #   max = 1,
            #   value = 0.95,
            #   step = 0.01
            # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Selected regression model to predict house price:"),
           #withMathJax("$$\\operatorname{\\widehat{Price}} = 3136.91 + 123131.17(\\operatorname{Waterfront}_{\\operatorname{1}}) + 0.91(\\operatorname{Land.Value}) + 71.03(\\operatorname{Living.Area}) + 27058.46(\\operatorname{Bathrooms})$$"),
           eqOutput("formula"),
           helpText("Categorical variables have subscript indicating its level. Replace the one you choose with \\(1\\) and the others with \\(0\\) to calculate the predicted house price."),
           h3("Guide"),
           p("1. Select the variables you want to fit the linear regression model for predicting house price."),
           p("2, Fill in the value of each variable you choose."),
           h3("Prediction"),
           uiOutput("predictedPrice")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # reactive function to return the model object user fit
    house_model <- reactive({
      
      # null model
      lm_formula = "Price ~ 1"
      vars = input$house_vars
      if (length(vars) == 0) {
        return(lm(lm_formula, data = house))
      }
      
      for (i in 1:length(vars)) {
        if (i == 1) {
          lm_formula = "Price ~"
        }
        lm_formula = 
          paste(
            lm_formula, 
            if_else(i < length(vars), paste(vars[i], "+"), vars[i])
            )
      }
      
      return(lm(lm_formula, data = house))
    })
    
    print_model <- reactive({
      return(extract_eq(house_model()))
    })
  
    # reactive function to return the predicted price for the new data
    # if any of the input value is invalid, return FALSE
    predict_price <- reactive({
      
      new_obs = data.frame(
        Waterfront = if_else(input$Waterfront, "1", "0") ,
        New.Construct = if_else(input$New.Construct, "1", "0"),
        Central.Air = if_else(input$Central.Air, "1", "0"),
        
        Heat.Type = input$Heat.Type,
        Fuel.Type = input$Fuel.Type,
        Sewer.Type = input$Sewer.Type,
        
        Lot.Size = input$Lot.Size,
        Age = input$Age,
        Land.Value = input$Land.Value, 
        Living.Area = input$Living.Area,
        Pct.College = input$Pct.College,
        Fireplaces = input$Fireplaces,
        Bedrooms = input$Bedrooms,
        Bathrooms = input$Bathrooms,
        Rooms = input$Rooms
      )
      
      
      pred = predict.lm(house_model(), # the lm model user selected 
                        new_obs,       # new data to predict price
                        interval = "confidence", 
                        #interval = input$interval, 
                        se.fit = TRUE, 
                        level = 0.95
                        #level = input$level
                        )
      return(
        paste(
          format(round(pred$fit[1]), big.mark = ","), 
          format(round(pred$se.fit), big.mark = ","),
          sep = "±"
        )
      )
    })
    
    output$formula <- renderEq(
      if (house_model()$rank == 1) {
         "\\operatorname{\\widehat{Price}} = 211545"
      } else {
        extract_eq(house_model(), use_coefs = TRUE) 
      }
    )
    
    output$predictedPrice <- renderUI (
      if (!isTruthy(input$Land.Value)) {
        p("Value of Land is missing or not numeric.")
      } else if (input$Land.Value <= 0) {
        p("Value of Land must be positive.")
      } else if (!isTruthy(input$Living.Area)) {
        p("Living area is missing or not numeric.")
      } else if (input$Living.Area <= 0) {
        p("Living area must be positive.")
      } else if (!isTruthy(input$Bathrooms)) {
        p("Number of bathroom is missing or not numeric.")
      } else if (input$Bathrooms < 1) {
        p("Number of bathroom must be at least 1.")
      } else {
        withMathJax(sprintf("The predicted house price is \\(%s\\) US dollars.", predict_price()))  
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
