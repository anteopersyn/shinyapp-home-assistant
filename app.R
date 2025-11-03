# =====================
# ShinyApp
# Olivia Beaulieu - Anteo Persyn
# D3S M1
# =====================

library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(ggplot2)
library(randomForest)
library(tidyr)
library(scales) 


data <- read.csv("train.csv", header = TRUE, sep = ",")

# ---- Rename variables, convert units, ensure numeric, and regroup factors ----
data <- data %>%
  
  mutate(
    LivingArea   = Gr.Liv.Area * 0.092903, 
    LotArea      = Lot.Area * 0.092903,   
    BasementArea = Total.Bsmt.SF * 0.092903 
  ) %>%
  
  
  rename(
    OverallQuality = Overall.Qual,
    YearBuilt      = Year.Built,
    Bedrooms       = Bedroom.AbvGr,
    GarageCars     = Garage.Cars,
    HouseStyle     = House.Style,
    Zoning         = MS.Zoning
  ) %>%
  
  
  mutate(
    Bathrooms   = Full.Bath + Half.Bath,
    Fireplaces  = ifelse(Fireplaces > 0, 1, 0),
    
    HouseStyle = case_when(
      HouseStyle %in% c("1Story")   ~ "Single-story",
      HouseStyle %in% c("2Story")   ~ "Two-story",
      HouseStyle %in% c("1.5Fin")   ~ "Partial-story",
      TRUE                           ~ "Other"
    ) %>% factor(levels = c("Single-story", "Two-story", "Partial-story", "Other")),
    
    Zoning = case_when(
      Zoning == "RL"               ~ "Residential low density",
      Zoning == "RM"               ~ "Residential medium density",
      Zoning == "FV"               ~ "Floating village residential",
      TRUE                           ~ "Other"
    ) %>% factor(levels = c("Residential low density", "Residential medium density",
                            "Floating village residential", "Other")),
    
    
    Neighborhood = case_when(
      Neighborhood %in% c("Names", "NWAmes", "NoRidge", "NridgHt", "Veenker") ~ "North & North-West",
      Neighborhood %in% c("SWISU", "Timber", "StoneBr") ~ "South & South-West",
      Neighborhood %in% c("ClearCr", "MeadowV", "BrDale") ~ "East & Far East",
      Neighborhood %in% c("CollgCr", "Crawfor", "Edwards", "OldTown", "Sawyer", "SawyerW") ~ "Central & Established",
      TRUE ~ "Other"
    ) %>% factor(levels = c(
      "North & North-West",
      "Central & Established",
      "South & South-West",
      "East & Far East",
      "Other"
    ))
  ) %>%
  
  
  mutate_at(vars(LivingArea, LotArea, OverallQuality, YearBuilt, BasementArea,
                 Bedrooms, GarageCars, Fireplaces, Bathrooms),
            as.numeric)


data_model <- data %>%
  select(SalePrice, LivingArea, LotArea, OverallQuality, YearBuilt, BasementArea,
         Bedrooms, Bathrooms, GarageCars, Fireplaces, Neighborhood, HouseStyle, Zoning)


num_vars <- c(
  "LivingArea",
  "LotArea",
  "OverallQuality",
  "BasementArea",
  "Bedrooms",
  "GarageCars",
  "Bathrooms",
  "SalePrice"
)

int_vars <- c("OverallQuality", "Bedrooms", "GarageCars", "Bathrooms")

for(v in num_vars){
  p1  <- quantile(data_model[[v]], 0.01, na.rm = TRUE)
  p99 <- quantile(data_model[[v]], 0.99, na.rm = TRUE)
  data_model[[v]] <- pmin(pmax(data_model[[v]], p1), p99)
  
  if(v %in% int_vars){
    data_model[[v]] <- round(data_model[[v]])
  } else {
    data_model[[v]] <- round(data_model[[v]], 2)
  }
}

# ---- Random Forest training ---- 
set.seed(2025)
rf_model <- randomForest(
  SalePrice ~ LivingArea + LotArea + OverallQuality + YearBuilt + BasementArea +
    Bedrooms + Bathrooms + GarageCars + Fireplaces +
    Neighborhood + HouseStyle + Zoning,
  data = data_model,
  ntree = 1000,
  na.action = na.roughfix
)

# ============================================================
# Code for each button's view
# ============================================================

page_explore <- function(data_model) {
  wellpanel_style <- "background-color: #EFEFEF; color: #212529; 
  border-radius:12px; padding:20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border: 1px solid #D1D9E0;"
  
  tagList(
    br(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          style = wellpanel_style,
          h4("Filters"),
          sliderInput("year_filter", "Construction year:",
                      min = min(data_model$YearBuilt, na.rm = TRUE),
                      max = max(data_model$YearBuilt, na.rm = TRUE),
                      value = c(min(data_model$YearBuilt, na.rm = TRUE),
                                max(data_model$YearBuilt, na.rm = TRUE)),
                      sep = ""),
          checkboxGroupInput("style_filter", "House style:",
                             choices = levels(data_model$HouseStyle),
                             selected = levels(data_model$HouseStyle),
                             inline = FALSE),
          checkboxGroupInput("zoning_filter", "Zoning:",
                             choices = levels(data_model$Zoning),
                             selected = levels(data_model$Zoning),
                             inline = FALSE),
          sliderInput("price_range", "Price range (k$):",
                      min = min(data_model$SalePrice, na.rm = TRUE),
                      max = max(data_model$SalePrice, na.rm = TRUE),
                      value = c(min(data_model$SalePrice, na.rm = TRUE),
                                max(data_model$SalePrice, na.rm = TRUE))),
          sliderInput("area_range", "Living area (m²):",
                      min = min(data_model$LivingArea, na.rm = TRUE),
                      max = max(data_model$LivingArea, na.rm = TRUE),
                      value = c(min(data_model$LivingArea, na.rm = TRUE),
                                max(data_model$LivingArea, na.rm = TRUE)))
        )
      ),
      column(
        width = 8,
        wellPanel(
          style = wellpanel_style,
          h4("Market snapshot"),
          fluidRow(
            column(4, strong("Filtered homes:"), textOutput("kpi_n")),
            column(4, strong("Median price($):"), textOutput("kpi_med_price")),
            column(4, strong("Median area (m²):"), textOutput("kpi_med_area"))
          ),
          br(),
          plotOutput("hist_price", height = "260px"),
          br(),
          plotOutput("scatter_area_price", height = "260px"),
          br(),
          plotOutput("box_price_Neighborhood", height = "260px"),
          br(),
          plotOutput("hist_bath_bed", height = "260px")
        )
      )
    )
  )
}

page_advisor <- function(data_model) {
  wellpanel_style <- "background-color: #EFEFEF; color: #212529; border-radius:12px; 
  padding:20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border: 1px solid #D1D9E0;"
  
  tagList(
    br(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          style = wellpanel_style,
          h4("Home characteristics"),
          numericInput("living_input", "Living area (m²):", value = 120, min = 20, step = 10),
          numericInput("lot_input", "Lot area (m²):", value = 400, min = 50, step = 10),
          numericInput("basement_input", "Basement area (m²):", value = 50, min = 0, step = 5),
          numericInput("year_input", "Construction year:", value = 1980, min = 1900, max = 2008, step = 1),
          numericInput("quality_input", "Overall quality (1–10):", value = 6, min = 1, max = 10, step = 1),
          numericInput("bedrooms_input", "Bedrooms:", value = 3, min = 1, step = 1),
          numericInput("bathrooms_input", "Bathrooms:", value = 2, min = 1, step = 1),
          numericInput("garage_input", "Garage capacity (cars):", value = 2, min = 0, step = 1),
          checkboxInput("fireplace_input", "Fireplace present?", value = FALSE),
          selectInput("house_input", "House style:",
                      choices = levels(data_model$HouseStyle),
                      selected = "Single-story"),
          selectInput("zoning_input", "Zoning:",
                      choices = levels(data_model$Zoning),
                      selected = "Residential low density"),
          selectInput("Neighborhood_input", "Neighborhood:",
                      choices = levels(data_model$Neighborhood),
                      selected = levels(data_model$Neighborhood)[1]),
          br(),
          actionButton("predict_btn", "Estimate price", class = "btn-primary")
        )
      ),
      column(
        width = 8,
        wellPanel(
          style = wellpanel_style,
          h4("Estimated price"),
          textOutput("price_rf"),
          br(),
          h4("Advice and insights"),
          textOutput("advice1"),
          textOutput("advice2"),
          br(),
          uiOutput("details_block")
        )
      )
    )
  )
}

page_home <- function() {
  tagList(
    fluidRow(
      column(12, align = "center",
             tags$img(src = "home2.jpg", 
                      style = "max-width: 70%; 
                                  height: auto; 
                                  margin-top: 30px; 
                                  border-radius: 10px; 
                                  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);")
      )
    )
  )
}
# ============================================================
# UI 
# ============================================================
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly", 
    bg = "#C9DFF0",   
    fg = "#212529",  
    primary = "#003D73", 
    secondary = "#5F9EA0",
    base_font = NULL,
    heading_font = NULL 
  ),
  div(
    style = "margin-top: 30px;", 
    actionLink("btn_home", 
               h2("From data to dream price— Instantly!", align = "center", style = "color: #003D73;"), # Titre en bleu TSE
               style = "display: block; text-decoration: none; color: inherit;")
  ),
  
  br(),
  
  fluidRow(
    column(6,
           wellPanel(
             style = "background-color: #EFEFEF; color: #212529;  
                      border-radius:12px; padding:40px;
                      box-shadow: 4px 4px 15px rgba(0,0,0,0.15);
                      transition: transform 0.2s; border: 1px solid #DEDEDE;", 
             h3("Explore the market", align = "center"),
             p("Dive in! Explore the housing market data like never before."),
             actionButton("btn_explore", "Go to exploration", class = "btn-primary btn-lg"),
             tags$style(HTML("
               #btn_explore:hover {transform: scale(1.05); box-shadow: 6px 6px 20px rgba(0,0,0,0.3);}
             "))
           )
    ),
    column(6,
           wellPanel(
             style = "background-color: #EFEFEF; color: #212529;  
                      border-radius:12px; padding:40px;
                      box-shadow: 4px 4px 15px rgba(0,0,0,0.15);
                      transition: transform 0.2s; border: 1px solid #DEDEDE;",
             h3("Your home advisor", align = "center"),
             p("Estimate your home's priceand receive personalized advice."),
             actionButton("btn_advisor", "Go to advisor", class = "btn-primary btn-lg"),
             tags$style(HTML("
               #btn_advisor:hover {transform: scale(1.05); box-shadow: 6px 6px 20px rgba(0,0,0,0.3);}
             "))
           )
    )
  ),
  
  br(),
  uiOutput("dynamic_section")
)


# ============================================================
# SERVER 
# ============================================================
server <- function(input, output, session) {
  
  app_state <- reactiveValues(page = "home")
  
  observeEvent(input$btn_home, {
    app_state$page <- "home"
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_explore, {
    app_state$page <- "explore"
  })
  
  observeEvent(input$btn_advisor, {
    app_state$page <- "advisor"
  })
  
  output$dynamic_section <- renderUI({
    req(app_state$page)
    
    if (app_state$page == "explore") {
      return(page_explore(data_model)) 
    } else if (app_state$page == "advisor") {
      return(page_advisor(data_model))
    } else {
      return(page_home()) 
    }
  })
  
  # ---- Explore data ----
  df_explore <- reactive({
    d <- data_model %>%
      filter(
        YearBuilt >= input$year_filter[1], YearBuilt <= input$year_filter[2],
        SalePrice >= input$price_range[1], SalePrice <= input$price_range[2],
        LivingArea >= input$area_range[1], LivingArea <= input$area_range[2]
      )
    
    # ---- House style filter ----
    if (!is.null(input$style_filter) && length(input$style_filter) > 0) {
      d <- d %>% filter(HouseStyle %in% input$style_filter)
    }
    
    # ---- Zoning filter ----
    if (!is.null(input$zoning_filter) && length(input$zoning_filter) > 0) {
      d <- d %>% filter(Zoning %in% input$zoning_filter)
    }
    
    d
  })
  
  # ---- Market summary ----
  output$kpi_n <- renderText({ nrow(df_explore()) })
  output$kpi_med_price <- renderText({
    d <- df_explore(); if (!nrow(d)) return("—")
    paste0("$", format(round(median(d$SalePrice), 0), big.mark = " "))
  })
  output$kpi_med_area <- renderText({
    d <- df_explore(); if (!nrow(d)) return("—")
    paste0(round(median(d$LivingArea), 1), " m²")
  })
  
  # ---- Plots ----
  output$hist_price <- renderPlot({
    d <- df_explore()
    ggplot(d, aes(SalePrice)) +
      geom_histogram(bins = 30, fill = "#003D73", color = "#EFEFEF") +
      scale_x_continuous(
        labels = scales::label_number(scale = 1/1000, suffix = "")) +
      labs(title = "Price distribution", x = "Price (k$)", y = "Count") +
    theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#EFEFEF", color = NA),
        panel.background = element_rect(fill = "#EFEFEF", color = NA),
        axis.line = element_line(color = "#A9A9A9", linewidth = 0.5), 
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color = "#D3D3D3")
      )})
  
  output$scatter_area_price <- renderPlot({
    d <- df_explore()
    ggplot(d, aes(LivingArea, SalePrice)) +
      geom_point(alpha = 0.6, color = "#003d73") +
      geom_smooth(se = FALSE, color = "red") +
      scale_y_continuous(
        labels = scales::label_number(scale = 1/1000, suffix = "")) +
      labs(title = "Price vs Living area", x = "Living area (m²)", y = "Price (k$)") +
      theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#EFEFEF", color = NA),
        panel.background = element_rect(fill = "#EFEFEF", color = NA),
        axis.line = element_line(color = "#A9A9A9", linewidth = 0.5), 
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color = "#D3D3D3")
      )})
  
  output$box_price_Neighborhood <- renderPlot({
    d <- df_explore()
    levels(d$Neighborhood) <- c(
      "North &\nNorth-West",
      "Central &\nEstablished",
      "South &\nSouth-West",
      "East &\nFar East",
      "Other"
    )
    ggplot(d, aes(Neighborhood, SalePrice)) +
      geom_boxplot(fill = "#003d73", alpha = 0.7) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1/1000, suffix = "")
      ) +
      labs(
        title = "Price distribution by Neighborhood", 
        x = "Neighborhood", 
        y = "Price (k$)"
      ) +
      theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#EFEFEF", color = NA),
        panel.background = element_rect(fill = "#EFEFEF", color = NA),
        axis.line = element_line(color = "#A9A9A9", linewidth = 0.5), 
        panel.grid.minor = element_line(color = "#D3D3D3")
      )})
  
  output$hist_bath_bed <- renderPlot({
    d <- df_explore() %>%
      pivot_longer(cols = c(Bedrooms, Bathrooms), names_to = "Type", values_to = "Count")
    ggplot(d, aes(Count, fill = Type)) +
      geom_histogram(position = "dodge", bins = 10, color = "white") +
      scale_fill_manual(values = c("Bedrooms" = "#4C84B4", "Bathrooms" = "#003d73")) +
      labs(title = "Distribution of bedrooms and bathrooms", x = "Count", y = "Number of houses", fill = "")+
      theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#EFEFEF", color = NA),
        panel.background = element_rect(fill = "#EFEFEF", color = NA),
        axis.line = element_line(color = "#A9A9A9", linewidth = 0.5), 
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color = "#D3D3D3")
      )})
  
  
  # ---- Advisor ----
  observeEvent(input$predict_btn, {  
    
    # Create new observation from user inputs
    new_data <- tibble(
      LivingArea     = input$living_input,
      LotArea        = input$lot_input,
      BasementArea   = input$basement_input,
      YearBuilt      = input$year_input,
      OverallQuality = input$quality_input,
      Bedrooms       = input$bedrooms_input,
      Bathrooms      = input$bathrooms_input,
      GarageCars     = input$garage_input,
      Fireplaces     = ifelse(input$fireplace_input, 1, 0),
      Neighborhood   = factor(input$Neighborhood_input, levels = levels(data_model$Neighborhood)),
      HouseStyle     = factor(input$house_input, levels = levels(data_model$HouseStyle)),
      Zoning         = factor(input$zoning_input, levels = levels(data_model$Zoning))
    )
    
    # ---- Scenarios
    sc_area10 <- new_data; sc_area10$LivingArea  <- sc_area10$LivingArea + 10
    sc_area15 <- new_data; sc_area15$LivingArea  <- sc_area15$LivingArea + 15
    sc_area25 <- new_data; sc_area25$LivingArea  <- sc_area25$LivingArea + 25
    sc_bedroom <- new_data; sc_bedroom$Bedrooms  <- sc_bedroom$Bedrooms + 1
    sc_bathroom <- new_data; sc_bathroom$Bathrooms <- sc_bathroom$Bathrooms + 1
    sc_garage <- new_data; sc_garage$GarageCars <- sc_garage$GarageCars + 1
    sc_fireplace <- new_data; sc_fireplace$Fireplaces <- 1
    
    # ---- Predictions ----
    p_base <- as.numeric(predict(rf_model, newdata = new_data))
    p_area10 <- as.numeric(predict(rf_model, newdata = sc_area10))
    p_area15 <- as.numeric(predict(rf_model, newdata = sc_area15))
    p_area25 <- as.numeric(predict(rf_model, newdata = sc_area25))
    p_bedroom <- as.numeric(predict(rf_model, newdata = sc_bedroom))
    p_bathroom <- as.numeric(predict(rf_model, newdata = sc_bathroom))
    p_garage <- as.numeric(predict(rf_model, newdata = sc_garage))
    p_fireplace <- as.numeric(predict(rf_model, newdata = sc_fireplace))
    
    output$price_rf <- renderText({
      paste0("Predicted price: $", format(round(p_base, 0), big.mark = " "))
    })
    
    # ---- Comparative Scenario Table ----
    comp <- tibble(
      Scenario = c(
        "As entered", "+10 m²", "+15 m²", "+25 m²",
        "+1 bedroom", "+1 bathroom", "+1 garage", "+ fireplace"
      ),
      Predicted_Price = c(
        p_base, p_area10, p_area15, p_area25,
        p_bedroom, p_bathroom, p_garage, p_fireplace
      ),
      Added_Area = c(0, 10, 15, 25, 0, 0, 0, 0)
    ) %>%
      mutate(
        Delta_vs_Base = Predicted_Price - p_base,
        Uplift_pct    = 100 * Delta_vs_Base / p_base,
        Gain_per_m2    = ifelse(Added_Area > 0, Delta_vs_Base / Added_Area, NA)
      )
    
    comp_simple <- comp %>% filter(Scenario != "As entered")
    best_row <- comp_simple[which.max(comp_simple$Delta_vs_Base), ]
    
    # ---- Identify best expansion scenario (gain/m²) ----
    comp_area <- comp %>% filter(Added_Area > 0)
    best_area_row <- comp_area[which.max(comp_area$Gain_per_m2), ]
    
    output$advice1 <- renderText({
      paste0(
        "The best upgrade is ", best_row$Scenario,
        ": it would increase the estimated house value by $",
        format(round(best_row$Delta_vs_Base, 0), big.mark = " "),
        " (", sprintf("%.1f", best_row$Uplift_pct), "%)." 
      )
    })
    
    output$advice2 <- renderText({
      paste0(
        "Regarding expansions, adding ", best_area_row$Added_Area, " m² offers the best marginal return: $",
        format(round(best_area_row$Gain_per_m2, 0), big.mark = " "),
        " value increase per m² added." 
      )
    })
    
    # ---- Detailed table ----
    output$comparison_table <- renderTable({
      comp %>%
        transmute(
          `Scenario`              = Scenario,
          `Predicted Price ($)`  = format(round(Predicted_Price, 0), big.mark = " "),
          `Gain vs Base ($)`     = format(round(Delta_vs_Base,    0), big.mark = " "),
          `Uplift (%)`           = sprintf("%.1f%%", Uplift_pct),
          `Gain per m² ($)`      = ifelse(is.na(Gain_per_m2), "—", format(round(Gain_per_m2, 0), big.mark = " "))
        )
    })
    
    # ---- Display details ----
    output$details_block <- renderUI({
      tagList(
        tags$em("More details below"),
        br(), br(),
        tableOutput("comparison_table")
      )
    })
  })
}

# ---- Run app ----
shinyApp(ui = ui, server = server)

