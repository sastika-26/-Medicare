library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# Sample medicine data
medicine_data <- data.frame(
  Medicine = c("Paracetamol", "Vitamin D", "Amlodipine", "Cetirizine"),
  Total_Tablets = c(30, 60, 30, 20),
  Tablets_Used = c(12, 20, 10, 8),
  Tablets_Per_Day = c(2, 1, 1, 1),
  stringsAsFactors = FALSE
)


ui <- dashboardPage(
  
  dashboardHeader(
    title = "MediAhead"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Medicine Dashboard",
        tabName = "dashboard",
        icon = icon("tablets")
      ),
      
      menuItem(
        "Add Medicine",
        tabName = "add",
        icon = icon("plus-circle")
      )
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ==============================
      # DASHBOARD
      # ==============================
      
      tabItem(
        tabName = "dashboard",
        
        h2("Home Medicine Intelligence"),
        
        fluidRow(
          
          valueBoxOutput("total_medicines", width = 3),
          
          valueBoxOutput("critical_medicines", width = 3),
          
          valueBoxOutput("warning_medicines", width = 3),
          
          valueBoxOutput("safe_medicines", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            title = "Medicine Stock Prediction",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("medicine_table")
          )
          
        )
        
      ),
      
      
      # ==============================
      # ADD MEDICINE
      # ==============================
      
      tabItem(
        tabName = "add",
        
        h2("Add Medicine"),
        
        fluidRow(
          
          box(
            title = "Medicine Information",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            textInput(
              "medicine_name",
              "Medicine Name"
            ),
            
            numericInput(
              "total_tablets",
              "Total Tablets",
              value = 30,
              min = 1
            ),
            
            numericInput(
              "used_tablets",
              "Tablets Already Used",
              value = 0,
              min = 0
            ),
            
            numericInput(
              "daily_usage",
              "Tablets Per Day",
              value = 1,
              min = 0.1,
              step = 0.1
            ),
            
            numericInput(
              "safety_days",
              "Safety Buffer (Days)",
              value = 3,
              min = 0
            ),
            
            actionButton(
              "add_medicine",
              "Add Medicine"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
)


server <- function(input, output, session) {
  
  medicines <- reactiveVal(medicine_data)
  
  
  # ==========================================
  # ADD MEDICINE
  # ==========================================
  
  observeEvent(input$add_medicine, {
    
    if (input$medicine_name == "") {
      
      showNotification(
        "Please enter medicine name",
        type = "error"
      )
      
      return()
      
    }
    
    
    new_medicine <- data.frame(
      
      Medicine = input$medicine_name,
      
      Total_Tablets = input$total_tablets,
      
      Tablets_Used = input$used_tablets,
      
      Tablets_Per_Day = input$daily_usage,
      
      stringsAsFactors = FALSE
      
    )
    
    
    medicines(
      bind_rows(
        medicines(),
        new_medicine
      )
    )
    
    
    showNotification(
      "Medicine added successfully!",
      type = "message"
    )
    
  })
  
  
  # ==========================================
  # PREDICTION
  # ==========================================
  
  prediction_data <- reactive({
    
    data <- medicines()
    
    data %>%
      
      mutate(
        
        Remaining_Tablets =
          Total_Tablets - Tablets_Used,
        
        Estimated_Days_Left =
          round(
            Remaining_Tablets / Tablets_Per_Day,
            1
          ),
        
        Predicted_Depletion_Date =
          Sys.Date() +
          ceiling(Estimated_Days_Left),
        
        Recommended_Purchase_Date =
          Predicted_Depletion_Date - 3,
        
        Status = case_when(
          
          Estimated_Days_Left <= 3 ~
            "CRITICAL",
          
          Estimated_Days_Left <= 7 ~
            "WARNING",
          
          TRUE ~
            "SAFE"
          
        )
        
      )
    
  })
  
  
  # ==========================================
  # TOTAL MEDICINES
  # ==========================================
  
  output$total_medicines <- renderValueBox({
    
    valueBox(
      
      value = nrow(
        prediction_data()
      ),
      
      subtitle = "Total Medicines",
      
      icon = icon("pills"),
      
      color = "blue"
      
    )
    
  })
  
  
  # ==========================================
  # CRITICAL
  # ==========================================
  
  output$critical_medicines <- renderValueBox({
    
    valueBox(
      
      value = sum(
        prediction_data()$Status == "CRITICAL"
      ),
      
      subtitle = "Critical",
      
      icon = icon("triangle-exclamation"),
      
      color = "red"
      
    )
    
  })
  
  
  # ==========================================
  # WARNING
  # ==========================================
  
  output$warning_medicines <- renderValueBox({
    
    valueBox(
      
      value = sum(
        prediction_data()$Status == "WARNING"
      ),
      
      subtitle = "Warning",
      
      icon = icon("bell"),
      
      color = "yellow"
      
    )
    
  })
  
  
  # ==========================================
  # SAFE
  # ==========================================
  
  output$safe_medicines <- renderValueBox({
    
    valueBox(
      
      value = sum(
        prediction_data()$Status == "SAFE"
      ),
      
      subtitle = "Safe",
      
      icon = icon("circle-check"),
      
      color = "green"
      
    )
    
  })
  
  
  # ==========================================
  # TABLE
  # ==========================================
  
  output$medicine_table <- renderDT({
    
    data <- prediction_data()
    
    
    display_data <- data %>%
      
      select(
        
        Medicine,
        
        Total_Tablets,
        
        Tablets_Used,
        
        Remaining_Tablets,
        
        Tablets_Per_Day,
        
        Estimated_Days_Left,
        
        Predicted_Depletion_Date,
        
        Recommended_Purchase_Date,
        
        Status
        
      )
    
    
    datatable(
      
      display_data,
      
      rownames = FALSE,
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE
        
      ),
      
      colnames = c(
        
        "Medicine",
        
        "Total Tablets",
        
        "Used",
        
        "Remaining",
        
        "Daily Usage",
        
        "Days Left",
        
        "Predicted Depletion",
        
        "Buy Before",
        
        "Status"
        
      )
      
    )
    
  })
  
}


# ==========================================
# START APP
# ==========================================

shinyApp(
  ui = ui,
  server = server
)