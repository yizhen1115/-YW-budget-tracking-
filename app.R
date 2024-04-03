Sys.setlocale(locale = "en_US.UTF-8")
# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(DT)
library(stringr)
library(lubridate)
library(shinymanager)

###################### Data import

# the Excel
file_paths <- c( 
  "EXAMPLE.xlsx",
  "EXAMPLE.xlsx",
  "EXAMPLE.xlsx",
  "EXAMPLE.xlsx",
  "EXAMPLE.xlsx"
)

# Vector of grant names (must correspond to the order of file_paths)
grant_names <- c("A", "B", "C","D", "E")

###################### Expenses Table (YR1 Summary)

expenses_dfs <- vector("list", length(file_paths))

first_rows <- vector("list", length(file_paths))

for(i in seq_along(file_paths)){
  
  excel_data <- read_excel(file_paths[[i]], sheet = "YR1 SUMMARY")
  
  category <- excel_data[,1]
  
  budget_amount <- excel_data[,2] # Budget
  
  spent <- excel_data[,3] # Expenses
  
  variance <- excel_data[,4] # Variance
  
  first_row <- excel_data[1,] #Extract grant name and daterange
  
  first_rows[[i]] <- first_row
  
  expenses_dfs[[i]] <- data.frame(category, budget_amount, spent, variance)
  
  names(expenses_dfs[[i]]) <- c("category", "budget_amount", "spent", "variance")
  
}

# Build a date datafram
date_data <- data.frame(grant = grant_names, start_date = NA, end_date = NA)

for(i in seq_along(first_rows)) {
  date_string <- names(first_rows[[i]])
  dates <- str_extract_all(date_string, "\\b[A-Za-z]+\\s\\d{1,2},\\s\\d{4}\\b")[[1]]
  
  date_data$start_date[i] <- mdy(dates[1])
  date_data$end_date[i] <- mdy(dates[2])
  
}
date_data <- date_data[,2:3]
date_data$start_date <- format(as.Date(date_data$start_date), "%Y-%m-%d")
date_data$end_date <- format(as.Date(date_data$end_date), "%Y-%m-%d")

print(date_data)

# Name list elements
names(expenses_dfs) <- grant_names

expenses_dfs$`CIRCLE Research Core`
expenses_dfs$`Family Spirit Strengths-R01`
expenses_dfs$`Native-RISE`
expenses_dfs$`Comic Relief`
expenses_dfs$`PAWS`

expenses_dfs <- lapply(expenses_dfs, function(x) x[-c(1:1),])

# CIRCLE Research Core
expenses_dfs$`CIRCLE Research Core` <- expenses_dfs$`CIRCLE Research Core` %>%
  filter(category %in% c("Equipment/Facility Rental/Fees", "Travel", "Vehicle Expenses", "Materials & Supplies", "Incentives"))

# Family Spirit Strengths-R01
expenses_dfs$`Family Spirit Strengths-R01` <- expenses_dfs$`Family Spirit Strengths-R01` %>%
  filter(category %in% c("Equipment/Facility Rental/Fees", "Travel", "Vehicle Expenses", "Materials & Supplies", "Incentives"))

# Native-RISE
expenses_dfs$`Native-RISE` <- expenses_dfs$`Native-RISE` %>%
  filter(category %in% c("Equipment/Facility Rental/Fees", "Travel", "Vehicle Expenses", "Materials & Supplies", "Incentives"))

# Comic Relief
expenses_dfs$`Comic Relief` <- expenses_dfs$`Comic Relief` %>%
  filter(category %in% c("Equipment/Facility Rental/Fees", "Travel", "Vehicle Expenses", "Materials & Supplies", "Incentives"))

# PAWS
expenses_dfs$`PAWS` <- expenses_dfs$`PAWS` %>%
  filter(category %in% c("Equipment/Facility Rental/Fees", "Travel", "Vehicle Expenses", "Materials & Supplies", "Incentives"))

expenses_dfs <- lapply(expenses_dfs, function(df) {
  df$budget_amount <- as.numeric(as.character(df$budget_amount))
  df$spent <- as.numeric(as.character(df$spent))
  df$variance <- as.numeric(as.character(df$variance))
  return(df)
})

# Combine data frames into one
totalexpenses_df <- bind_rows(expenses_dfs, .id="grant")
print(totalexpenses_df)

# Custom function to format numbers as dollar amounts
dollar_format_custom <- function() {
  function(x) {
    paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
  }
}

###################### Data import

# the Excel
file_paths <- c( 
  "FSS_Year 1 Operational Budget-Expense Tracking_EXAMPLE.xlsx",
  "CIRCLE Center _Research Core_Year 1 Operational Budget-Expense Tracking_EXAMPLE.xlsx",
  "Native-RISE_Year 1 Operational Budget-Expense Tracking_EXAMPLE.xlsx",
  "FSSComicRelief_Year2_Tracker_EXAMPLE.xlsx",
  "PAWs_Year 1 Operational Budget-Expense Tracking_EXAMPLE.xlsx"
)

# Vector of grant names (must correspond to the order of file_paths)
grant_names <- c("Family Spirit Strengths-R01", "CIRCLE Research Core", "Native-RISE","Comic Relief", "PAWS")


###################### Salary Allocations

# Create empty list to store salary df's
salary_dfs <- vector("list", length(file_paths))

for(i in seq_along(file_paths)){
  
  excel_data <- read_excel(file_paths[[i]], sheet = "Salary Allocations")
  
  names <- excel_data[,2]
  
  submitted_amount <- excel_data[,3] # SUBMITTED EFFORT
  
  percent_allocated <- excel_data[,5] # ALLOCATED EFFORT
  
  amount_allocated <- excel_data[,6] # ALLOCATED AMOUNT*  
  
  team <- excel_data[,12]
  
  salary_dfs[[i]] <- data.frame(names, submitted_amount, percent_allocated, amount_allocated, team )
  
  names(salary_dfs[[i]]) <- c("names", "submitted_amount", "percent_allocated", "amount_allocated", "team")
  
}

# Name list elements
names(salary_dfs) <- grant_names
salary_dfs$`CIRCLE Research Core`
salary_dfs$`Family Spirit Strengths-R01`
salary_dfs$`Native-RISE`
salary_dfs$`Comic Relief`
salary_dfs$`PAWS`
salary_dfs <- lapply(salary_dfs, function(x) x[-c(1:1),])

# Remove rows with NA values for submitted_amount and percent_allocated in each data frame

# CIRCLE Research Core
salary_dfs$`CIRCLE Research Core` <- salary_dfs$`CIRCLE Research Core` %>%
  filter(!is.na(submitted_amount) & !is.na(percent_allocated))

# Family Spirit Strengths-R01
salary_dfs$`Family Spirit Strengths-R01` <- salary_dfs$`Family Spirit Strengths-R01` %>%
  filter(!is.na(submitted_amount) & !is.na(percent_allocated))

# Native-RISE
salary_dfs$`Native-RISE` <- salary_dfs$`Native-RISE` %>%
  filter(!is.na(submitted_amount) & !is.na(percent_allocated))

# Comic Relief
salary_dfs$`Comic Relief` <- salary_dfs$`Comic Relief` %>%
  filter(!is.na(submitted_amount) & !is.na(percent_allocated))

# PAWS
salary_dfs$`PAWS` <- salary_dfs$`PAWS` %>%
  filter(!is.na(submitted_amount) & !is.na(percent_allocated))

# Clean up formatting
for(i in seq_along(salary_dfs)) {
  
  print(paste("Inspecting", names(salary_dfs[i])))  
  print(str(salary_dfs[[i]]))
  
  # Extract the dataframe
  df <- salary_dfs[[i]]
  
  # Inspect non-numeric values
  non_numeric <- df$percent_allocated[!grepl("^-?\\d*\\.?\\d*%?$", df$percent_allocated)]
  if(length(non_numeric) > 0) {
    print(paste("Non-numeric values found in percent_allocated:", toString(non_numeric)))
  }
  
  # Convert percent_allocated to numeric, removing all non-numeric characters
  df$percent_allocated <- as.numeric(gsub("[^0-9.-]", "", df$percent_allocated))
  
  # Handle NAs if necessary
  # df$percent_allocated[is.na(df$percent_allocated)] <- 0 # Uncomment this line if you want to replace NAs with 0
  
  # Assign the modified dataframe back to the list
  salary_dfs[[i]] <- df
  
  print(str(salary_dfs[[i]]))
}

#salary_dfs$`Native-RISE`$percent_allocated <-  
# as.numeric(gsub("%", "", salary_dfs$`Native-RISE`$percent_allocated))

# Combine data frames into one
total_df <- bind_rows(salary_dfs, .id="grant")
#head(total_df)

#Remove NA rows
total_df <- total_df %>%
  filter(!is.na(names) & !is.na(percent_allocated))

str(total_df)

total_df <- total_df %>%
  mutate(names = as.character(names))

total_df <- total_df %>%
  mutate(team = as.character(team))

# Covert variables to correct amount
total_df$names <- factor(total_df$names)
total_df$team <- factor(total_df$team)
total_df$grant <- factor(total_df$grant)

# Convert "submitted_amount" column to numeric
total_df$submitted_amount <- as.numeric(total_df$submitted_amount)

total_df$amount_allocated <- as.numeric(total_df$amount_allocated)

# Create a vector of date ranges (adjust the ranges as needed)
#date_ranges <- c("4/25/2023 - 3/31/2024", "4/25/2023 - 3/31/2024", "4/25/2023 - 3/31/2024","4/25/2023 - 3/31/2024", "4/25/2023 - 3/31/2024")  

# Replace with your actual date ranges in the order of the grants listed above
# grant_names <- c("Family Spirit Strengths-R01", "CIRCLE Research Core", "Native-RISE","Comic Relief", "PAWS")

# Split date ranges into start and end date columns
#date_data <- data.frame(DateRange = date_ranges)
#date_data
# Split the DateRange column into StartDate and EndDate columns
#date_data <- separate(date_data, DateRange, into = c("StartDate", "EndDate"), sep = " - ")
#date_data

# Convert the StartDate and EndDate columns to date objects
#date_data$StartDate <- as.Date(date_data$StartDate, format = "%m/%d/%Y")
#date_data$EndDate <- as.Date(date_data$EndDate, format = "%m/%d/%Y")

date_data
#class(date_data$start_date)

# Initialize an empty list to store sequences of months
months_list <- list()

# Initialize an empty list to store corresponding grant names and dates
grant_info_list <- list()

# Make sure the date_data is Date TYpe
date_data$start_date <- as.Date(date_data$start_date)
date_data$end_date <- as.Date(date_data$end_date)

# # Loop through each grant's date range
for (i in 1:nrow(date_data)) {
  start_date <- date_data$start_date[i]
  end_date <- date_data$end_date[i]
  
  months <- seq(from = start_date, to = end_date, by = "1 month")
  
  months_list[[i]] <- months
  
  grant_info_list[[i]] <- data.frame(
    GrantName = rep(grant_names[i], length(months)),
    StartDate = rep(start_date, length(months)),
    EndDate = rep(end_date, length(months)),
    Month = months
  )
}

print(months_list)
print(grant_info_list)

# Combine all sequences of months into a single data frame
months_df <- data.frame(Month = unlist(months_list))

# Combine all grant-related information into a single data frame
grant_info_df <- do.call(rbind, grant_info_list)

# Add grant-related columns to months_df
months_df$grant <- grant_info_df$GrantName
months_df$StartDate <- grant_info_df$StartDate
months_df$EndDate <- grant_info_df$EndDate
months_df

months_df$Month <- as.Date(months_df$Month, origin = "1970-01-01")

print(months_df)

# Pivot the data frame to have months as separate columns

##### error here
months_df_wide <- months_df %>%
  group_by(grant) %>%
  mutate(Month_ID = row_number()) %>%
  pivot_wider(names_from = Month_ID, values_from = Month, names_prefix = "Month_")
View(months_df_wide)
# Replace all values in month columns with NA
months_df_wide[, grepl("^Month_", names(months_df_wide))] <- NA

# Now, months_df_wide contains grant names, start and end dates, and separate columns for months
# str(months_df_wide)

# Assuming that "grant" is the common column in both dataframes
combined_df <- total_df %>%
  left_join(months_df_wide, by = "grant")
View(combined_df)

# Add Additional month columns
#combined_df <- combined_df %>%
#  mutate(
#    Month_13 = NA,
#    Month_14 = NA,
#    Month_15 = NA,
#    Month_16 = NA,
#    Month_17 = NA,
#    Month_18 = NA,
#    Month_19 = NA,
#    Month_20 = NA
#  )

# Create a vector of month column names
#month_columns <- c(
#  "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
#  "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12",
#  "Month_13", "Month_14", "Month_15", "Month_16", "Month_17", "Month_18", "Month_19", "Month_20"
#)
month_columns <- grep("^Month_", names(combined_df), value = TRUE)


# Fill percent_allocated across all month columns
combined_df <- combined_df %>%
  mutate(across(all_of(month_columns), ~percent_allocated))
View(combined_df)

#Now to group by name####
# str(combined_df)

long_df <- combined_df %>%
  gather(month, percent_allocated, all_of(month_columns), na.rm = TRUE)
View(long_df)

earliest_date <- min(date_data$start_date)
latest_date <- max(date_data$end_date)

# Define month_mapping
month_mapping <- setNames(seq.Date(earliest_date, latest_date, by = "1 month"), 
                          paste0("Month_", 1:length(months)))

month_mapping

# Define a mapping of original values to new values for all 14 months 
#month_mapping <- c(   "Month_1" = "2023-05-01",   
#                      "Month_2" = "2023-06-01",   
#                      "Month_3" = "2023-07-01",   
#                      "Month_4" = "2023-08-01",   
#                      "Month_5" = "2023-09-01",   
#                      "Month_6" = "2023-10-01",   
#                      "Month_7" = "2023-11-01",   
#                      "Month_8" = "2023-12-01",   
#                      "Month_9" = "2024-01-01",   
#                      "Month_10" = "2024-02-01",   
#                      "Month_11" = "2024-03-01",   
#                      "Month_12" = "2024-04-01",   
#                      "Month_13" = "2024-05-01",   
#                      "Month_14" = "2024-06-01", 
#                      "Month_15" = "2024-07-01", 
#                      "Month_16" = "2024-08-01",
#                      "Month_17" = "2024-09-01",
#                      "Month_18" = "2024-10-01",
#                      "Month_19" = "2024-11-01",
#                      "Month_20" = "2024-12-01" ) 

# Use mutate to rename the values in the Month column 
long_df <- long_df %>% 
  mutate(month = as.Date(month_mapping[month]))
long_df
# Ensure Start and EndDate is a Date type
long_df$EndDate <- as.Date(long_df$EndDate)
long_df$StartDate <- as.Date(long_df$StartDate)

# Replace amounts with 0 if enddate of the grant has been reached
long_df <- long_df %>%
  mutate(
    percent_allocated = ifelse(month > EndDate, 0, percent_allocated),
    amount_allocated = ifelse(month > EndDate, 0, amount_allocated)
  )

# Replace amounts with 0 if startdate of the grant has not been reached
long_df <- long_df %>%
  mutate(
    percent_allocated = ifelse(month < StartDate, 0, percent_allocated),
    amount_allocated = ifelse(month < StartDate, 0, amount_allocated)
  )
View(long_df)

# Password setting
credentials <- data.frame(
  user = c("admi"),
  password = c("1207"),
  stringsAsFactors = FALSE
)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000); // after 120s it will log out auto
window.onmousemove = resetTimer; 
window.onmousedown = resetTimer; 
window.onclick = resetTimer;    
window.onscroll = resetTimer;   
window.onkeypress = resetTimer;  

function logout() {
window.close();  
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000); 
}
}
idleTimer();"


# Define UI

ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                   # If the logo does not show please make sure you put the logo in a 'www' folder
                   titlePanel(title = span(img(src = "CIH-Logo-Horizontal-Colour.png", height = 100))),
                   theme = shinytheme("united"),
                   
                   
                   #Here can add the title for dashborad
                   #titlePanel("Budget and Expense Tracking Dashbord"),
                   
                   navbarPage(title = "Menu",
                              # Password
                              
                              
                              # Tab1 
                              tabPanel("Salary Allocations",
                                       tabsetPanel(
                                         type = "tabs",
                                         tabPanel("Overview", plotlyOutput("tab1_plot1")),
                                         
                                         tabPanel("Specific Team",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      div(
                                                        selectInput(inputId = "tab1_select_team", 
                                                                    label = "Select a Team:", 
                                                                    choices = unique(long_df$team),
                                                                    selected = "PI"), width = 2
                                                      ),
                                                      div(
                                                        selectInput(inputId = "tab1_select_member", 
                                                                    label = "Selec3t a Member:", 
                                                                    choices = NULL), width = 2
                                                      ),
                                                      div(
                                                        dateRangeInput(inputId = "tab1_date_range",
                                                                       label = "Select Date Range:",
                                                                       start = min(long_df$month), 
                                                                       end = max(long_df$month),
                                                                       min = min(long_df$month),
                                                                       max = max(long_df$month)), width = 2
                                                      ),
                                                      # Choose apply
                                                      div(
                                                        actionButton(inputId = "tab1_apply_changes", 
                                                                     label = "Apply Changes",
                                                                     class = "btn btn-primary"), width = 2
                                                      )
                                                    ),
                                                    mainPanel(
                                                      plotlyOutput("tab1_plot2")
                                                    )
                                                  )
                                         )
                                         
                                         
                                       )
                              ),
                              
                              #Tab2 
                              tabPanel("Expense Summary",
                                       tabsetPanel(
                                         type = "tabs",
                                         tabPanel("Amount Unspent by Category", plotlyOutput("tab2_plot1")),
                                         tabPanel("Total Amount Allocated Across Grants", plotlyOutput("tab2_plot2")),
                                         tabPanel("Data",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      selectInput(inputId = "tab2_select_grant", 
                                                                  label = "Select a Grant:", 
                                                                  choices = c("All" = "All", unique(totalexpenses_df$grant))), width = 2
                                                    ),
                                                    mainPanel(
                                                      DT::dataTableOutput("tab2_table")
                                                    )
                                                  )
                                         )
                                       )
                              )
                              # Tab about About
                              # tabPanel("About", includeMarkdown("about.Rmd")),
                   )
                 )
                 )


# Define server logic
server <- function(input, output, session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # Tab1
  # Overview page
  output$tab1_plot1 <- renderPlotly({
    t1_p1<-ggplot(long_df, aes(x = month, y = percent_allocated, fill = grant, label = percent_allocated)) +
      geom_bar(stat = "identity", position = "stack") +
      #  geom_text(position = position_stack(vjust = 0.5), size=2) +
      scale_x_date(date_labels = "%b %y") +
      facet_wrap(~names) +
      theme_bw() +
      labs(title = "Percent Allocation to Each Grant Over Time", 
           x = "Month", 
           y = "Percent Allocation") + theme(plot.title = element_text(hjust = 0.5, vjust = -5))
    
    
    ggplotly(t1_p1)
  })
  # Specific employees page
  # The link for team and name
  observeEvent(input$tab1_select_team, {
    members <- as.character(unique(long_df$names[long_df$team == input$tab1_select_team]))
    updateSelectInput(session, "tab1_select_member", 
                      label = "Select a Member:", 
                      choices = c("All members", members))
  })
  # Active data
  tab1_filtered_data <- reactive({
    # Canceling Execution with req()
    req(input$tab1_select_team, input$tab1_select_member)
    if (input$tab1_select_member == "All members") {
      long_df %>%
        filter(team == input$tab1_select_team, 
               month >= input$tab1_date_range[1],
               month <= input$tab1_date_range[2])
    } else {
      long_df %>%
        filter(team == input$tab1_select_team, 
               names == input$tab1_select_member,
               month >= input$tab1_date_range[1],
               month <= input$tab1_date_range[2])
    }
  })
  # click apply and run the plot
  observeEvent(input$tab1_apply_changes, {
    output$tab1_plot2 <- renderPlotly({
      # Canceling Execution with req()
      req(tab1_filtered_data())
      if (input$tab1_select_member == "All members") {
        t1_p2<-ggplot(tab1_filtered_data(), aes(x = month, y = percent_allocated, fill = grant, label = percent_allocated)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_x_date(date_labels = "%b %y") +
          facet_wrap(~names) +
          theme_bw() +
          labs(title = "Percent Allocation to Each Grant Over Time", 
               x = "Month", 
               y = "Percent Allocation") + theme(plot.title = element_text(hjust = 0.5, vjust = -5))
        ggplotly(t1_p2)
      } else {
        t1_p2 <- ggplot(tab1_filtered_data(), aes(x = month, y = percent_allocated, fill = grant)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
          theme_minimal() +
          labs(title = paste("Percent Allocation to", input$tab1_select_member, "Over Time"), 
               x = "Month", 
               y = "Percent Allocation") + theme(plot.title = element_text(hjust = 0.5, vjust = -5))
        ggplotly(t1_p2)
      }
    })
  }, ignoreInit = TRUE)
  
  #Tab2
  # By cator
  output$tab2_plot1 <- renderPlotly({
    t2_p1<-ggplot(totalexpenses_df, aes(x = category, y = variance, fill = grant, label = variance)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = dollar_format_custom()) + # Use custom dollar formatting
      theme_bw() +
      labs(title = "Amount Unspent by Category", 
           x = "Category", 
           y = "Amount (USD)") + theme(plot.title = element_text(hjust = 0.5, vjust = -5))
    ggplotly(t2_p1)
  })
  
  # total
  output$tab2_plot2 <- renderPlotly({
    t2_p2 <- ggplot(totalexpenses_df, aes(x = category, y = budget_amount, fill = grant)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = dollar_format_custom()) +
      theme_bw() +
      labs(title = "Total Amount Allocated Across Grants", 
           x = "Category", 
           y = "Budget Amount (USD)") + theme(plot.title = element_text(hjust = 0.5, vjust = -5))
    ggplotly(t2_p2)
  })
  
  # Data Table output
  output$tab2_table <- renderDataTable({
    if (input$tab2_select_grant != "All") {
      tab2_table_filtered_data <- totalexpenses_df|>
        filter(grant == input$tab2_select_grant)
    } else {
      tab2_table_filtered_data <- totalexpenses_df
    }
    datatable(tab2_table_filtered_data, options = list(pageLength = 25, autoWidth = TRUE), class='table-active')})

  

  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
