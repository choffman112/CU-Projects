# libraries
library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(janitor)
library(tmap)

# Loading VSRR (CDC) dataset
vsrr_data <- read_csv("/Users/colehoffman/Desktop/BIOS 7719 - Information Visualization/Final Project/VSRR_Data.csv")

#############################################
# DATA EXPLORATION - List of Variables
# State
# Year
# Month
# Period
# Indicator
# Data Value
# Percent Complete
# Percent Pending Investigation
# State Name
# Footnote
# Footnote Symbol
# Predictive Value
#############################################

# what you want to do now is emulate what you did for the Colorado dataset
# before you filtered by state
vsrr_data <- vsrr_data %>% 
  dplyr::mutate(Month = factor(Month, levels = month.name, ordered = TRUE)) %>% # factor month to have some intrinsic order
  arrange(Year, Month) # arrange observations chronologically

vsrr_data <- clean_names(vsrr_data) # clean variable names

# use rename() to rename 'data_value' variable to something more intuitive
vsrr_data <- vsrr_data %>%
  rename(observed_value = data_value)

# convert dataset to long format
vsrr_data_long <- vsrr_data %>%
  pivot_longer(cols = c(observed_value, predicted_value), 
               names_to = "type", 
               values_to = "count") %>%
  mutate(type = recode(type, "observed_value" = "observed", "predicted_value" = "predicted")) 

# additional cleaning of dataset: only include variables of interest
vsrr_data_long <- vsrr_data_long %>%
  select(state, year, month, indicator, type, count)


# create a US/national level dataset which includes the 6 drug classes and the 3 death reporting metrics
vsrr_usa_data <- vsrr_data_long %>%
  dplyr::filter(!indicator %in% c("Natural & semi-synthetic opioids, incl. methadone (T40.2, T40.3)", "Natural, semi-synthetic, & synthetic opioids, incl. methadone (T40.2-T40.4)")) %>% # only filter out these 2 for now, keep the 3 summary death metrics
  mutate(indicator = case_when(indicator %in% c("Cocaine (T40.5)", "Psychostimulants with abuse potential (T43.6)") ~ "Stimulants (T40.5, T43.6)",
                               TRUE ~ indicator)) %>%
  dplyr::group_by(state, year, indicator, type) %>% # don't forget to group by 'state', too
  summarise(death_count = if (all(is.na(count))) NA_integer_ else sum(count, na.rm = TRUE), .groups = "drop") # keep your 'NA' values!

# filter out extraneous variables and rename dataset to avoid confusion/overwriting
vsrr_data_drugtype <- vsrr_data_long %>%
  filter(!indicator %in% c("Number of Deaths", "Number of Drug Overdose Deaths", "Percent with drugs specified", "Natural & semi-synthetic opioids, incl. methadone (T40.2, T40.3)", "Natural, semi-synthetic, & synthetic opioids, incl. methadone (T40.2-T40.4)")) %>%
  mutate(indicator = case_when(indicator %in% c("Cocaine (T40.5)", "Psychostimulants with abuse potential (T43.6)") ~ "Stimulants (T40.5, T43.6)",
                               TRUE ~ indicator)) %>%
  dplyr::group_by(state, year, indicator, type) %>%# don't forget to group by 'state', too
  summarise(death_count = sum(count, na.rm = TRUE), .groups = "drop")

# Summarize total deaths by state (for US map)
state_summary <- vsrr_data_long %>%
  dplyr::filter(type == "observed") %>%
  group_by(state, year) %>%
  summarise(
    total_deaths = sum(count[indicator == "Number of Deaths"], na.rm = TRUE),
    drug_overdose_deaths = sum(count[indicator == "Number of Drug Overdose Deaths"], na.rm = TRUE),
    percent_drug_specified = round((sum(count[indicator == "Percent with drugs specified"], na.rm = TRUE)/12),2)
  )


################################################################################

# first build the colors for the drug classes/types
custom_colors <- c(
  "Opioids (T40.0-T40.4,T40.6)" = "gray50",
  "Heroin (T40.1)" = "#1b9e77",
  "Synthetic opioids, excl. methadone (T40.4)" = "#d95f02",
  "Stimulants (T40.5, T43.6)" = "#7570b3",
  "Methadone (T40.3)" = "#e7298a",
  "Natural & semi-synthetic opioids (T40.2)" = "#66a61e"
)

# build a lookup table that spells out the states
state_lookup <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia",
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa",
  KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri",
  MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey",
  NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio",
  OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
  SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont",
  VA = "Virginia", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming",
  DC = "District of Columbia"
)

# create helper function for the states
get_state_name <- function(abbrev) {
  state_lookup[[abbrev]]
}

################################################################################

text_summary <- c("This provides background information on my CDC dataset.")

################################################################################

# Define UI
ui <- fluidPage(
  titlePanel("Interactive U.S. Drug-Related Deaths Visualization (2015–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_state",
        label = "Choose a State:",
        choices = sort(unique(vsrr_usa_data$state)),
        selected = "CO"
      ),
      selectInput(
        inputId = "selected_drug",
        label = "Choose a drug type:",
        choices = c("All", unique(vsrr_data_drugtype$indicator)),
        selected = "All"
      ),
      sliderInput(
        inputId = "year_range",
        label = "Select year range:",
        min = min(vsrr_data_drugtype$year, na.rm = TRUE),
        max = max(vsrr_data_drugtype$year, na.rm = TRUE),
        value = c(min(vsrr_data_drugtype$year, na.rm = TRUE),
                  max(vsrr_data_drugtype$year, na.rm = TRUE)),
        sep = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of Dataset",
                 h4("Summary"), # paragraph
                 p("The primary data for this project are sourced from the Center for Disease Control's (CDC) 'Provisional Drug Overdose Death Counts' (https://www.cdc.gov/nchs/nvss/vsrr/drug-overdose-data.htm). This dataset is compiled by the National Center for Health Statistics (NCHS) from death certificate data collected by state and local vital statistics offices within the National Vital Statistics Syste (NVSS). It includes provisional drug overdose death counts across all 50 U.S. states, totaling nearly 70,000 records (rows) and 12 attributes (columns)."), # paragraph
                 p("This Shiny App aims to investigate opioid and drug overdose mortality death counts across the United States from 2015-2024. The intended audience includea medical experts (physicians, pharmacists, nurses), public health experts (epidemiologists and biostatisticians), policymakers, and the general public. The hope is that these visualizations raise awareness about the ongoing opioid epidemic and povide healthcare professionals with deeper insights that could influence medical practices and health policies.")
                 ),
        tabPanel("Dataset Information",
                 p("Provisional counts are often incomplete, and the reported causes of death may still be under investigation. Therefore, they should not be considered comparable with the final data; in fact, these provisional counts underestimate the true final death counts. Methods were developed to address this discrepancy by adjusting observed provisional counts for reporting delays by generating a set of predicted provisional death counts."),
                 p("Deaths are classified by the reporting jurisdiction in which they occurred, and the provisional death count includes foreign residents. The final death count includes only U.S. citizens. Provisional drug overdose death counts are based on death certificates received and processed by the CDC’s NCHS as of a specified cutoff date, which is generally the first Sunday of every month. The time it takes to declare a true cause of death varies, and this is known as the lag time (i.e., the elapsed time from death to when the data are available for analysis). Lag times are typically longer for drug overdose deaths at about 6 months, but this has reduced to 4 months in 2022 due to recent improvements in the data collection process. Provisional death counts are often reported for 12-month ending periods, defined as the number of deaths that occurred in the 12-month period ending in the month indicated. These reports include all seasons in the year and are insensitive to seasonal variations."),
                 p("Mortality statistics are reported in accordance with the World Health Organization (WHO), which involves the use of the International Statistical Classification of Diseases and Related Health Problems (ICD). ICD provides codes that classify causes of death, and these data use the cause-of-death codes from the Tenth Revision of ICD (ICD-10). Please resort to the linked CDC dataset webpage to reference which codes are used to classify cause of death. Of note, among those causes of death related to drug overdose, the percentage with at least one drug or drug class specified is defined by at least one ICD-10 multiple cause-of-death code. Drug overdose deaths involving multiple drugs would be logged for each drug class; for example, a drug overdose death involving both heroin and fentanyl would be included both the number of heroin deaths and the number of synthetic opioid deaths."),
                 p("Provisional counts reported from each jurisdiction are based on three primary data quality metrics: (1) the number of death records with cause of death “pending investigation”, (2) the overall completeness of the data, and (3) the percentage of drug overdose death records that specify the specific drug or drug class. As previously mentioned, provisional death counts often underestimate the true final death count due to the lag time of cause of death reporting. For jurisdictions that report fewer than 1% of records as “pending investigation,” the provisional death count in 2015 was about 5% lower than the final death count. If the number of “pending investigation” reports rises above 1%, the provisional death counts may be underestimating the true death count by as much as 30%. For jurisdictions that do not meet these data quality metrics, predicted values are shown for the data that meet percent completeness and drug specificity thresholds with reported values only shown for months where all three data quality measures were met. Percent completeness is calculated by dividing the number of death records for each jurisdiction for each 12-month period by control counts and multiplying by 100. Jurisdictions with data completeness consistently above 90% are typically included in data analyses. Lastly, reportable jurisdictions consistently have 90% or more of drug overdose death certificates mentioning at least one specific drug for all the 12-month ending periods."),
                 p("Lastly, methods have been used to adjust for the difference between provisional death counts and the final death count data. These involve setting multiplication factors that are based on the degree of underreporting in the provisional data. If provisional death counts are historically 90% complete relative to the final data, then the multiplication factor is 1.1. The provisional death counts are multiplied by this factor to determine the predicted provisional death counts that adjust for any lag times or reporting delays."),
                 ),
        tabPanel("Plots",
                 plotlyOutput("us_map", height = 500), # dimensions of US map
                 plotlyOutput("line_plot", height = 600), # dimensions of line plot
                 br(),
                 p("Note: Some drug classes may have missing ('NA') death counts for certain years."),
                 plotlyOutput("stacked_barplot", height = 600), # dimensions of stacked bar plot
                 p("Note: Some drug classes may have missing ('NA') death counts for certain years.")
                 ),
      )
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # include dataset summary informtion
  output$summary <- renderText({
    paste(text_summary, collapse = "\n")
  })
  
  # Create a reactive expression to track the selected state
  selected_state_reactive <- reactive({
    input$selected_state
  })
  
  # U.S. Map with interactive state selection
  output$us_map <- renderPlotly({
    selected_state <- selected_state_reactive()  # Use the reactive value here
    year_range <- input$year_range # use the same year range for US map as the line and bar plots
    #selected_drug <- input$selected_drug
    
    # define max number of overdose deaths in a given year, within the specified year range
    max_od_deaths_per_year <- state_summary %>%
      filter(year >= year_range[1], year <= year_range[2]) %>%
      group_by(state, year) %>%
      summarise(deaths = sum(drug_overdose_deaths, na.rm = TRUE), .groups = "drop") %>%
      summarise(max_deaths = max(deaths, na.rm = TRUE)) %>%
      pull(max_deaths)
    
    filtered_data <- state_summary %>%
      filter(year >= year_range[1], year <= year_range[2]) %>%
      group_by(state) %>%
      summarise(
        #total_deaths = sum(total_deaths, na.rm = TRUE),
        drug_overdose_deaths = sum(drug_overdose_deaths, na.rm = TRUE),
        percent_drug_specified = round(mean(percent_drug_specified, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      ungroup()
    
    # dyanmic color scale based on current filtered overdose death counts
    #z_values <- filtered_data$drug_overdose_deaths
    
    # start plotting 
    p <- plot_geo(filtered_data, locationmode = 'USA-states') %>%
      add_trace(
        locations = ~state,
        z = ~drug_overdose_deaths,
        zmin = 0,
        zmax = max_od_deaths_per_year,
        text = ~paste(
          state, "<br>",
          #"Total Deaths:", total_deaths, "<br>",
          "Drug Overdose Deaths:", drug_overdose_deaths, "<br>",
          "Average % Drug Specified:", percent_drug_specified, "%"
        ),
        hoverinfo = 'text',
        colorscale = 'Reds',
        colorbar = list(title = "Overdose Deaths"),
        type = 'choropleth',
        key = ~state
      )
    
    # If a state is selected, add a highlight
    if (!is.null(selected_state)) {
      highlight_data <- filtered_data %>% filter(state == selected_state) # revert to 'state_summary' if doesnt work! (Wed 4/30 at 4:45 pm)
      
      p <- p %>% add_trace(
        locations = highlight_data$state,
        locationmode = 'USA-states',
        type = 'scattergeo',
        mode = 'markers',
        marker = list(size = 10, color = 'black', symbol = 'square'),
        hoverinfo = 'none'
      )
    }
    
    # add annotation with the note 
    p <- p %>% layout(
      geo = list(scope = 'usa', projection = list(type = 'albers usa')),
      margin = list(l = 40, r = 40, t = 40, b = 40))
    
      p %>% event_register("plotly_click")
  })
  
  # Listen for the click event and update the selected state
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    
    if (!is.null(click_data)) {
      clicked_state <- click_data$key
      
      # Make sure clicked_state is valid
      if (!is.null(clicked_state) && clicked_state %in% vsrr_usa_data$state) {
        updateSelectInput(session, "selected_state", selected = clicked_state)
      }
    }
  })
  
  # Filtering step for line and stacked bar plots
  filtered_data <- reactive({
    data <- vsrr_data_drugtype %>%
      filter(state == input$selected_state) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$selected_drug != "All") {
      data <- data %>%
        filter(indicator == input$selected_drug)
    }
    
    data
  })
  
  # now build the line plot
  output$line_plot <- renderPlotly({
    data <- filtered_data()  # Get filtered data for line plot
    
    # Create the plot and assign it to 'p'
    p <- ggplot(data, aes(x = factor(year), y = death_count, color = indicator, linetype = type, 
                          group = interaction(indicator, type),
                          text = paste("Year:", year,
                                       "<br>Drug Type:", indicator,
                                       "<br>Count Type:", type,
                                       "<br>Deaths:", death_count))) +
      geom_line(size = 1) +
      labs(
        title = paste("Observed and Predicted Drug-Related Deaths\nOver Time in", get_state_name(input$selected_state)),
        x = "Year", y = "Death Count", color = "Drug Type", linetype = "Count Type",
        caption = "Note: Some drug classes may have missing ('NA') death counts for certain years."
      ) +
      scale_color_manual(values = custom_colors) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        plot.caption = element_text(size = 8, hjust = 0)
      ) +
      guides(
        color = guide_legend(ncol = 2),
        linetype = guide_legend(ncol = 1)
      )
    
    # Convert to plotly with tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.3))
  })
  
  # stacked bar plot
  output$stacked_barplot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data %>% dplyr::filter(type == "observed"), aes(x = factor(year), y = death_count, fill = indicator,
                          text = paste("Year:", year,
                                       "<br>Drug Class:", indicator,
                                       "<br>Deaths:", death_count))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste("Observed Drug-Related Deaths by Class in", get_state_name(input$selected_state)),
        x = "Year", y = "Death Count", fill = "Drug Class",
        caption = "Note: Some drug classes may have missing ('NA') death counts for certain years."
      ) +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        plot.caption = element_text(size = 8, hjust = 0)
      ) +
      guides(
        fill = guide_legend(ncol = 2)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.3))
  })
  
}


# Run the app
shinyApp(ui, server)


################################################################################




