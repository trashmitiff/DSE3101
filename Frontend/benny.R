library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(stringr)
library(reticulate)
library(scales)
library(tidyverse)
library(shinyjs)

data_path <- "data"


##############################
#   Current Transport Maps   #
##############################
mrt <- read_csv(file.path(data_path, "mrt.csv"))
bus_routes <- read_csv(file.path(data_path, "bus_routes_df.csv"))
bus_stops <- read_csv(file.path(data_path, "bus_stops_df.csv"))

mrt_line_colors <- c(
  "NS" = "red",
  "EW" = "green",
  "CC" = "orange",
  "CE" = "orange",
  "DT" = "blue",
  "NE" = "purple",
  "TE" = "sienna",
  "CG" = "green",
  "BP" = "grey",
  "PE" = "grey",
  "PT" = "grey",
  "PW" = "grey",
  "SE" = "grey",
  "ST" = "grey",
  "SW" = "grey"
)
mrt_line_groups <- list(
  "East-West Line" = c("EW", "CG"),
  "North-South Line" = c("NS"),
  "Circle Line" = c("CC", "CE"),
  "Downtown Line" = c("DT"),
  "North East Line" = c("NE"),
  "Thomson-East Coast Line" = c("TE"),
  "Bukit Panjang LRT" = c("BP"),
  "Punggol LRT" = c("PE", "PT", "PW"),
  "Sengkang LRT" = c("SE", "ST", "SW")
)
mrt <- mrt %>% mutate(station_line = substr(station_code, 1, 2), 
                      station_sequence = str_sub(station_code, 3), 
                      station_sequence = if_else(station_sequence == "C", "0", station_sequence),
                      station_sequence = as.numeric(station_sequence), 
                      color = mrt_line_colors[station_line]) 
station_lines_df <- mrt %>%
  group_by(station_name) %>%
  summarise(all_lines = paste(unique(station_line), collapse = ", ")) %>%
  ungroup()

mrt <- mrt %>% left_join(station_lines_df, by = "station_name")

bus_stops <- bus_stops %>% mutate(BusStopCode = as.character(BusStopCode))

bus_routes <- bus_routes %>% select(-c(7:13)) %>% 
  mutate(BusStopCode = as.character(BusStopCode)) %>%
  left_join(bus_stops %>% select(BusStopCode, Latitude, Longitude, Description),
            by = "BusStopCode") 

bus_stop_info <- bus_stops %>%
  left_join(bus_routes %>% select(BusStopCode, ServiceNo), by = "BusStopCode") %>%
  group_by(BusStopCode, Description, Longitude, Latitude) %>%
  summarise(buses = paste(unique(ServiceNo), collapse = ", ")) %>%
  ungroup() 

####################################
#    Commuter Hub Density Data     #
####################################

future_com_hub_score <- read_csv(file.path(data_path, "future_com_hub_score_scaled.csv"))
future_combined <- read_csv(file.path(data_path, "final_combined_com_v2.csv"))
final_com_pca <- read_csv(file.path(data_path, "final_combined_com_pca.csv"))
subzone_data <- read_csv(file.path(data_path, "subzone_final.csv"))
subzone_data <- st_as_sf(subzone_data, wkt = "geog_boundary", crs = 4326)

commuter_subzone_data <- subzone_data %>%
  left_join(final_com_pca, by = "subzone_name") %>%
  mutate(relative_commuter_score = round(ifelse(is.na(relative_commuter_score), 0, relative_commuter_score), 3)) %>%
  rename(planning_area_name = planning_area_name.x) %>%
  select(subzone_name, commuter_score, relative_commuter_score, geog_boundary, planning_area_name)

# invalid geometries
commuter_subzone_data <- commuter_subzone_data %>%
  mutate(geog_boundary = st_make_valid(geog_boundary)) %>%
  filter(st_is_valid(geog_boundary))

# average commuter score per planning area (chart)
commuter_planning_area <- commuter_subzone_data %>%
  group_by(planning_area_name) %>%
  summarise(avg_commuter_score = round(mean(relative_commuter_score, na.rm = TRUE), 3)) %>%
  ungroup() %>%
  select(planning_area_name, avg_commuter_score)

# creating a new dataframe to get rid of geog_boundary (table)
commuter_data4table <- commuter_subzone_data %>%
  select(subzone_name, planning_area_name, relative_commuter_score) %>%
  rename(
    "Subzone" = subzone_name,
    "Planning Area" = planning_area_name,
    "Commuter Hub Score" = relative_commuter_score
  ) %>%
  group_by(Subzone, `Planning Area`) %>%
  summarise(`Commuter Hub Score` = round(mean(`Commuter Hub Score`, na.rm = TRUE), 3), .groups = 'drop') %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  arrange(desc(`Commuter Hub Score`)) 


####################################
#   Transport Accessibility Data   #
####################################
final_acc_pca = read.csv(file.path(data_path, "final_combined_acc_pca.csv"))
hourly_data = read.csv(file.path(data_path, "hourly_data_pca.csv"))
planning_agg = read.csv(file.path(data_path, "planning_agg.csv"))
raw_acc_values = read.csv(file.path(data_path, "final_combined_acc.csv"))

accessibility_subzone_data <- subzone_data %>%
  left_join(final_acc_pca, by = "subzone_name") %>%
  mutate(acc_score_scaled = round(ifelse(is.na(acc_score_scaled), 0, acc_score_scaled), 3)) %>%
  mutate(acc_score = round(ifelse(is.na(acc_score), 0, acc_score), 3)) %>%
  mutate(bus_acc_score_scaled = round(ifelse(is.na(bus_acc_score_scaled), 0, 
                                             bus_acc_score_scaled), 3)) %>%
  mutate(bus_acc_score = round(ifelse(is.na(bus_acc_score), 0, bus_acc_score), 3)) %>%
  mutate(mrt_acc_score_scaled = round(ifelse(is.na(mrt_acc_score_scaled), 0, 
                                             mrt_acc_score_scaled), 3)) %>%
  mutate(mrt_acc_score = round(ifelse(is.na(mrt_acc_score), 0, mrt_acc_score), 3)) %>%
  rename(planning_area_name = planning_area_name.x) %>%
  select(subzone_name, acc_score, acc_score_scaled, bus_acc_score, 
         bus_acc_score_scaled, mrt_acc_score, mrt_acc_score_scaled, 
         geog_boundary.x, planning_area_name)

# invalid geometries
accessibility_subzone_data <- accessibility_subzone_data %>%
  mutate(geog_boundary.x = st_make_valid(geog_boundary.x)) %>%
  filter(st_is_valid(geog_boundary.x))

# average accessibility score for each transport mode per planning area (chart)
accessibility_planning_area <- accessibility_subzone_data %>%
  group_by(planning_area_name) %>%
  summarise(
    avg_acc_score = round(mean(acc_score_scaled, na.rm = TRUE), 3),
    avg_bus_acc_score = round(mean(bus_acc_score_scaled, na.rm = TRUE), 3),
    avg_mrt_acc_score = round(mean(mrt_acc_score_scaled, na.rm = TRUE), 3),) %>%
  ungroup() %>%
  select(planning_area_name, avg_acc_score, avg_bus_acc_score, avg_mrt_acc_score)

# creating a new dataframe to get rid of geog_boundary (table)
accessibility_data4table <- accessibility_subzone_data %>%
  select(subzone_name, planning_area_name, acc_score_scaled, bus_acc_score_scaled, mrt_acc_score_scaled) %>%
  rename(
    "Subzone" = subzone_name,
    "Planning Area" = planning_area_name,
    "Total Transport Accessibility Score" = acc_score_scaled,
    "Bus Accessibility Score" = bus_acc_score_scaled,
    "MRT Accessibility Score" = mrt_acc_score_scaled
  ) %>%
  group_by(Subzone, `Planning Area`) %>%
  summarise(`Total Transport Accessibility Score` = round(mean(`Total Transport Accessibility Score`, na.rm = TRUE), 3),
            `Bus Accessibility Score` = round(mean(`Bus Accessibility Score`, na.rm = TRUE), 3), 
            `MRT Accessibility Score` = round(mean(`MRT Accessibility Score`, na.rm = TRUE), 3), .groups = 'drop' 
  ) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  arrange(desc(`Total Transport Accessibility Score`)) 

####################################
#   Commuter Accessibility Ratio   #
####################################

# loading data 
future_com_hub_score_unscaled <- read_csv(file.path(data_path, "future_com_hub_score.csv"))
new_coefficient_dict <- read.csv(file.path(data_path, "new_coefficient_dict.csv"))
prenorm_data <- read.csv(file.path(data_path, "prenorm_data.csv"))

# commuter hub score over the next few years by planning area 
future_com_by_planning_area <- future_com_hub_score_unscaled %>%
  group_by(planning_area_name) %>%
  select(c("2025":"2035")) %>%
  summarise_all(mean, na.rm = TRUE) 

# find min score and add to all rows because we don't want negative scores 
min_com_score <- future_com_by_planning_area %>% 
  summarise(across(where(is.numeric), \(x) min(x, na.rm = TRUE))) %>%
  unlist() %>%
  min()

future_com_by_planning_area <- future_com_by_planning_area %>%
  mutate(across(where(is.numeric), ~ . - min_com_score))

# initial stations by planning areas, to use this to calculate when we change user inputs 
initial_stations_sum <- prenorm_data %>% 
  group_by(planning_area_name) %>%
  select(c(area, planning_area_name, total_bus_passengers, 
           total_buses_per_hour, num_bus_stops, num_bus_services, 
           num_stations_per_subzone, num_train_lines_per_subzone, num_rail_types_per_subzone,
           total_train_passengers)) %>%
  summarise_all(sum)

initial_stations_mean <- prenorm_data %>% 
  group_by(planning_area_name) %>%
  select(c(priv_to_public_ratio, mean_bus_distance, AM_Peak_Interval, AM_Offpeak_Interval,
           PM_Peak_Interval, PM_Offpeak_Interval, mean_duration_to_train_station, 
           mean_walking_dist_to_train_station, mean_fare_to_train_station)) %>%
  summarise_all(mean)

initial_stations <- initial_stations_sum %>%
  left_join(initial_stations_mean, by = "planning_area_name")

# common columns with PCA coeffcieint dictionary
common_cols <- intersect(names(initial_stations), names(new_coefficient_dict))

# normalise all columns function
get_normalised_df  <- function(df){
  new_df <- df %>% ungroup() %>%
    mutate(across(where(is.numeric), ~ (. - min(., na.rm = TRUE)) / 
                    (max(., na.rm = TRUE) - min(., na.rm = TRUE)))) %>%
    rowwise() %>%
    mutate(new_acc_score = sum(c_across(all_of(common_cols)) * new_coefficient_dict[1, common_cols])) %>%
    ungroup() %>%
    mutate(new_acc_score_scaled = rescale(new_acc_score, to = c(0, 1), na.rm = TRUE)) 
  return(new_df)
}

# normalise the initial 
initial_stations_norm <- get_normalised_df(initial_stations)

# planning area geometries
planning_area_geometry <- subzone_data %>% group_by(planning_area_name) %>% 
  mutate(geog_boundary = st_make_valid(geog_boundary)) %>%
  filter(st_is_valid(geog_boundary)) %>%
  summarize()

# initial accessibility data by planning area
initial_acc_planning <- planning_area_geometry %>%
  left_join(initial_stations_norm, by = "planning_area_name") %>%
  mutate(geog_boundary = st_make_valid(geog_boundary)) %>%
  filter(st_is_valid(geog_boundary))

# get vectors for ui
area_names <- as.vector(initial_stations$planning_area_name)
all_years <- future_com_by_planning_area %>% select(-planning_area_name) %>% colnames()

# function to get new accessibility data with changes made 
# previous df needs to be unscaled !!
get_new_df <- function(previous_df, area_name, 
                       n_bus_per_hour_diff, n_bus_stops_diff, n_bus_services_diff,
                       n_stations_diff, n_lines_diff) {
  
  # new data input calculation
  initial_data <- previous_df %>% filter(planning_area_name == area_name)
  new_bus_per_hour <- max(initial_data$total_buses_per_hour + n_bus_per_hour_diff, 0)
  new_bus_stops <- max(initial_data$num_bus_stops + n_bus_stops_diff, 0)
  new_bus_services <- max(initial_data$num_bus_services + n_bus_services_diff, 0)
  new_mrt <- max(initial_data$num_stations_per_subzone + n_stations_diff, 0)
  new_lines <- max(initial_data$num_train_lines_per_subzone + n_lines_diff, 0)
  
  # replace data in the dataframe for accessibility calculation
  new_df <- previous_df %>%
    mutate(total_buses_per_hour = 
             ifelse(planning_area_name == area_name, new_bus_per_hour, total_buses_per_hour),
           num_bus_stops = 
             ifelse(planning_area_name == area_name, new_bus_stops, num_bus_stops), 
           num_bus_services = 
             ifelse(planning_area_name == area_name, new_bus_services, num_bus_services), 
           num_stations_per_subzone = 
             ifelse(planning_area_name == area_name, new_mrt, num_stations_per_subzone), 
           num_train_lines_per_subzone = 
             ifelse(planning_area_name == area_name, new_lines, num_train_lines_per_subzone))
  
  return(new_df)
}
# new df is also unscaled!! so after this need to do get_normalised_df()


# duplicate all initial accessibility scores for all years
acc_scores_all_years <- initial_stations_norm %>% 
  select(planning_area_name, new_acc_score) %>%
  bind_cols(
    setNames(
      replicate(length(all_years), .$new_acc_score, simplify = FALSE),
      all_years
    )
  ) %>%
  select(-new_acc_score)

# function to get ratio of commuter-accessibility score
# input needs to be scaled/normalised dataframe, but use unscaled acc score
get_com_acc_ratio <- function(acc_scores_all_years, future_com_by_planning_area) {
  acc_scores_all_years <- acc_scores_all_years %>% arrange(planning_area_name)
  future_com_by_planning_area <- future_com_by_planning_area %>% arrange(planning_area_name)
  
  # make sure they have common years 
  common_years <- intersect(
    names(acc_scores_all_years)[-1],  # exclude planning area name
    names(future_com_by_planning_area)[-1]
  )
  
  result <- acc_scores_all_years %>%
    select(planning_area_name) %>%
    bind_cols(
      map_dfc(common_years, ~ {
        future_com_by_planning_area[[.x]] / acc_scores_all_years[[.x]]
      }) %>%
        set_names(paste0(common_years)))
  
  return(result)
}
# higher ratio means got alot of commuters but not accessible enough. should aim for lower ratio 

initial_ratio <- get_com_acc_ratio(acc_scores_all_years, future_com_by_planning_area)


# update accessibility scores for all years
update_acc_scores <- function(acc_scores_all_years, new_norm_df, year) {
  new_scores <- new_norm_df %>% 
    select(planning_area_name, new_acc_score)
  
  year <- as.character(year)
  all_years <- names(acc_scores_all_years)[-1]  # exclude planning area
  years_to_update <- all_years[all_years >= year]
  
  acc_scores_all_years[years_to_update] <- lapply(
    years_to_update, 
    function(y) new_scores$new_acc_score
  )
  
  return(acc_scores_all_years)
}

# function to combine and make all changes in changes_list
make_all_changes_df <- function(initial_stations, changes_list, acc_scores_all_years,
                                future_com_by_planning_area){
  current_df <- initial_stations
  
  for (change in changes_list) {
    # change params
    area_name <- change$area_name
    year <- change$year
    n_bus_per_hour_diff <- change$n_bus_per_hour_diff
    n_bus_stops_diff <- change$n_bus_stops_diff
    n_bus_services_diff <- change$n_bus_services_diff
    n_stations_diff <- change$n_stations_diff
    n_lines_diff <- change$n_lines_diff
    
    current_df <- get_new_df(current_df, area_name, n_bus_per_hour_diff, n_bus_stops_diff,
                             n_bus_services_diff, n_stations_diff, n_lines_diff)
    current_norm_df <- get_normalised_df(current_df)
    acc_scores_all_years <- update_acc_scores(acc_scores_all_years, current_norm_df, year)
  }
  
  final_ratio <- get_com_acc_ratio(acc_scores_all_years, future_com_by_planning_area)
  
  return (final_ratio)
}



##########
#   UI   #
##########
ui <- dashboardPage(
  dashboardHeader(title = "BAG DOWN BENNY"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home_page", icon = icon("home")),
      menuItem("Current Transport Visualisation", tabName = "current_transport", icon = icon("train")),
      menuItem("Commuter Hub Mode", tabName = "commuter_hub", icon = icon("map")),
      menuItem("Transport Accessibility Mode", tabName = "accessibility", icon = icon("bus")),
      menuItem("Simulation Mode", tabName = "simulation", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML(" 
        .skin-blue .main-header .navbar {
          background-color: #ffffff !important;
          border-bottom: 1px solid #e0e0e0 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        }
      
        .skin-blue .main-header .logo {
          background-color: #ffffff !important;
          color: #212121 !important;
          font-weight: bold;
          font-size: 18px;
          display: flex;
          align-items: center;
          justify-content: center;
        }
      
        .skin-blue .main-header .navbar .sidebar-toggle {
          color: #212121 !important;
          font-size: 18px;
        }

        .hero-section {
          position: relative;
          height: 350px;
          background-image: url('https://goodmigrations.com/wp-content/themes/goodMigrationsV5/static/images/locations/singapore/singapore-transportation.jpg');
          background-size: cover;
          background-position: center;
          margin-bottom: 30px;
        }
        .hero-overlay {
          position: absolute;
          top: 0; left: 0;
          width: 100%; height: 100%;
          background-color: rgba(0, 0, 0, 0.6);
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .hero-text {
          font-size: 40px;
          font-weight: bold;
          color: white;
          text-align: center;
          padding: 20px 40px;
        }
        .info-box {
          background-color: #f5f5f5;
          border-radius: 10px;
          padding: 20px;
          margin: 10px;
          min-height: 220px;
          cursor: pointer;
          transition: background-color 0.3s;
        }
        .info-box:hover {
          background-color: #e0e0e0;
          margin-left: 10px;
          margin-right: 10px;
        }
        .collapsible-content {
          background-color: #f9f9f9;
          padding: 15px;
          border-left: 4px solid #007BFF;
          border-right: 4px solid #007BFF;
          border-radius: 5px;
          display: none;
          margin-top: 10px;
          width: 100%;
          box-sizing: border-box; 
          clear: both;              
        }

        .section-header {
          font-size: 24px;
          font-weight: bold;
          margin-top: 20px;
        }
      ")),
    tabItems(
      # HOME TAB
      tabItem(tabName = "home_page",
              div(class = "hero-section",
                  div(class = "hero-overlay",
                      h1(class = "hero-text", "Smarter Transport Planning Starts Here")
                  )
              ),
              div(),
              fluidRow(
                column(width = 12,
                       p(style = "font-size:16px; font-weight:500;",
                         "This interactive public transport urban planning dashboard helps planners identify commuter hotspots, assess accessibility coverage, and simulate future transport interventions—all in one platform. Built to complement existing planning workflows, it streamlines analysis and optimises data-driven decision-making aligned with Singapore’s Land Transport Master Plan 2040."),
                       p(style = "font-size:12px;",
                         "*Each subzone belongs to a Planning Area as defined in Singapore’s official boundaries.")
                )
              ),
              br(),
              actionButton("toggle_data", "📚 Data Sources", onclick ="event.stopPropagation();"),
              div(id = "data", class = "collapsible-content", style = "margin-left: 30px;",
                  p("This dashboard integrates multiple trusted datasets to ensure robust analysis:"),
                  tableOutput("data_sources")
              ),
              br(),
              div(class = "section-header", "📍 How to Use Benny with Our 3 Modes"),
              fluidRow(
                column(4,
                       div(class = "info-box",
                           tags$h4("Step 1: Commuter Hub Mode"),
                           tags$p("Identify commuter hubs as focus points for public transport system strategies."),
                           actionButton("toggle_tech1", "📘 Technicals", onclick = "event.stopPropagation();"),
                           hidden(div(id = "tech1", class = "collapsible-content",
                                      p("Commuter Hub definition: When there is a large inflow and large outflow of people from other parts, signifying the place’s existence as a key transport node"),
                                      p("📊 We engineered two features: inflow score and outflow score. These scores take into account the volume of flows and their distances, providing the strength of the inflows and outflows."),
                                      p("🔧 We utilised PCA to derive a data-driven index scoring system for commuter hubs using inflow score, outflow score as well as total tap-ins for both bus and MRT.")
                           ))
                       )
                ),
                column(4,
                       div(class = "info-box",
                           tags$h4("Step 2: Transport Accessibility Mode"),
                           tags$p("Explore current coverage by MRT, bus, or both. You can also toggle by time of day to compare peak and off-peak accessibility, helping identify underserved regions.
"),
                           actionButton("toggle_tech2", "📘 Technicals", onclick = "event.stopPropagation();"),
                           hidden(div(id = "tech2", class = "collapsible-content",
                                      p("The Transport Accessibility Score integrates a wide range of bus and train-related features to provide a comprehensive overview of the accessibility of each subzone by different modes of transport. Accessibility is defined as the ease with which individuals can reach a destination."),
                                      p("📊️ Data collection and preprocessing were conducted to synthesise 35 features. We estimated and incorporated the ratio of travel duration between private and public transport. 14 bus-related features such as the total number of bus stops and mean distance coverage of buses were included. 20 train-related features such as the number of train stations and rail types were also added."),
                                      p("🔧 Variance-adjusted loading technique with PCA was leveraged to derive normalised weights based on each principal component’s degree of contribution to the explained variance in the dataset. Three different scores were developed on a subzone level based on overall accessibility, accessibility by bus and accessibility by train.")
                           ))
                       )
                ),
                column(4,
                       div(class = "info-box",
                           tags$h4("Step 3: Simulation Mode️"),
                           tags$p("Test real-world trade-offs by placing a new stop or adjusting bus frequency in your preferred planning areas. We can see how accessibility improves across time and region, giving you immediate feedback on possible interventions."),
                           actionButton("toggle_tech3", "📘 Technicals", onclick = "event.stopPropagation();"),
                           hidden(div(id = "tech3", class = "collapsible-content",
                                      p("The commuter-accessibility ratio for each area is calculated by dividing an estimate of the commuter hub score over the years by an estimate of the accessibility score."),
                                      p("  🟢 A lower ratio indicates that the area is well-equipped to handle its commuter traffic, with sufficient infrastructure."),
                                      p("  🔴 A higher ratio suggests that the area’s infrastructure is insufficient relative to the commuter traffic it receives."),
                                      p("📈 The commuter hub score is estimated using a population forecast generated via simple linear regression, assuming that population growth is correlated with the growth in the commuter hub score. This allows us to predict the commuter hub score for future years."),
                                      p("🔧 The estimated accessibility score is derived from raw transport data, incorporating any input changes. This data is normalized and weighted using a scaled-down PCA network. Any changes made to accessibility scores will be reflected for future years as well.")))
                       )
                ),
                br(),
                div(style = "text-align: center;",
                    tags$h2(style = "font-weight: bold; font-size: 28px; margin-bottom: 30px;", "🤝 Meet the Team")
                ),
                fluidRow(
                  lapply(list(
                    list(name = "Elsie Woo", role = "Front-End Developer"),
                    list(name = "Liew Ting Yu", role = "Front-End Developer"),
                    list(name = "Chong Yi Ting", role = "Front-End Developer"),
                    list(name = "Tay Lerui", role = "Back-End Developer"),
                    list(name = "Celine Tan", role = "Back-End Developer"),
                    list(name = "Tiffany Lam", role = "Back-End Developer")
                  ), function(member) {
                    column(
                      width = 4, align = "center",
                      div(style = "background-color: #ffffff; border-radius: 10px; padding: 15px 10px; margin-bottom: 20px; box-shadow: 0px 2px 6px rgba(0,0,0,0.05); width: 85%; max-width: 240px;",
                          tags$img(
                            src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcS5libZDE7O8AFEkCbroUtvvGaV36pkhfJzeA&s",
                            style = "width: 80px; height: 80px; border-radius: 50%; object-fit: cover; margin-bottom: 10px;"
                          ),
                          tags$h5(style = "margin-bottom: 4px; font-weight: 600;", member$name),
                          tags$p(style = "color: #555; font-size: 14px; margin: 0;", member$role)
                      )
                    )
                  })
                )
                
                )),
                
      # CURRENT TRANSPORT MAPS TAB 
      tabItem(tabName = "current_transport",
              fluidRow(
                actionButton("toggle_curr_notes", "📚 Guide on Current Transport Visualisation", style = "margin-right: 10px;", onclick = "event.stopPropagation();"),
                hidden(div(
                  id = "current_notes", class = "collapsible-content",
                  tags$ul(
                    tags$li(HTML("<b>Explore Singapore's Current Transportation Maps</b><br>
      ⏲️️ Toggle between the <b>MRT Map</b>, <b>Bus Stop Map</b> and <b>Bus Route Map</b> to visually explore public transport routes.")),
                    tags$li(HTML("<b>Select Desired MRT Lines</b><br>
      🔍 Use the <b>MRT Lines selector</b> to display specific lines you're interested in.")),
                    tags$li(HTML("<b>Choose a Bus Route</b><br>
      🚌 Pick a service in the <b>Bus Route Map</b> to see its route and stops.")),
                    tags$li(HTML("<b>Interactive Features</b><br>
      📍 Hover over or click markers to reveal <b>station and stop details</b>, including bus service numbers and MRT line information."))
                  )
                ))
              ),
              br(),
              fluidRow(tabsetPanel(
                tabPanel("MRT Map",
                         column(width = 9, leafletOutput("mrt_connectivity_map", height = "700px")), 
                         column(width = 3, box(width = NULL, 
                                               tags$h4("Select MRT Lines to Display:"), 
                                               checkboxGroupInput(
                                                 inputId = "selected_groups",
                                                 label = NULL,
                                                 choiceNames = lapply(names(mrt_line_groups), function(name) {
                                                   color <- mrt_line_colors[mrt_line_groups[[name]][1]]  
                                                   HTML(paste0("<span style='color:", color, "'><b>", name, "</b></span>"))
                                                 }),
                                                 choiceValues = names(mrt_line_groups),
                                                 selected = names(mrt_line_groups)
                                               )))
                ),
                tabPanel("Bus Stop Map", leafletOutput("bus_stop_map", height = "700px")),
                tabPanel("Bus Route Map",
                         fluidRow(
                           column(9),
                           column(3, selectInput("selected_route", "Choose Bus Route:",
                                                 choices = sort(unique(bus_routes$ServiceNo)), 
                                                 selected = "966"))), 
                         leafletOutput("bus_route_map", height = "700px"))))), 
      
      # COMMUTER HUB TAB
      tabItem(tabName = "commuter_hub",
              fluidRow(
                actionButton("toggle_com_notes", "📚 Guide on Commuter Hub Mode", style = "margin-right: 10px;", onclick = "event.stopPropagation();"),
                hidden(div(
                  id = "com_notes", class = "collapsible-content",
                  tags$ul(
                    tags$li(HTML("<b>Explore the Commuter Hub Map</b><br>
      🗺️ Click on the map to explore <b>commuter hub density</b> visually.")),
                    tags$li(HTML("<b>Search and Filter</b><br>
      🔍 Use the table's <b>search bar</b> to filter subzones or planning areas of your choice.")),
                    tags$li(HTML("<b>Compare with the Chart</b><br>
      📊 Check the <b>Chart</b> to compare average commuter hub scores across planning areas.")),
                    tags$li(HTML("<b> Interpreting the Commuter Hub Density score</b><br>
💡 With a minimum of 0 and a maximum of 1, a higher commuter hub density score indicates higher movement of commuters, suggesting it as a major transport hub."))
                  )
                ))
              ),
              br(),
              fluidRow(
                column(width = 6,
                       box(title = "🗺️ Commuter Hub Map", width = NULL, height = "800px",
                           leafletOutput("commuter_map", height = "700px")
                       )
                ),
                column(width = 6,
                       box(title = "📊 Commuter Hub Data", width = NULL, height = "800px",
                           tabsetPanel(
                             tabPanel("Table", 
                                      textInput("subzone_search", "🔍 Search for an Area:", ""),
                                      DTOutput("commuter_table"), height = "700px"
                             ),
                             tabPanel("Chart", 
                                      plotlyOutput("commuter_chart", height = "700px"))))))),
      
      # TRANSPORT ACCESSIBILITY TAB
      tabItem(tabName = "accessibility",
              fluidRow(
                actionButton("toggle_acc_notes", "📚 Guide on Transport Accessibility Mode",style = "margin-right: 10px;", onclick = "event.stopPropagation();"),
                hidden(div(
                  id = "acc_notes", class = "collapsible-content",
                  tags$ul(
                    tags$li(HTML("<b>Select a Transport Mode</b><br>
      🚇 Choose your preferred mode (All/Bus/MRT) to view the <b>accessibility score</b> of each subzone or planning area via the map, chart, or table.")),
                    tags$li(HTML("<b>Search and Filter</b><br>
      🔍 Use the table's <b>search bar</b> to filter subzones or planning areas of your choice.")),
                    tags$li(HTML("<b>Adjust by Time of Day</b><br>
      🕰️ Drag the <b>time slider</b> below the map to view how accessibility scores change throughout the day.")),
                    tags$li(HTML("<b> Interpreting the accessibility score</b><br>
💡 With a minimum of 0 and a maximum of 1, a higher accessibility score indicates greater ease of reaching the area via public transport."))
                  )
                ))),
              br(),
              fluidRow(
                column(width = 12,
                       selectInput("transport_mode", "Select Transport Mode:",
                                   choices = c("All", "Bus", "MRT")))
              ),
              fluidRow(
                column(width = 6,
                       box(title = "🗺️ Transport Accessibility Map", width = NULL, height = "800px",
                           leafletOutput("accessibility_map", height = "575px"),
                           conditionalPanel(
                             condition = "input.transport_mode == 'All'",
                             sliderInput("time_of_day", "Select Hour of Day:",
                                         min = 0, max = 23, value = 12, step = 1, animate = TRUE)))),
                
                column(width = 6, 
                       box(title = "📊 Transport Accessibility Data", width = NULL, height = "800px",
                           tabsetPanel(
                             tabPanel("Table",
                                      textInput("subzone_search", "🔍 Search for an Area:", ""),
                                      DTOutput("accessibility_table"), height = "700px"
                             ),
                             tabPanel("Chart", 
                                      plotlyOutput("accessibility_chart", height = "700px"))))))),
      
      # SIMULATION TAB
      tabItem(tabName = "simulation",
              fluidRow(
                actionButton("toggle_sim_notes", "📚 Guide on Simulation Mode", style = "margin-right: 10px;",onclick = "event.stopPropagation();"),
                hidden(div(
                  id = "sim_notes", class = "collapsible-content",
                  tags$ul(
                    tags$li(HTML("<b>Explore the Interactive Map</b><br>
      🗺️ Click on the map to view the <b>Commuter-Accessibility Ratio</b> of different areas and see their current transport availability!")),
                    tags$li(HTML("<b>Choose Your Preferred Visualization Tool</b><br>
      ⏲️ Toggle between the <b>map</b>, <b>chart</b>, and <b>table</b> to compare the ratios in more detail.")),
                    tags$li(HTML("<b>Search for an Area</b><br>
      🔍 Enter the name of the area you wish to explore into the <b>search bar</b> in the table.")),
                    tags$li(HTML("<b>Use the Year Slider</b><br>
      📊 Adjust the <b>year slider</b> below the map and table to see how the ratio changes over time.")),
                    tags$li(HTML("<b>Interpreting the Commuter-Accessibility Ratio</b><br>
🟢 A lower ratio means the area has adequate transport infrastructure for its commuter demand.<br>
🔴 A higher ratio highlights potential infrastructure gaps relative to the volume of commuters."))
                  )
                ))),
              br(),
              fluidRow(
                column(width = 7,
                       box(title = "️Commuter-Accessibility Ratios", width = NULL,height="900px",
                           # MAP 
                           tabsetPanel(
                             tabPanel("🗺 Map", 
                                      leafletOutput("sim_map", height = "700px"), 
                                      sliderInput("map_slider", 
                                                  "Select Year:",
                                                  min = 2025, max = 2035, value = 2025,step = 1,
                                                  animate = TRUE, sep = "")
                             ),
                             
                             # CHART WITH SLIDING YEAR
                             tabPanel("📊 Chart", 
                                      plotlyOutput("sim_chart", height = "700px"), ## to change 
                                      sliderInput("year_slider", 
                                                  "Select Year:",
                                                  min = 2025, max = 2035, value = 2025,step = 1,
                                                  animate = TRUE, sep = "") 
                             ),
                             
                             # TABLE WITH ALL YEARS
                             tabPanel("📋 Table", 
                                      textInput("area_search", "🔍 Search for an Area:", ""), 
                                      DTOutput("sim_table"), height = "700px" ## to change
                             )
                           )
                       )
                ),
                column(width = 5,
                       box(
                         title = ("Simulation"), 
                         width = NULL, 
                         actionButton("toggle_sim_guide", "❓How to Run Your Simulation", onclick = "event.stopPropagation();"),
                         
                         hidden(div(id = "sim_guide", class = "collapsible-content",
                                    tags$ol(
                                      tags$li(HTML("<b>Select</b> your desired area and year.")),
                                      tags$li(HTML("<b>Input your changes</b> for the following:"),
                                              tags$ul(
                                                tags$li("Total Buses per hour"),
                                                tags$li("Total Number of Bus Stops"),
                                                tags$li("Total Number of Bus Services in the Area"),
                                                tags$li("Total Number of MRT Stations"),
                                                tags$li("Total Number of MRT lines")
                                              )
                                      ),
                                      tags$li(HTML("<b>Click</b> the 'Add change' button to make your changes."))
                                    ),
                                    tags$p(HTML("<i>Tip:</i> The year you select will affect future ratios too!")),
                                    tags$ol(start = 4,
                                            tags$li(HTML("<b>Manage your changes:</b>"),
                                                    tags$ul(
                                                      tags$li("After adding changes, you’ll see them listed in a table below."),
                                                      tags$li(HTML("To remove a specific change, just click the <b>'X'</b> button next to it.")),
                                                      tags$li(HTML("To remove all changes at once and reset the simulation, click the <b>'Clear All Changes'</b> button."))
                                                    )
                                            )
                                    ),
                                    tags$p(HTML("<b>Need more info before adjusting?</b><br>Click directly on the map to explore each area’s current transport options — it’ll help you make better decisions for your simulation.")),
                                    tags$p(HTML("<b>Reminder:</b><br>A lower ratio means better transport coverage, so aim for lower values when optimizing your area!"))
                         ))
                         ,
                         selectInput("year", "Select year", choices = all_years, selected = all_years[1]),
                         selectInput("planning_area", "Select area", choices = area_names),
                         textInput("busesph", "Input changes to buses per hour", 0),
                         textInput("busstops", "Input changes to bus stops", 0),
                         textInput("busservices", "Input changes to bus services", 0),
                         textInput("mrtstations", "Input changes to MRT stations", 0),
                         textInput("mrtlines", "Input changes to MRT lines", 0),
                         fluidRow(
                           column(width = 6, actionButton("makechange", "Add change")),
                           column(width = 6, actionButton("clearchanges", "Clear all changes"))
                         ),
                         DTOutput("changes_table")
                       )
                       
                )
              )
      )
    )
  ),
)

##############
#   Server   #
##############
server <- function(input, output, session) {
  output$data_sources <- renderTable({
    data.frame(
      "Source" = c("LTA DataMall", "SingStat", "Data.gov.sg"),
      "Description" = c("Bus stops, MRT stations, ridership statistics, train/bus routes",
                        "Population data, employment zones, transport modes",
                        "Official subzone boundaries and GIS shapefiles"),
      "Use" = c("Assess transport density & ridership patterns",
                "Correlate transport access with demand",
                "Define region boundaries for accessibility scoring")
    )
  })
  
  #For all collapsible buttons
  observeEvent(input$toggle_tech, {
    toggle(id = "technicals", anim = TRUE)
  })
  observeEvent(input$toggle_tech1, {
    toggle(id = "tech1", anim = TRUE)
  })
  observeEvent(input$toggle_tech2, {
    toggle(id = "tech2", anim = TRUE)
  })
  observeEvent(input$toggle_tech3, {
    toggle(id = "tech3", anim = TRUE)
  })
  observeEvent(input$toggle_data, {
    toggle(id = "data", anim = TRUE)
  })
  observeEvent(input$toggle_com_notes, {
    toggle(id = "com_notes", anim = TRUE)
  })
  observeEvent(input$toggle_acc_notes, {
    toggle(id = "acc_notes", anim = TRUE)
  })
  observeEvent(input$toggle_sim_notes, {
    toggle(id = "sim_notes", anim = TRUE)
  })
  observeEvent(input$toggle_sim_guide, {
    toggle(id = "sim_guide", anim = TRUE)
  })
  observeEvent(input$toggle_curr_notes, {
    toggle(id = "current_notes", anim = TRUE)
  })
  
  
  
  ### Current MRT Map
  output$mrt_connectivity_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 103.851959, lat = 1.290270, zoom = 11)
  })
  
  observe({
    req(input$selected_groups)
    selected_lines <- unlist(mrt_line_groups[input$selected_groups])
    
    leafletProxy("mrt_connectivity_map") %>%
      clearGroup(group = names(mrt_line_colors)) %>%
      clearGroup(group = paste0(names(mrt_line_colors), "_stations")) %>%
      {
        for (station_line in selected_lines) {
          line_data <- mrt[mrt$station_line == station_line, ] %>%
            arrange(as.numeric(station_sequence))
          
          color <- mrt_line_colors[[station_line]]
          
          . <- addPolylines(., data = line_data,
                            lng = ~lon, lat = ~lat,
                            color = color, weight = 3, opacity = 0.8,
                            group = station_line)
          
          . <- addCircleMarkers(., data = line_data,
                                lng = ~lon, lat = ~lat,
                                radius = 5, color = color,
                                label = ~paste0(station_name, " (", station_code, ")"),
                                popup = ~paste0("<b style='color:", color, "'>", station_name, "</b><br>",
                                                "Code: ", station_code, "<br>",
                                                "Lines: ", all_lines), 
                                group = paste0(station_line, "_stations"))
        }
        .
      }
  })
  
  
  ### Current Bus Stops Map
  output$bus_stop_map <- renderLeaflet({
    leaflet(bus_stop_info) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 1, color = "blue", fillOpacity = 0.3,
        popup = ~paste("<b>", Description, "</b><br>",
                       "Bus Stop Code: ", BusStopCode, "<br>",
                       "Buses: ", buses)
      )
  })
  
  ### Bus Route Map
  output$bus_route_map <- renderLeaflet({
    req(input$selected_route) 
    
    # Filter the bus route data by the selected route
    route_data <- bus_routes %>%
      filter(ServiceNo == input$selected_route) %>%
      arrange(Direction, StopSequence)
    
    leaflet(route_data) %>%
      addTiles() %>%
      addPolylines(
        lng = ~Longitude, lat = ~Latitude,
        color = ~ifelse(Direction == "1", "red", "blue"), 
        weight = 4, opacity = 0.7, 
        label = ~paste("Service: ", ServiceNo)
      ) %>%
      
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 4, 
        color = ~ifelse(Direction == "1", "darkred", "darkblue"), 
        fillOpacity = 0.7,
        label = ~paste(Description),
        popup = ~paste("<b>", Description, "</b><br>",
                       "Stop Code: ", BusStopCode, "<br>",
                       "Direction: ", Direction, "<br>",
                       "Stop Sequence: ", StopSequence)
      )
  })
  
  # Commuter Hub Table
  output$commuter_table <- renderDT({
    # when search input is empty, still shows all data
    if (is.null(input$subzone_search) || input$subzone_search == "") {commuter_data4table
    } else {
      commuter_data4table <- commuter_data4table %>%
        filter(
          str_detect(`Subzone`, regex(input$subzone_search, ignore_case = TRUE)) |
            str_detect(`Planning Area`, regex(input$subzone_search, ignore_case = TRUE))
        )
    }
    datatable(commuter_data4table,
              options = list(pageLength = 10,  scrollY = "450px", searching = FALSE), 
              rownames = FALSE, 
              filter = "none")
  })
  
  # Commuter Hub Map
  output$commuter_map <- renderLeaflet({
    leaflet(commuter_subzone_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorQuantile("Reds", relative_commuter_score)(relative_commuter_score), 
        fillOpacity = 0.4,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>", subzone_name, "</b><br>Commuter Score: ", relative_commuter_score)
      ) %>%
      addLegend(position = "bottomright",
                colors = c("#B71C1C", "#D32F2F", "#EF5350", "#FFCDD2", "#FFEBEE"),
                labels = c("Very High", "High", "Medium", "Low", "Very Low"),
                title = "Commuter Hub Score",
                opacity = 1
      )
  })
  
  # Commuter Hub Chart
  output$commuter_chart <- renderPlotly({
    com_chart <- ggplot(commuter_planning_area, aes(x = reorder(planning_area_name, avg_commuter_score), 
                                                    y = avg_commuter_score, 
                                                    fill = avg_commuter_score, 
                                                    title = "Commuter Hub Density Score",
                                                    text = paste("Planning Area:", planning_area_name, 
                                                                 "<br>Score:", round(avg_commuter_score, 2)))) +
      geom_col() +
      scale_fill_distiller(palette = "Reds", direction = 1, name = "Score") +
      coord_flip() +
      labs(x = "Planning Area", y = "Average Commuter Score", title = "Average Commuter Score by Planning Area") +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(size = 7),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    
    ggplotly(com_chart, tooltip = "text")
  })
  
  
  # Transport Accessibility Table
  output$accessibility_table <- renderDT({
    if (input$transport_mode == "All") {
      accessibility_data4table <- accessibility_data4table %>%
        select(`Subzone`, `Planning Area`, `Total Transport Accessibility Score`)
    } else if (input$transport_mode == "Bus") {
      accessibility_data4table <- accessibility_data4table %>%
        select(`Subzone`, `Planning Area`, `Bus Accessibility Score`)
    } else {
      accessibility_data4table <- accessibility_data4table %>%
        select(`Subzone`, `Planning Area`, `MRT Accessibility Score`)
    }
    
    # when search input is empty, still shows all data
    if (is.null(input$subzone_search) || input$subzone_search == "") {accessibility_data4table
    } else {
      accessibility_data4table <- accessibility_data4table %>%
        filter(
          str_detect(`Subzone`, regex(input$subzone_search, ignore_case = TRUE)) |
            str_detect(`Planning Area`, regex(input$subzone_search, ignore_case = TRUE))
        )
    }
    datatable(accessibility_data4table,
              options = list(pageLength = 10, scrollY = "450px",searching = FALSE), 
              rownames = FALSE, 
              filter = "none")
  })  
  
  # Transport Accessibility Map
  output$accessibility_map <- renderLeaflet({
    
    if (input$transport_mode == "All") {
      
      hour_data <- hourly_data %>%
        filter(hour == input$time_of_day) %>%
        mutate(hourly_acc_score_scaled = round(acc_score_scaled, 3)) %>%
        select(subzone_name, hourly_acc_score_scaled)
      
      accessibility_map_data <- accessibility_subzone_data %>% 
        left_join(hour_data, by = "subzone_name") %>%
        left_join(raw_acc_values, by = "subzone_name") %>%
        mutate(accessibility_score = ifelse(is.na(hourly_acc_score_scaled), 0, hourly_acc_score_scaled))
      
      if (input$time_of_day >= 1 & input$time_of_day <= 4) {
        color_scale <- colorBin(
          palette = "Reds",
          domain = c(0, 1),
          bins = c(seq(0, 0.01, by = 0.002), 0.02, 1))
      } else {
        color_scale <- colorQuantile(
          palette = "Oranges",
          domain = accessibility_map_data$accessibility_score,
          n = 5)
      }
      
      leaflet(accessibility_map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~color_scale(accessibility_score), 
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste0(
            "<b>", subzone_name, "</b><br>",
            "Accessibility Score: ", accessibility_score, "<br>",
            "No. of Bus Stops: ", round(num_bus_stops, 0), "<br>",
            "No. of Bus Services: ", round(num_bus_services, 0), "<br>",
            "No. of Train Stations: ", round(num_stations_per_subzone, 0), "<br>",
            "No. of Train Lines: ", round(num_train_lines_per_subzone, 0))
        ) %>%
        addLegend(position = "bottomright",
                  colors = c("#F24C27","#FB8C4A","#FDBA74", "#FEE0A8", "#FFF5EB"),
                  labels = c("Very High", "High", "Medium", "Low", "Very Low"),
                  title = "Accessibility Score",
                  opacity = 1)
    } else if (input$transport_mode == "Bus") {
      bus_data <- raw_acc_values %>%
        select(subzone_name, num_bus_stops, num_bus_services)
      
      accessibility_map_data <- accessibility_subzone_data %>%
        left_join(bus_data, by = "subzone_name") %>%
        mutate(accessibility_score = ifelse(is.na(bus_acc_score_scaled), 0, bus_acc_score_scaled))
      
      pal <- colorBin(
        palette = rev("Greens"),
        domain = accessibility_map_data$accessibility_score,
        bins = rev(c(0, 0.05, 0.1, 0.15, 0.2, 0.4, 0.6, 0.8, 1)))
      
      leaflet(accessibility_map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(accessibility_score),
          fillOpacity = 0.7,
          color = "white", 
          weight = 1,
          popup = ~paste0("<b>", subzone_name, "</b><br>",
                          "Accessibility Score: ", accessibility_score, "<br>",
                          "No. of Bus Stops: ", round(num_bus_stops, 0), "<br>",
                          "No. of Bus Services: ", round(num_bus_services, 0))) %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = ~accessibility_score,
                  title = "Accessibility Score",
                  opacity = 1)
    } else {
      mrt_data <- raw_acc_values %>%
        select(subzone_name, num_stations_per_subzone, num_train_lines_per_subzone)
      
      accessibility_map_data <- accessibility_subzone_data %>%
        left_join(mrt_data, by = "subzone_name") %>%
        mutate(accessibility_score = ifelse(is.na(mrt_acc_score_scaled), 0, mrt_acc_score_scaled))
      
      pal <- colorBin(
        palette = rev("Blues"),
        domain = accessibility_map_data$accessibility_score,
        bins = rev(c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 1)))
      
      leaflet(accessibility_map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(accessibility_score),
          fillOpacity = 0.7,
          color = "white", 
          weight = 1,
          popup = ~paste0("<b>", subzone_name, "</b><br>",
                          "Accessibility Score: ", accessibility_score, "<br>",
                          "No. of Train Stations: ", round(num_stations_per_subzone, 0), "<br>",
                          "No. of Train Lines: ", round(num_train_lines_per_subzone, 0))) %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = ~accessibility_score,
                  title = "Accessibility Score",
                  opacity = 1)
    }
  })
  
  
  # Transport Accessibility Chart
  output$accessibility_chart <- renderPlotly({
    selected_score <- ifelse(input$transport_mode == "All", "avg_acc_score", ifelse(input$transport_mode == "Bus", "avg_bus_acc_score", "avg_mrt_acc_score"))
    acc_chart <- ggplot(accessibility_planning_area, aes(x = reorder(planning_area_name, get(selected_score)),
                                                         y = get(selected_score),
                                                         fill = get(selected_score),
                                                         text = paste("Planning Area:", planning_area_name,
                                                                      "<br>Score:", round(get(selected_score), 2)))) +
      geom_col() +
      scale_fill_distiller(palette = ifelse(selected_score == "avg_acc_score", "Reds", 
                                            ifelse(selected_score == "avg_bus_acc_score", "Greens", "Blues")), direction = 1, name = "Score") + 
      coord_flip() +
      labs(x = "Planning Area",
           y = "Average Accessibility Score", 
           title = paste("Average",
                         switch(selected_score, 
                                "avg_acc_score" = NULL,
                                "avg_bus_acc_score" = "Bus",
                                "avg_mrt_acc_score" = "MRT"),
                         "Accessibility Score by Planning Area")) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(size = 7),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    ggplotly(acc_chart, tooltip = "text")
  })
  
  #SIMULATION NEEDS
  changes <- reactiveVal(list())
  
  final_ratio <- reactive({
    current_changes <- changes()
    if (length(current_changes) > 0) {
      changes_data <- lapply(current_changes, function(x) {
        x[c("area_name", "year", "n_bus_per_hour_diff", 
            "n_bus_stops_diff", "n_bus_services_diff",
            "n_stations_diff", "n_lines_diff")]})
      make_all_changes_df(initial_stations, changes_data, acc_scores_all_years, future_com_by_planning_area)
    } else {initial_ratio}})
  
  # EVENT MAKE CHANGE
  observeEvent(input$makechange, {
    req(input$planning_area, input$year, input$busesph, input$busstops, 
        input$busservices, input$mrtstations, input$mrtlines)
    
    new_change <- list(
      area_name = as.character(input$planning_area),
      year = as.character(input$year),
      n_bus_per_hour_diff = as.numeric(input$busesph),
      n_bus_stops_diff = as.numeric(input$busstops),
      n_bus_services_diff = as.numeric(input$busservices),
      n_stations_diff = as.numeric(input$mrtstations),
      n_lines_diff = as.numeric(input$mrtlines)
    )
    
    isolate({
      current_changes <- changes()
      changes(c(current_changes, list(new_change)))})
    
    sapply(c("busesph", "busstops", "busservices", "mrtstations", "mrtlines"),
           function(x) updateTextInput(session, x, value = "0"))})
  
  
  # EVENT CLEAR CHANGES
  observeEvent(input$clearchanges, {changes(list())})
  
  # CHANGES TABLE 
  output$changes_table <- renderDT({
    req(length(changes()) > 0)
    
    changes_df <- data.frame(
      Area = sapply(changes(), function(x) x$area_name),
      Year = sapply(changes(), function(x) x$year),
      `Bus/Hour` = sapply(changes(), function(x) x$n_bus_per_hour_diff),
      `Bus Stops` = sapply(changes(), function(x) x$n_bus_stops_diff),
      `Bus Services` = sapply(changes(), function(x) x$n_bus_services_diff),
      `MRT Stations` = sapply(changes(), function(x) x$n_stations_diff),
      `MRT Lines` = sapply(changes(), function(x) x$n_lines_diff),
      Remove = sapply(1:length(changes()), function(i) {
        as.character(actionButton(
          paste0("delete_", i),
          label = "X",
          onclick = paste0('Shiny.setInputValue("delete_row", ', i, ')'),
          class = "btn-danger btn-sm"
        ))
      }),
      check.names = FALSE
    )
    
    datatable(
      changes_df, escape = FALSE, rownames = FALSE,
      options = list(scrollX = TRUE, scrollY = "200px", paging = FALSE, dom = 't',autoWidth = TRUE,
                     columnDefs = list(list(targets = "_all", className = "dt-center"))
      )
    )
  })
  
  # EVENT DELETE ROW 
  observeEvent(input$delete_row, {
    req(input$delete_row)
    changes(changes()[-input$delete_row])
  })
  
  # SIMULATION MAP
  current_df <- reactive({
    planning_area_geometry %>% 
      left_join(final_ratio(), by = "planning_area_name")
  })
  
  selected_year <- reactive({
    req(input$map_slider)  
    as.character(input$map_slider)
  })
  
  pal <- reactive({
    req(current_df(), selected_year())
    colorNumeric("RdYlGn", domain = current_df()[[selected_year()]], reverse = TRUE, na.color = "gray")
  })
  
  output$sim_map <- renderLeaflet({
    req(current_df(), selected_year(), pal())
    
    current_year <- selected_year()
    df <- current_df()
    df <- df %>% left_join(initial_stations, by = "planning_area_name")
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal()(df[[current_year]]), 
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>", planning_area_name, 
                        "</b><br>Commuter-Accessibility Ratio: ", round(df[[current_year]], 2), 
                        "</b><br>2025 Number of Buses/Hour: ", round(total_buses_per_hour), 
                        "</b><br>2025 Number of Bus Stops: ", num_bus_stops, 
                        "</b><br>2025 Number of Bus Services: ", round(num_bus_services), 
                        "</b><br>2025 Number of MRT Stations: ", num_stations_per_subzone, 
                        "</b><br>2025 Number of MRT Lines: ", round(num_train_lines_per_subzone))
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal(), 
        values = na.omit(df[[current_year]]), 
        title = "Ratio",
        opacity = 1
      )
  })
  
  # SIMULATION CHART
  output$sim_chart <- renderPlotly({
    req(input$year_slider, current_df())
    
    year_col <- as.character(input$year_slider)
    df <- current_df()
    
    plot_data <- df %>%
      arrange(-is.na(.data[[year_col]]), .data[[year_col]]) %>%
      mutate(planning_area_name = factor(planning_area_name, levels = unique(planning_area_name)))
    
    p <- ggplot(plot_data, 
                aes(x = planning_area_name,
                    y = .data[[year_col]], 
                    fill = .data[[year_col]],
                    text = paste("Planning Area:", planning_area_name,
                                 "<br>Commuter-Accessibility Ratio:", round(.data[[year_col]], 2)))) +
      geom_col() +
      scale_fill_distiller(palette = "RdYlGn", direction = -1) +
      coord_flip() +
      labs(x = "Planning Area", 
           y = "Commuter-Accessibility Ratio", 
           title = paste("Commuter-Accessibility Ratio (", input$year_slider, ")")) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 7), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    
    ggplotly(p, tooltip = "text") 
  })
  
  # SIMULATION TABLE
  output$sim_table <- renderDT({
    req(current_df())
    
    table_data <- current_df() %>% 
      st_drop_geometry() %>%
      mutate(across(where(is.numeric),\(x) round(x, digits = 3))) %>%
      rename(Area = planning_area_name)
    
    if (is.null(input$area_search) || input$area_search == "") {table_data
    } else {
      table_data <- table_data %>%
        filter(str_detect(Area, regex(input$area_search, ignore_case = TRUE)))}
    
    datatable(
      table_data, extensions = 'FixedColumns', rownames = FALSE,
      options = list(
        pageLength = 15,
        searching = FALSE,
        scrollX = TRUE, scrollY = "600px",
        fixedColumns = list(leftColumns = 1)
      )
    )
  })
}


####################
#     shinyApp     #
####################
shinyApp(ui = ui, server = server)
