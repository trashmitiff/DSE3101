
# Public Transport Accessibility Analysis in Singapore

This project aims to analyze and enhance the understanding of commuter hubs within Singapore’s transport network by identifying high-traffic areas, measuring accessibility across different regions, and uncovering potential gaps in the transport infrastructure. The insights gained will support the Land Transport Authority (LTA) in improving mobility, optimizing transport planning, and making informed decisions about infrastructure investments. 


## Frontend
Our dashboard is run via RShiny, which can be accessed [here](https://wooderland.shinyapps.io/benny/).

The dashboard is built using R Shiny and is structured using the shinydashboard framework. Custom CSS and HTML elements were utilised to deliver an intuitive and modular user interface. The UI is structured into five primary modules, where users may toggle between the different tabs using the collapsible side navigation bar.

Current Transport Visualisation Tab:
This tab was designed to allow users to visualise Singapore’s current transport network in detail. It contains 3 subtabs:

MRT Map: view of current MRT lines and stations
![image](https://github.com/user-attachments/assets/b66da4af-7217-42f0-ac51-0698ef4b2b92)

Bus Stop Map: view of current bus stops
![image](https://github.com/user-attachments/assets/b3f7ef70-c3f4-4788-ae2a-5fe2b6ef75bf)


Bus Routes Map: view of all current bus routes
![image](https://github.com/user-attachments/assets/92a3875d-d3a5-42fe-9b20-bcc2a2e9f860)


Commuter Hub Mode:

![image](https://github.com/user-attachments/assets/2cf6e673-070c-409c-9241-0ea08323845b)


The Commuter Hub Mode was designed to allow users to identify commuter hubs as focus points for public transport system strategies. This tab visualizes ridership hotspots in subzone areas using a PCA-derived commuter hub score based on inflow/outflow metrics and tap-in data. The scores are normalised from a range of 0 to 1, where a higher score indicates higher movement of commuters, suggesting it as a denser and major transport hub. Higher scores are reflected as darker shades on the map.

Key Features:
1. Choropleth map of subzone-level commuter hub scores
2. Bar chart and searchable data table
3. Help button with collapsible guide



Transport Accessibility Mode:
![image](https://github.com/user-attachments/assets/d884393e-1980-4c81-8023-ae803f819a54)


The Transport Accessibility Mode was designed to allow users to explore current coverage by MRT, bus, or both. Users can also toggle by time of day to compare peak and off-peak accessibility, helping identify underserved regions. This tab displays accessibility scores across subzones by transport type and has a similar tab layout as the Commuter Hub Mode. The scores are similarly normalised, with a higher accessibility score indicating greater ease of reaching the area via public transport.

Key Features:
1.  Mode selector (All/Bus/MRT)
2. Hour-of-day slider to simulate variations of accessibility throughout the day
3. Interactive choropleth map, table and chart

Simulation Mode: 
![image](https://github.com/user-attachments/assets/99836538-e7be-46b4-9d4d-356bfdf0a484)

The Simulation Mode was designed to allow users to test real-world trade-offs by placing a new stop or adjusting bus frequency in their preferred planning areas. They can see how accessibility improves across time and region, giving them immediate feedback on possible interventions. 

It displays commuter-accessibility ratios across a larger planning area, comprising multiple subzones. The ratio of an area is calculated by (commuter density score/ accessibility score). A lower ratio for an area signifies adequate transport infrastructure for its commuter demand, while a higher ratio highlights potential infrastructure gaps relative to commuter volume. Users may simulate changes to an area’s transport infrastructure by adjusting inputs. Simulated changes are assumed to persist from the input year onward, affecting scores in future years.

Key Features: 
1. Interactive choropleth map, table and chart
2. Simulation Table
    - Users select year and area, making changes to buses per hour, bus stops, no. of bus services, MRT stations and MRT lines. 
    - Negative inputs are supported, representing removal of infrastructure. 
3. Simulation Changes Table
4. Year Slider: Adjustable time slider to view how ratios change over the years. 


## Backend
We made use of variance-adjusted Principal Component Analysis to derive accessibility and commuter hub scores. Below are some key files where data preprocessing and modelling are conducted:

- DSEproject.ipynb
    - Data cleaning 
        - Removal of duplicates
        - Imputation of missing data
        - Data quality validation
   - Various data sources were combined and organised on a subzone level
- PCA.ipynb
    - Application of variance-adjusted Principal Component Analysis
    - Derivation of weights for features


## Run locally

1. Clone the project

2. Go to the project directory

```bash
  cd DSE3101
```

3. Install dependencies

```bash
  pip install -r requirements.txt
```

## Acknowledgements

Data Sources
 - [Data.gov](http://data.gov)
 - [LTA Dynamic API](https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html)
 - [LTA Static Datasets](https://datamall.lta.gov.sg/content/datamall/en/static-data.html)
 - [SingStat Data](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data)

Others
- [Scraping Method for train station information](https://github.com/elliotwutingfeng/singapore_train_station_coordinates/blob/main/station_coordinates.pyv)
