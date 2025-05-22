# This is a map for the number of countries included in the interview data from Micah

# Author: Chris LeBoa
# Version: 2025-05-22

#This document makes a map of the numvber of studies that include specific countries for the IW review

#install.packages("sf")
# Libraries
library(tidyverse)
library(googlesheets4)
library(sf)
# Parameters
table_all <- read_sheet("1LLX7EsaaZymIloo3ynSnDpoNRNjtmuUXSsRE1bLWMa4")

#URL to sheet: https://docs.google.com/spreadsheets/d/1hoiEchc_An2ULJrLv5tzTtZpUNpULt2StPNxg9ciuKk/edit?gid=0#gid=0
#table_2_location <- "1xX4svbcqVUlwjBJ1KfHGn6ouBA6khZfQ13yCibdLcDY"

#Data from the interviews
countries <-
  read_sf("/Users/christopherleboa/Library/CloudStorage/GoogleDrive-cleboa@berkeley.edu/.shortcut-targets-by-id/1aMSePP0QEpnGatESIEaZaCJYd9I3tzVA/Mortality Reduction Blast Injury/Review/Code/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
  filter(ADMIN != "Antarctica")
#===============================================================================
table_all$Country


country_count <-
  separate_rows(table_all, Country, sep = ",") %>%
  mutate(
    Country = str_trim(Country, "both"),
    Country = case_when(
      Country == "UK" ~ "United Kingdom",
      Country == "USA" ~ "United States of America",
      Country == "United States" ~ "United States of America",
      Country == "Gaza" ~ "Palestine",
      Country == "DRC" ~ "Democratic Republic of the Congo",
      .default = as.character(Country)
    )) %>%
  group_by(Country) %>%
  summarise(Count = n(), .groups = "drop")


view(country_count)

write_csv(country_count, "IW_country_count.csv")


#[Natural Earth](http://www.naturalearthdata.com) provides free vector and raster map data in a range of scales.
#In particular, download the first file in the section _Admin 0 - Countries_ and save in a place other than a GitHub repo.

data_world <-
  country_count %>%
  mutate(Country = str_trim(Country, "both")) %>%
  arrange(desc(Country)) %>%
  right_join(countries, by = c("Country" = "ADMIN")) %>%
  filter(Country != "Antarctica") %>%
  select(Country, Count, geometry, CONTINENT)




figure_2 <-
 ggplot() +
  geom_sf(data = countries, aes(geometry = geometry), fill = "white") +
  geom_sf(data = data_world, aes(fill = Count, geometry = geometry)) +
  theme_minimal() +
 # scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_colourbar(barwidth = 8, barheight = .5)) +
  theme(legend.position = "bottom") +
  #theme_bw() +
  labs(
    title = "",
    fill = ""
  )

figure_2

ggsave(filename = "map_thermobarics.png", plot = figure_2)


