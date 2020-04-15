library(tidyverse)
library(tigris)
library(ggplot2)
library(sf)
library(tmap)
library(magick)
options(scipen = 999)

tmp <- readxl::read_xlsx("inputs/co-est2019-annres-26.xlsx", skip = 3) %>%
        rename(., "geography" = 1, "population_2018" = "2018") %>%
          filter(geography != "Michigan") %>%
          select(geography, population_2018) %>%
            mutate(geography = str_replace_all(geography, c(" County, Michigan" = "", "Wayne" = "Balance_of_Wayne")),
                   geography = str_sub(geography, 2, -1),
                   population_2018 = ifelse(geography == "Balance_of_Wayne", population_2018 - 672662, population_2018)) %>%
            add_row(., geography = "Detroit City", population_2018 = 672662) %>%
        left_join(., read_csv("https://raw.githubusercontent.com/Wilfongjt/wilfongjt-data/master/covid-19/covid19.csv") %>%
                      rename_all(tolower) %>%
                        replace(., is.na(.), 0) %>%
                          rename(., "geography" = "county") %>%
                            mutate(geography = ifelse(geography == "Wayne", "Balance_of_Wayne", geography), 
                                   geography = str_replace(geography, "St", "St.")),
                  by = "geography") %>%
  complete(date, nesting(geography, population_2018), 
           fill = list(cases = 0, reported_deaths = 0)) %>%
  filter(!is.na(date)) %>%
    left_join(., tigris::counties("MI", cb = TRUE, class = "sf") %>%
                rename_all(tolower) %>%
                rename(., "geography" = name) %>%
                mutate(geography = ifelse(geography == "Wayne", "Balance_of_Wayne", geography)) %>%
                select(geography, aland),
              by = "geography") %>%
  mutate(aland = ifelse(geography == "Detroit City", 359279682,
                        ifelse(geography == "Balance_of_Wayne", aland - 359279682, aland)),
         cases_per_hundred_thousand_residents = ifelse(cases == 0, 0, cases / (population_2018 / 100000)),
         deaths_per_hundred_thousand_residents = ifelse(reported_deaths == 0, 0, reported_deaths / (population_2018 / 100000)),
         sq_km = aland / 1000000,
         residents_per_km2 = population_2018 / sq_km,
         death_rate = reported_deaths / population_2018) %>%
  st_sf(.) 


cases <- ggplot(tmp, aes(x = date, y = cases_per_hundred_thousand_residents)) +
  facet_wrap("geography", nrow = 7) +
    geom_line(col = "blue", size = 1.1) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linetype = "solid", size = 0.4),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.8))

ggsave(cases, filename = "graphics/covid-cases.jpeg", width = 16, height = 10, dpi = 600)

deaths <- ggplot(tmp, aes(x = date, y = deaths_per_hundred_thousand_residents)) +
  facet_wrap("geography", nrow = 7) +
  geom_line(col = "blue", size = 1.1) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "solid", size = 0.4),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(color = "black", fill = "white", size = 0.8))

ggsave(deaths, filename = "graphics/covid-deaths.jpeg", width = 16, height = 10, dpi = 600)

#########
## creates a map of covid death rates by county
county_data <- 
  tmp %>%
    filter(date == max(date)) %>%
      mutate(deaths_per_hundred_thousand_residents =
               ifelse(geography == "Balance_of_Wayne",
                      (reported_deaths[which(geography == "Detroit City")] +
                       reported_deaths[which(geography == "Balance_of_Wayne")]) /
                        ((population_2018[which(geography == "Detroit City")] +
                          population_2018[which(geography == "Balance_of_Wayne")])  / 100000),
                      deaths_per_hundred_thousand_residents)) %>%
  filter(geography != "Detroit City")

county_map <- 
  tm_shape(county_data) +
      tm_fill(col = "deaths_per_hundred_thousand_residents", palette = "Blues", breaks = c(-1, 0, 10, 20, 30, 44), 
              interval.closure = "right", labels = c("0", "1 to 10", "10 to 20", "20 to 30", "30 to 44")) +
  tm_shape(county_data) +
    tm_borders(col = "grey80") +
  tm_layout(legend.title.color = "white",
            main.title = str_c("Covid Deaths Per One Hundred Thousand Residents\n Through ", max(county_data$date)),
            main.title.size = 0.9,
            main.title.position = "center",
            frame = FALSE)

tmap_save(county_map, "graphics/county_map.jpeg")           
  

#####
# maps cases per sq kilometer
#####

county_cases_per_km2 <- 
  tmp %>%
  filter(date == max(date)) %>%
  mutate(cases =
           ifelse(geography == "Balance_of_Wayne",
                  (cases[which(geography == "Detroit City")] +
                  cases[which(geography == "Balance_of_Wayne")]),
                  cases),
         sq_km = ifelse(geography == "Balance_of_Wayne", (aland + 359279682) / 1000000, sq_km),
         cases_per_sq_km = cases / sq_km) %>%
  filter(geography != "Detroit City")

county_cases_per_km2_map <- 
  tm_shape(county_cases_per_km2) +
  tm_fill(col = "cases_per_sq_km", palette = "Blues", breaks = c(-1, 0, .1, 1, 2, 3, 8), 
          interval.closure = "right", labels = c("0", ">0 to .1", ".1 to 1", "1 to 2", "2 to 3", "3 to 8")) +
  tm_shape(county_cases_per_km2) +
  tm_borders(col = "grey80") +
  tm_layout(legend.title.color = "white",
            main.title = str_c("Covid Cases Per Square Kilometer\n Through ", max(county_cases_per_km2$date)),
            main.title.size = 0.9,
            main.title.position = "center",
            frame = FALSE)

tmap_save(county_cases_per_km2_map, "graphics/county_cases_per_km2_map.jpeg")           


county_april_cases_per_km2 <- 
  tmp %>%
  mutate(geography = 
           ifelse(geography == "Detroit City", "Balance_of_Wayne", geography)) %>%
        group_by(geography, date) %>%
          summarise_if(is.numeric, sum) %>%
         mutate(cases_per_sq_km = cases / sq_km) 

county_april_cases_per_km2_map <- 
  tm_shape(county_april_cases_per_km2) +
  tm_facets(along = "date", free.coords = FALSE, nrow = 1, ncol = 1) +
    tm_fill(col = "cases_per_sq_km", palette = "Blues", breaks = c(-1, 0, .1, 1, 3, 6, 8), 
            interval.closure = "right", labels = c("0", "up to .1", ".1 to 1", "1 to 3", "3 to 6", "6 to 8")) +
  tm_shape(county_april_cases_per_km2) +
    tm_borders(col = "grey80") +
  tm_layout(legend.title.color = "white",
            title = str_c("Covid Cases Per Square Kilometer By Date"),
            title.bg.color = "white",
            main.title.size = 0.9,
            main.title.position = "center",
            frame = FALSE) 

tmap_animation(county_april_cases_per_km2_map, filename="graphics/county_april_cases.gif", width=1200, delay=100, loop = TRUE)

magick::image_read("graphics/county_april_cases.gif")





county_april_deaths <- 
  tmp %>%
  mutate(geography = 
           ifelse(geography == "Detroit City", "Balance_of_Wayne", geography)) %>%
  group_by(geography, date) %>%
  summarise_if(is.numeric, sum) 

county_april_deaths_map <- 
  tm_shape(county_april_deaths) +
  tm_facets(along = "date", free.coords = FALSE, nrow = 1, ncol = 1) +
  tm_fill(col = "deaths_per_hundred_thousand_residents", palette = "Blues", breaks = c(-1, 0, 1, 5, 10, 30, 60, 90, 100), 
          interval.closure = "right", labels = c("0", "1", "1 to 5", "5 to 10", "10 to 30", "30 to 60", "60 to 90", "90 to 100")) +
  tm_shape(county_april_deaths) +
  tm_borders(col = "grey80") +
  tm_layout(legend.title.color = "white",
            title = str_c("Covid Deaths Per Hundred Thousand Residents By Date"),
            title.bg.color = "white",
            main.title.size = 0.9,
            main.title.position = "center",
            frame = FALSE) 

tmap_animation(county_april_deaths_map, filename="graphics/county_april_deaths.gif", width=1200, delay=100, loop = TRUE)

magick::image_read("graphics/county_april_deaths.gif")
