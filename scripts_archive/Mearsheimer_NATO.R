##Final_Version


library(tidyverse)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(visdat)
library(here)

load(here::here("data", "tidy_data", "nato_nations.rda") )


nato_nations$brk_a3 <- nato_nations$iso_a3 

nato_nationsm <- nato_nations %>%
  select(-country)



world <- ne_countries(scale = "medium", returnclass = "sf")

EU_Map2  <- world %>% filter(region_wb == "Europe & Central Asia")





# First Graph -------------------------------------------------------------

map_one <- EU_Map2 %>% 
  left_join(nato_nationsm, by = "brk_a3") %>%
  ggplot() +
  geom_sf( aes(fill = joined_NATO)) +
  coord_sf(xlim = c(-27,52), ylim = c(33,72), expand = FALSE) 

map_one <- map_one +
  labs(fill = "Year", title = "European NATO Alliance: 2022",
       x = "", y = "",
       caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 40, y = 54, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine")


map_one <- map_one +
  theme(legend.position = c(0.1, 0.4))
# Map Two -----------------------------------------------------------------



wpact <- c("1949", "1952", "1955", "1982")

warsaw_pact <- nato_nationsm %>%
  filter(joined_NATO %in% wpact ) %>% droplevels()

post_pact <- nato_nationsm %>%
  filter(!joined_NATO %in% wpact ) %>% droplevels()


##


##

map_two <- EU_Map2 %>% 
  left_join(warsaw_pact , by = "brk_a3") %>%
  ggplot() +
  geom_sf( aes(fill = joined_NATO)) +
  coord_sf(xlim = c(-27,52), ylim = c(33,72), expand = FALSE) 


map_two <- map_two +
  labs(fill = "Year",
       title = "European NATO Alliance: Warsaw Pact Years",
       x = "", y = "", caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 40, y = 54, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine")


map_two <- map_two +
  theme(legend.position = c(0.1, 0.5))


# Map Three ---------------------------------------------------------------



map_three <- EU_Map2 %>% 
  left_join(post_pact  , by = "brk_a3") %>%
  ggplot() +
  geom_sf( aes(fill = joined_NATO)) +
  coord_sf(xlim = c(-27,52), ylim = c(33,72), expand = FALSE) 


map_three <- map_three +
  labs(fill = "Year", 
       title = "European NATO Expansion: Post-Warsaw Pact",
       x = "", y = "",
       caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 40, y = 54, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine")


map_three <- map_three +
  theme(legend.position = c(0.1, 0.5))



# Map Four  ---------------------------------------------------------------

ukr_nato <- nato_nationsm %>%
  mutate(NATO = case_when(iso_a3 == "UKR" ~ 1,
                          TRUE ~ NATO)  ) %>%
  mutate(enemy = case_when(NATO == 1 ~ "YES",
                           TRUE ~ NA_character_) )

map_four <- EU_Map2 %>% 
  left_join(ukr_nato , by = "brk_a3") %>%
  ggplot() +
  geom_sf( aes(fill = enemy)) +
  coord_sf(xlim = c(-27,52), ylim = c(33,72), expand = FALSE) 



map_four <- map_four +
  labs(fill = "Year", title = "If Ukraine Flipped to NATO ...",
       x = "", y = "",
       caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine") +
  annotate("text", x = 43, y = 55, 
           label = "Russia:\n indefensible \nwestern\n  borders\n ") +
  guides(fill = "none") + scale_fill_manual(values = "red" )


map_four


just_maps <- c("map_one", "map_two" , "map_three", "map_four")

save(list = just_maps, file = here::here("data", "tidy_data", "four_NATO_maps.rda"))