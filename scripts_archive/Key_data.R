
library(tidyverse)
library(here)
library(visdat)


load("~/R_STUDIO/TSD_Two/data/tidy_data/world_map2_project.rda")
load("~/R_STUDIO/TSD_Two/data/tidy_data/gapminder.rda")


setdiff(world_map2_ISO$country,  gapminder$country) %>% 
  enframe(name = NULL, value ="diff")


setdiff( gapminder$country, world_map2_ISO$country) %>% 
  enframe(name = NULL, value ="diff")




gapminder$country <- recode(gapminder$country,
                            `Macedonia, FYR`  = "North Macedonia",
                            `West Bank and Gaza` = "Palestine",
                            `Swaziland` =  "Eswatini") 

setdiff( gapminder$country, world_map2_ISO$country) %>% 
  enframe(name = NULL, value ="diff")
## step back to step forward
gdppercapita_ppp_inflation_adjusted <- read_csv("data/raw_data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv")


gdppercapita_ppp_inflation_adjusted %>% vis_dat()

## Mutate to numerics
gdppercapita_ppp_inflation_adjusted <- gdppercapita_ppp_inflation_adjusted %>%
  mutate(across(everything(), as.character))

gdppercapita_ppp_inflation_adjusted %>% vis_dat()



percapita_gdp_gni <- gdppercapita_ppp_inflation_adjusted %>%
  pivot_longer(cols = !country,
               names_to = "year",
               names_transform = list(year = as.integer),
               values_to = "gni_ppp_cap") %>%
  mutate(gni_ppp_cap = readr::parse_number(gni_ppp_cap) )%>%
  mutate(gni_ppp_cap = case_when(gni_ppp_cap < 200 ~ gni_ppp_cap * 1000,
                                 TRUE ~ gni_ppp_cap) ) %>%
  filter(between(year, 1949, 2021))


save(percapita_gdp_gni, file = here::here("data", "tidy_data", "ppp_per_capita.rda" )  )




per_capita_ppp_tidy <- percapita_gdp_gni 

save(per_capita_ppp_tidy , file = here::here("data", "tidy_data", "per_capita_ppp_tidy.rda" )  )


life_expectancy_years <- read_csv("data/raw_data/life_expectancy_years.csv")


life_expectancy_years %>% vis_dat()

life_expectancy_tidy <-  life_expectancy_years %>%
  pivot_longer(cols = !country,
               names_to = "year",
               names_transform = list(year = as.integer),
               values_to = "life_exp") %>%
  filter(between(year, 1949, 2021))



setdiff(life_expectancy_tidy$country, world_map2_ISO$country)







load(file = here::here("data", "tidy_data", "EU_Map2.rda"))


EU_list <- EU_Map2 %>% select(name, brk_a3 ) %>% 
  tibble() 





world_map2_ISO_names <- world_map2_ISO %>% 
  select(code_3, country)

EU_list <- EU_list %>% 
  left_join(world_map2_ISO_names,
            by = c("brk_a3" = "code_3"))

EU_list <- EU_list %>% distinct(name,brk_a3, country)

EU_Life_EXP <- life_expectancy_tidy %>% 
  filter(country %in% EU_list$country)


EU_percapita <- percapita_gdp_gni %>% 
  filter(country %in% EU_list$country)




EU_Key_Data <- EU_Life_EXP 
EU_Key_Data$ppp_cap <- EU_percapita$gni_ppp_cap


#EU_Key_Data %>% mutate(iso_3a = case_when(country == EU_list$country ~ EU_list$iso_a3,
  #                                        TRUE ~ NA_character_) )



EU_Key_Data <- EU_Key_Data %>% left_join(EU_list, by = "country")


load("~/R_STUDIO/Misc_Sub/data/tidy_data/nato_nations.rda")

nato_nations$brk_a3 <- nato_nations$iso_a3 

nato_nationsm <- nato_nations %>%
  select(-country)

# BLR

nato_consider <- c("BLR", "UKR", nato_nationsm$brk_a3)

NATO_Key_Data <- EU_Key_Data %>% 
  filter(brk_a3 %in% nato_consider)

NATO_core <- c("BLR", "RUS","UKR", 
              "FRA", "DEU", 
              "GBR", "ITA", "ESP")

NATO_Key_Data 

CORE_NATO_data <- NATO_Key_Data  %>% 
  filter(brk_a3 %in% NATO_core )


breaks_years <- seq(1948, 2022, by = 8)

save(CORE_NATO_data, file = here::here("data", 
                                       "tidy_data", 
                                       "CORE_NATO_data.rda") )

life_exp_chart <-  CORE_NATO_data %>%
  mutate(country = fct_reorder(country,  life_exp)) %>%
  ggplot( aes(x = year, y = life_exp,
              color = country) ) +
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = breaks_years) + 
  theme_light() +
  labs( fill = "Nation", title = "Life Expectancy: 1949 to 2020",
  subtitle = "Major EU NATO Allies vs. Russia & Belarus",
        x = "Year", y = "Life Expectancy", color = "Nation",
  caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 2004, y = 63, label = "Russia") +
  annotate("text", x = 2004, y = 72, label = "Belarus") +
  geom_hline(yintercept = 75, lty = 3, color = "red")


life_exp_chart 

life_exp_chart2 <-  CORE_NATO_data %>%
  mutate(country = fct_reorder(country,  life_exp)) %>%
  ggplot( aes(x = year, y = life_exp,
              color = country) ) +
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_continuous(breaks = breaks_years) + 
  theme_light() +
  labs( fill = "Nation", title = "Life Expectancy: 1949 to 2020",
        subtitle = "Major EU NATO Allies vs. Russia & Belarus",
        x = "Year", y = "Life Expectancy", color = "Nation",
        caption = "Data Humanist, CC0 (Public Domain)" ) +
  geom_hline(yintercept = 75, lty = 3, color = "red")

life_exp_chart2

ppp_chart <- CORE_NATO_data %>%
  mutate(country = fct_reorder(country,  ppp_cap)) %>%
  ggplot( aes(x = year, y = ppp_cap,color = country) ) +
  geom_point( alpha = 0.5) +
  scale_x_continuous(breaks = breaks_years) + 
  geom_line() +
  theme_light() +
  labs( fill = "Nation", title = "PPP Per Capita GDP: 1949 to 2020",
        subtitle = "Major EU NATO Allies vs. Russia & Belarus",
        x = "Year", y = "PPP Per Capita GDP", color = "Nation",       
        caption = "Data Humanist, CC0 (Public Domain)" ) +
  annotate("text", x = 1998, y = 18000, label = "Russia") +
  annotate("text", x = 2008, y = 10000, label = "Belarus") +
  geom_hline(yintercept = 30000, lty = 3, color = "red")



ppp_chart 


ppp_chart2 <- CORE_NATO_data %>%
  mutate(country = fct_reorder(country,  ppp_cap)) %>%
  ggplot( aes(x = year, y = ppp_cap,color = country) ) +
  geom_point( alpha = 0.5) +
  scale_x_continuous(breaks = breaks_years) + 
  geom_line() +
  theme_light() +
  labs( fill = "Nation", title = "PPP Per Capita GDP: 1949 to 2020",
        subtitle = "Major EU NATO Allies vs. Russia & Belarus",
        x = "Year", y = "PPP Per Capita GDP", color = "Nation",       
        caption = "Data Humanist, CC0 (Public Domain)" ) +
  geom_hline(yintercept = 30000, lty = 3, color = "red")

plotly::ggplotly(ppp_chart2 )





 
## Major power 324.75


pop_count <- read_csv("data/raw_data/pop_count.txt")


break_pop <- seq(0, 160, by = 20)


pop_chart <- pop_count %>% 
  mutate(nation = fct_reorder(nation, status)) %>% 
  ggplot( aes(x = nation, y = population, fill = status)) +
  geom_col() +
  scale_fill_manual(values = c("red", "darkblue") ) +
  guides(fill = "none") +
  labs(title = "Population Count (2020)",
       x = "Nation", y = "Population in Millions",
       subtitle = "Major EU NATO Allies vs. Russia & Belarus",      
       caption = "Data Humanist, CC0 (Public Domain)" ) +
  theme_minimal() +
  scale_y_continuous(breaks = break_pop)

pop_chart 

use_pop <- pop_chart +  annotate("text", 
                      x = "Spain", 
                      y = 132, label = "RED: ~ 154 million people",
                      color = "red")  +
  annotate("text", 
           x = "Spain", 
                        y = 118, label = "BLUE: ~ 363 million people",
                        color = "darkblue") 

save(use_pop, file = here::here("data", "tidy_data", "use_pop.rd") )

pop_here <- pop_count %>%
  group_by(status) %>%
  summarize(each = sum(population))

pop_here 



pop_chart 


# Save --------------------------------------------------------------------

life_exp_chart2 

pop_chart 

ppp_chart2

pop_here 

three_more <- c("life_exp_chart2" , "pop_chart", "ppp_chart2", "pop_here")

save(list = three_more, file = here::here("data", "tidy_data", "three_more_dash.rda"))