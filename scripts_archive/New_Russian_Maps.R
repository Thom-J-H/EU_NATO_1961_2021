
# New maps of Russia ------------------------------------------------------

library(tidyverse)
library(here)
library(visdat)
library(glue)


# Data --------------------------------------------------------------------


#load(here::here("data", "tidy_data","names_by_region_continent.RData"))
load(here::here("data", "tidy_data","world_map2_project.rda"))
load(here::here("data", "tidy_data", "life_expectancy_tidy.rda"))
load(here::here("data", "tidy_data", "per_capita_ppp_tidy.rda"))
#
#
per_capita_ppp_tidy <- per_capita_ppp_tidy %>%
  dplyr::rename(per_cap_ppp = gni_ppp_cap)
## 1961

## 1991

## 2021



# Data Sets for mapping ---------------------------------------------------


life_1961  <- life_expectancy_tidy %>%
  filter(year == 1961) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(life_exp = NA, year = 1961 )) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))


life_1991 <- life_expectancy_tidy %>%
  filter(year == 1991) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(life_exp = NA, year = 1991 )) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))

 

life_2021 <- life_expectancy_tidy %>%
  filter(year == 2021) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(life_exp = NA, year = 2021)) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))




#  "per_cap_ppp"

ppp_1961  <- per_capita_ppp_tidy  %>%
  filter(year == 1961) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(per_cap_ppp= NA, year = 1961 )) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))


#
ppp_1991 <- per_capita_ppp_tidy %>%
  filter(year == 1991) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(per_cap_ppp= NA, year = 1991 )) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))


#
ppp_2021 <- per_capita_ppp_tidy  %>%
  filter(year == 2021) %>% 
  complete(country = world_map2_ISO$country, 
           fill = (list(per_cap_ppp = NA, year = 2021)) ) %>%
  left_join(world_map2_ISO, by = "country") %>% 
  filter(between(long, -30, 60) & between(lat, 25, 80))



# Map Bsaics --------------------------------------------------------------


breaks_x <- seq(-30, 60, by = 10)
breaks_y <- c(seq(30, 80, by = 10) )



life_1961_gg <- ggplot() + 
  geom_polygon(data = life_1961, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = life_exp),
               color = "white" ,  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c() +
  labs(fill = "Years",
     title ="European NATO Theatre: Life Expectancy in 1961",
     x = "Longitute: West to Prime Meridian (Zero) to East",
     y = "Latitude: South to Equator (Zero) to North",
     caption = "Data Humanist, CC0 (Public Domain)") +
  theme(legend.position = c(0.08, 0.3)) 




life_1961_gg_dy <- plotly::ggplotly(life_1961_gg)


sub_life_1961_gg2 <- life_1961_gg +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine") +
  annotate("text", x = 19, y = 52, label = "Poland") 


sub_life_1961_gg2 


# Let's hear it -----------------------------------------------------------


life_1991_gg <- ggplot() + 
  geom_polygon(data = life_1991, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = life_exp),
               color = "white" ,  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c() +
  labs(fill = "Years")



life_1991_gg2 <-   life_1991_gg +
  theme(legend.position = c(0.08, 0.3)) +
  labs(fill = "Years",
       title = "European NATO Theatre: Life Expectancy in 1991",
       x = "Longitute: West to Prime Meridian (Zero) to East",
       y = "Latitude: South to Equator (Zero) to North",
       caption = "Data Humanist, CC0 (Public Domain)")




life_1991_gg2_dy <- plotly::ggplotly(life_1991_gg2)


sub_life_1991_gg2 <- life_1991_gg2 +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine") +
  annotate("text", x = 19, y = 52, label = "Poland") 

sub_life_1991_gg2 

#####








life_2021_gg <- ggplot() + 
  geom_polygon(data = life_2021, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = life_exp),
               color = "white",  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c() +
  labs(fill = "Years")




life_2021_gg2 <-   life_2021_gg +
  theme(legend.position = c(0.08, 0.3)) +
  labs(fill = "Years",
       title = "European NATO Theatre: Life Expectancy in 2021",
       x = "Longitute: West to Prime Meridian (Zero) to East",
       y = "Latitude: South to Equator (Zero) to North",
       caption = "Data Humanist, CC0 (Public Domain)")


life_2021_gg2_dy <-  plotly::ggplotly(life_2021_gg2)


sub_life_2021_gg2 <- life_2021_gg2  +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia") +
  annotate("text", x = 27.8, y = 53.2, label = "Belarus") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine") +
  annotate("text", x = 19, y = 52, label = "Poland") 

sub_life_2021_gg2 



# PPP  --------------------------------------------------------------------




ppp_1961_gg <- ggplot() + 
  geom_polygon(data = ppp_1961, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = per_cap_ppp),
               color = "white",  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c(option = "B") +
  labs(fill = "PPP",
       title ="European NATO Theatre: PPP Per Capita GDP in 1961",
       x = "Longitute: West to Prime Meridian (Zero) to East",
       y = "Latitude: South to Equator (Zero) to North",
       caption = "Data Humanist, CC0 (Public Domain)") +
  theme(legend.position = c(0.08, 0.3)) 





ppp_1961_gg_dy <- plotly::ggplotly(ppp_1961_gg)


sub_ppp_1961_gg2 <- ppp_1961_gg +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia",
           color = "white") +
  annotate("text", x = 27.8, y = 53.2, 
           label = "Belarus", color = "white") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine",
           color = "white") +
  annotate("text", x = 19, y = 52, label = "Poland",
           color = "white") 


sub_ppp_1961_gg2 


# Let's hear it -----------------------------------------------------------


ppp_1991_gg <- ggplot() + 
  geom_polygon(data = ppp_1991, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = per_cap_ppp),
               color = "white",  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c(option = "B") 




ppp_1991_gg2 <-   ppp_1991_gg +
  theme(legend.position = c(0.08, 0.3)) +
  labs(fill = "PPP",
       title = "European NATO Theatre: PPP Per Capita GDP in 1991",
       x = "Longitute: West to Prime Meridian (Zero) to East",
       y = "Latitude: South to Equator (Zero) to North",
       caption = "Data Humanist, CC0 (Public Domain)")


ppp_1991_gg2 

ppp_1991_gg2_dy <-  plotly::ggplotly(ppp_1991_gg2)


sub_ppp_1991_gg2 <- ppp_1991_gg2 +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia",
           color = "white") +
  annotate("text", x = 27.8, y = 53.2, 
           label = "Belarus", color = "white") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine",
           color = "white") +
  annotate("text", x = 19, y = 52, label = "Poland",
           color = "white") 

sub_ppp_1991_gg2 

#####



ppp_2021_gg <- ggplot() + 
  geom_polygon(data = ppp_2021, 
               aes(x= long,  y= lat, group =  group, 
                   subgroup = country,fill = per_cap_ppp),
               color = "white" ,  size = 0.05) +
  scale_x_continuous(breaks = breaks_x ) +
  scale_y_continuous(breaks = breaks_y) +
  scale_fill_viridis_c(option = "B") 


ppp_2021_gg2 <-   ppp_2021_gg +
  theme(legend.position = c(0.08, 0.3)) +
  labs(fill = "PPP",
       title = "European NATO Theatre: PPP Per Capita GDP in 2021",
       x = "Longitute: West to Prime Meridian (Zero) to East",
       y = "Latitude: South to Equator (Zero) to North",
       caption = "Data Humanist, CC0 (Public Domain)")

ppp_2021_gg2 

ppp_2021_gg2_dy <- plotly::ggplotly(ppp_2021_gg2)


sub_ppp_2021_gg2 <- ppp_2021_gg2  +
  annotate("text", x = 37.618423, y = 55.751244, label = "Russia",
           color = "white") +
  annotate("text", x = 27.8, y = 53.2, 
           label = "Belarus", color = "white") +
  annotate("text", x = 31, y = 49.5, label = "Ukraine",
           color = "white") +
  annotate("text", x = 19, y = 52, label = "Poland",
           color = "white") 

sub_ppp_2021_gg2 



# Data Tables -------------------------------------------------------------

dat_ppp_1961 <- ppp_1961 %>% 
  distinct(year, country, per_cap_ppp) %>%
  na.omit() 

dat_ppp_1961 %>%
  DT::datatable()

dat_ppp_1991 <- ppp_1991 %>% 
  distinct(year, country, per_cap_ppp) %>%
  na.omit() 

dat_ppp_1991 %>%
  DT::datatable()


dat_ppp_2021  <- ppp_2021 %>% 
  distinct(year, country, per_cap_ppp) %>%
  na.omit() 

dat_ppp_2021 %>%
  DT::datatable()


dat_life_1961  <- life_1961 %>% 
  distinct(year, country, life_exp) %>%
  na.omit() 

dat_life_1961  %>%
  DT::datatable()

dat_life_1991 <- life_1991 %>% 
  distinct(year, country, life_exp) %>%
  na.omit() 

dat_life_1991 %>%
  DT::datatable()


dat_life_2021 <-  life_2021  %>%
  distinct(year, country, life_exp) %>%
  na.omit() 
  
dat_life_2021 %>%
  DT::datatable()

Comp_Dat <- dat_ppp_1961 %>% 
  bind_rows(dat_ppp_1991) %>%
  bind_rows(dat_ppp_2021) 


Comp_Dat2 <- dat_life_1961 %>% 
  bind_rows(dat_life_1991) %>%
  bind_rows(dat_life_2021)  


comp_dat_var <- left_join(Comp_Dat , Comp_Dat2,
                          by = c("year", "country")) %>%
  na.omit() 


comp_dat_var %>%
  DT::datatable()




quick_cor <- comp_dat_var %>%
  ggplot( aes(x = per_cap_ppp, 
              y = life_exp,
              label = country) )+
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 75, lty = 3, color = "red", alpha = 0.6) +
  geom_smooth( method = 'loess', se = TRUE) +
  facet_wrap(~year) +
  theme_light() +
  labs(x = "PPP Per Capita GDP", 
       y = "Life Expectancy",
       title = "60 Year Comparison: European Nato Theatre")

quick_cor 

plotly::ggplotly(quick_cor )


load(here::here("data", "tidy_data", "nato_nations.rda") )

nato_nations <- nato_nations %>% 
  add_row(country = "Belarus", 
          joined_NATO = NA, NATO = 0, iso_a3 = "BLR")

setdiff(nato_nations$country, comp_dat_var$country)

nato_nations$country <- recode(nato_nations$country, `Slovakia` = "Slovak Republic" )



setdiff(nato_nations$country, comp_dat_var$country)

## MUST set NATO as factor
nato_nations$NATO <- factor(nato_nations$NATO)

quick_cor2_dat <- comp_dat_var %>%
  filter(country %in% nato_nations$country)  %>%
  left_join(nato_nations, by = "country")

quick_cor2_dat$NATO <- recode(quick_cor2_dat$NATO, `1` = "Yes", `0` = "No")

quick_cor2_plot <-  quick_cor2_dat %>%
  ggplot( aes(x = per_cap_ppp, 
              y = life_exp,
              label = country,
              color = NATO) )+
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 75, lty = 3, 
             color = "black", alpha = 0.9) +
  scale_color_manual( values = c( "red", "darkblue" )) +
  facet_wrap(~year, scales = "free_x") +
  theme_light() +
  labs(x = "PPP Per Capita GDP", 
       y = "Life Expectancy",
       title = "Blue EU NATO Nations vs. Red Russia, Belarus, and Ukraine",
       color = "NAT0") 

quick_cor2_plot 

point_dat <- quick_cor2_dat %>% filter(country == "Russia")

quick_cor2_plot +  facet_wrap(~year, scales = "free_y") +  
  theme(legend.position = "none") +
  geom_vline(xintercept = 30000, lty = 3,
             color = "black", alpha = 0.9) +
  ggtitle("EU NATO Nations (BLUE) vs. Russia, Belarus, and Ukraine (RED)") +
  ggrepel::geom_text_repel(data = point_dat, mapping = aes(x = per_cap_ppp, 
                                           y = life_exp),
                           min.segment.length = 0 , 
                           box.padding = 0.5,size = 2)


quick_cor2_plot_dy <- quick_cor2_plot +  facet_wrap(~year, scales = "free_y") +  
  theme(legend.position = "none") +
  geom_vline(xintercept = 30000, lty = 3,
             color = "black", alpha = 0.9) +
  ggtitle("EU NATO Nations vs. Russia, Belarus, and Ukraine")   

quick_cor2_plot_dy  <- plotly::ggplotly(quick_cor2_plot_dy)

avg_stats <-  comp_dat_var %>%
  filter(country %in% nato_nations$country)  %>%
  left_join(nato_nations, by = "country") %>%
  group_by(year) %>%
  summarise(Avg_Life =mean(life_exp), Avg_PPP = mean(per_cap_ppp))

avg_stats 

three_EE <-  comp_dat_var %>%
  filter(country %in% nato_nations$country)  %>%
  left_join(nato_nations, by = "country") %>%
  filter(NATO == 0) %>%
  group_by(country, year) %>%
  select(life_exp, per_cap_ppp)


life_avg_1961 <- avg_stats$Avg_Life[1] 
life_avg_1991 <- avg_stats$Avg_Life[2] 
life_avg_2021 <- avg_stats$Avg_Life[3] 


ppp_avg_1961 <- avg_stats$Avg_PPP[1]
ppp_avg_1991  <- avg_stats$Avg_PPP[2]
ppp_avg_2021 <- avg_stats$Avg_PPP[3]

three_EE <- three_EE %>% 
  mutate(ratio_ppp =  case_when(year == 1961 ~ per_cap_ppp / ppp_avg_1961,
                                year == 1991 ~ per_cap_ppp / ppp_avg_1991,
                                year == 2021 ~ per_cap_ppp / ppp_avg_2021) )







three_EE <- three_EE %>% 
  mutate(life_diff = case_when(year == 1961 ~ life_exp - life_avg_1961,
                               year == 1991 ~ life_exp - life_avg_1991,
                               year == 2021 ~ life_exp - life_avg_2021) %>%
           round(3)) 


three_EE <- three_EE %>% 
  mutate(ppp_diff = case_when(year == 1961 ~ per_cap_ppp  - ppp_avg_1961,
                              year == 1991 ~ per_cap_ppp  - ppp_avg_1991,
                              year == 2021 ~ per_cap_ppp  - ppp_avg_2021)%>%
           round(3) ) 

three_EE %>% reactable::reactable()

comp_dat_var %>%   
  filter(country %in% nato_nations$country)  %>%
  filter( year == 2021, life_exp < 75)

# Plotly Dashboard --------------------------------------------------------

life_1961_gg_dy 
life_1991_gg_dy 
life_2021_gg2_dy 

ppp_1961_gg_dy 
ppp_1991_gg2_dy
ppp_2021_gg2_dy 


quick_cor2_dat
quick_cor2_plot_dy 

avg_stats ## NATO nations
three_EE %>% reactable::reactable()


dash_list <- c("three_EE" ,  "avg_stats" , 
               "quick_cor2_dat" ,"quick_cor2_plot_dy",
               "life_1961_gg_dy", "life_1991_gg2_dy",
               "life_2021_gg2_dy", "ppp_1961_gg_dy", 
               "ppp_1991_gg2_dy", "ppp_2021_gg2_dy")

save(list = dash_list, file = here::here("data", "tidy_data", "dash_plotly.rda"))

