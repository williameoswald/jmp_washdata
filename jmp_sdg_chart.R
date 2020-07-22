# 22 July 2020
# William Oswald
# Script creates JMP style figures for water, sanitation, and hygiene coverage
# 2017 data available from washdata.org

# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("ggnewscale", "tidyverse", "here") # package names
pacman::p_load(pkgs, character.only = TRUE)

devtools::install_github("reconhub/linelist")
library(linelist)

#Specify country using ISO3
country <- "KEN"

washdata <- read_csv(here("data","washdata_2017.csv")) %>% 
  filter(ISO3 %in% country) %>% 
  clean_variable_names() %>% 
  mutate(iso3=NULL, year=NULL, residence_type=NULL, population=NULL,
  servicecat=
    case_when(
      service_type=="Drinking water" ~ 1,
      service_type=="Sanitation" ~ 2,
      service_type=="Hygiene" ~ 3),
  levelcat=
    case_when(
      service_type=="Drinking water" & service_level=="Safely managed" ~ 5,
      service_type=="Drinking water" & service_level=="At least basic" ~ 4,
      service_type=="Drinking water" & service_level=="Limited service" ~ 3,
      service_type=="Drinking water" & service_level=="Unimproved" ~ 2,
      service_type=="Drinking water" & service_level=="Surface water" ~ 1,
      service_type=="Sanitation" & service_level=="Safely managed" ~ 5,
      service_type=="Sanitation" & service_level=="At least basic" ~ 4,
      service_type=="Sanitation" & service_level=="Limited service" ~ 3,
      service_type=="Sanitation" & service_level=="Unimproved" ~ 2,
      service_type=="Sanitation" & service_level=="Open defecation" ~ 1,
      service_type=="Hygiene" & service_level=="Basic service" ~ 3,
      service_type=="Hygiene" & service_level=="Limited service" ~ 2,
      service_type=="Hygiene" & service_level=="No handwashing facility" ~ 1))

w_washdata <- washdata %>%
  arrange(levelcat) %>%
  filter(service_type=="Drinking water") %>%
  mutate(levelcat=factor(levelcat, levels=c("1","2","3","4","5"),
                         labels=c("Surface water","Unimproved","Limited","Basic","Safely managed"),
                         ordered=TRUE))
s_washdata <- washdata %>%
  arrange(levelcat) %>%
  filter(service_type=="Sanitation") %>%
  mutate(levelcat=factor(levelcat, levels=c("1","2","3","4","5"),
                         labels=c("Open defecation","Unimproved","Limited","Basic","Safely managed"),
                         ordered=TRUE))
h_washdata <- washdata %>%
  arrange(levelcat) %>%
  filter(service_type=="Hygiene") %>%
  mutate(levelcat=factor(levelcat, levels=c("1","2","3"),
                         labels=c("No facility","Limited","Basic"),
                         ordered=TRUE)) 

w_pal <- c("#ffb300", "#ffd54e", "#fff076", "#50c2f7", "#0388d1")
s_pal <- c("#ffb300", "#ffd54e", "#fff076", "#81c783", "#378e3c")
h_pal <- c("#ffca26", "#fff076", "#ab47bb")

wash_sdg <- 
  ggplot(NULL, aes(x=servicecat, y=coverage)) +
  geom_bar(data=w_washdata, position="fill", stat="identity", width=0.5, 
           aes(fill=levelcat)) +
  scale_color_manual(name="Drinking water",
                     values=w_pal,
                     aesthetics = "fill") +
  new_scale_fill() +
  geom_bar(data=s_washdata, position="fill", stat="identity", width=0.5, 
           aes(fill=levelcat)) +
  scale_color_manual(name="Sanitation",
                     values=s_pal,
                     aesthetics = "fill") +
  new_scale_fill() +
  geom_bar(data=h_washdata, position="fill", stat="identity", width=0.5,
           aes(fill=levelcat)) +
  scale_color_manual(name="Hygiene",
                     values=h_pal,
                     aesthetics = "fill") +
  scale_y_continuous(labels = scales::percent_format(suffix=NULL), expand = c(0,0), n.breaks=6) +
  scale_x_continuous(labels = c("1"="Drinking water", "2"="Sanitation", "3"="Hygiene"), n.breaks=3) +
  theme(
    axis.line.x = element_line(size=0.25, colour="gray"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_line(size=0),
    axis.text.y = element_text(size=11),
    axis.text.x = element_text(size=11, face=1, vjust=1),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.grid.major.y = element_line(size=0.25, colour="gray"), 
    panel.grid.major.x = element_blank()) +
    labs(y="Coverage (%)", x=NULL) 
wash_sdg
  
  