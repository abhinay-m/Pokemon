setwd("./GitHub/Pokemon/")

# source("../../load_libraries.R")
# data sourcing
library('readxl')
library('readr')

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('progress') # code progress
library("plotly")

# general data manipulation
library('tidyverse') # data manipulation

# Interactivity
library('crosstalk')
library('plotly')
library('htmlwidgets')

poke_stats <- read.csv("Pokemon.csv")

poke_stats <- 
  poke_stats %>% 
  mutate(Legendary = ifelse(Legendary == "False", 0, 1))

## Generation level stats
generation_counts <-
  poke_stats %>% 
  group_by(Generation) %>% 
  summarise(total_pokemons = n_distinct(Name),
            mean_attack = mean(Attack),
            mean_defence = mean(Defense),
            mean_hp = mean(HP),
            mean_speed = mean(Speed),
            legendary_pokemons = sum(Legendary),
            types_v1 = n_distinct(Type.1),
            types_v2 = n_distinct(Type.2)
          )

## classification
## select hp, attack, defense, speed
df <-
  poke_stats %>% 
  filter(Generation == 1) %>% 
  select(Name, HP, Attack, Defense, Speed, Type.1) %>% 
  rename(type = Type.1)

df %>% 
  group_by(type) %>% 
  summarise(n_distinct(Name))

## Create a graph
g <- ggplot(data = df)  +
        geom_point(mapping = aes(x = Attack, y = Defense,  color = type)) 
  
## add title
g <- g + ggtitle("Pokemon - Attack Vs Defense")
g <- g + theme(plot.title = element_text(size = 20, 
                                         face = "bold",
                                         margin = margin(10, 0, 10, 1000),
                                         hjust = 0.5
                                         ))

## Add axis labels
g <- g + labs(x = "Attack", y = "Defense")
g <- g + theme(
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)   
              )

## Formatting Legend Title
g <- g + theme(legend.title = element_text(colour = "chocolate", 
                                           size = 16, 
                                           face = "bold"),
               legend.key = element_rect(fill = "grey"))+
          scale_color_discrete(name = "Type", palette="Set1") +
        guides(colour = guide_legend(override.aes = list(size=4)))

## Backgroud Color
g <- g + theme(plot.background = element_rect(fill = 'navajowhite1'),
               panel.background = element_rect(fill = 'navajowhite2'),
               # panel.grid.major = element_line(colour = "orange", size=2),
               panel.grid.minor = element_line(colour = "white"))

## -------------------------------------------------------------------------- ##

g <- 
  ggplot(data = df, aes(x = Attack, 
                        y = Defense))  +
  geom_point(mapping =  aes( color = type, 
                           size = HP)) +
  ggtitle("Pokemon 1st Generation") +
  labs(x = "Attack", y = "Defense") +
  theme(plot.title = element_text(size = 20, 
                                  face = "bold",
                                  margin = margin(10, 0, 10, 1000),
                                  hjust = 0.5
                                  ),
        legend.title = element_text(colour = "firebrick4", 
                                    size = 15, 
                                    face = "bold"
                                    ),
        legend.key = element_rect(fill = "skyblue2"),
        plot.background = element_rect(fill = 'skyblue1'),
        panel.background = element_rect(fill = 'skyblue1'),
        # panel.grid.major = element_line(colour = "orange", size=2),
        panel.grid.minor = element_line(colour = "white"),
        axis.title.x = element_text(color="firebrick4", vjust=-0.35, size = 15, face = "bold"),
        axis.title.y = element_text(color="firebrick4" , vjust=0.35, size = 15, face = "bold") 
        )+
  scale_color_discrete(name = "Type") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  geom_text(mapping = aes(label = ifelse(HP >= 150 | HP <= 10, Name,
                                         ifelse(Attack >= 150 | Attack <= 25, Name,
                                                ifelse(Defense >= 125 | Defense <= 25, Name, '')
                                                )
                                         )
                          ),
            nudge_y = -2
            )

ggplotly(g, dynamicTicks = TRUE, tooltip = "all", width = 1280, height = 720)

p <- plot_ly(g)

htmlwidgets::saveWidget(as_widget(g), "pokemon.html")
