# Install packages ----
# remotes::install_github("GuangchuangYu/hexSticker")
# install.packages("leaflet")

# Load packages ----
library(hexSticker)
library(here)
library(palmerpenguins)
library(tidyverse)
library(leaflet)

# Create leaflet map
m <- leaflet() %>% setView(lng = -1.150000, lat = 52.950001, zoom = 12.5)
m %>% addTiles()
m %>% addProviderTiles(providers$Stamen.Toner)
p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
# Now take a screenshot

# Get some colourful dots from penguins package
penguins %>%
  ggplot(aes(x = flipper_length_mm, 
             y  = bill_length_mm,
             colour = species)) +
  geom_jitter(alpha = .8, size = 2) +
  ggplot2::scale_color_viridis_d() +
  theme_void() +
  theme(legend.position = "none") +
  ggsave(filename = here("logo/nottshc-cdu-ds-team/dots-for-nottshc-cdu-ds-team-logo.png"), 
         width = 6, 
         height = 3, 
         units = "in",
         dpi = 300)

# Not open Affinity Photo and put it all together, see file 'nottshc-cdu-ds-team-logo.afphoto'
# Not reproducible unfortunately, boooooooooooooo!
