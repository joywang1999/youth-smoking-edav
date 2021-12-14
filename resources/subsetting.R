# save a subset

library(tidyverse)

data <- read_csv("data/nyts_grouped.csv", show_col_types = FALSE)

data %>% select(c(1,2,3,4,19)) %>%
  rename(Year = 1,
         Age = 2,
         Sex = 3,
         Grade = 4,
         QuitTime = 5) %>%
  mutate(radius = runif(nrow(data), 0.1, 3), theta = runif(nrow(data), 0, 2*pi),
         x = radius*cos(theta), y = radius*sin(theta)) %>%
  filter(QuitTime != "I have never smoked cigarettes, not even one or tw" &
           QuitTime != "I have never smoked cigarettes, not even one or two puffs") %>% 
  drop_na() %>% 
  write_csv("data/nyts_quittime.csv")


