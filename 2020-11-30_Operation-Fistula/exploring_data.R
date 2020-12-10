# Packages
library(tidyverse)
library(janitor)
library(patchwork)


# Import
data <- read_csv(here::here('2020-11-30_Operation-Fistula/pilot_data.csv')) %>%
  clean_names()




position <-tibble::tibble()

for(i in seq_along(x_axis)){

  tib <- tibble::tibble(x = x_axis[i], y = seq(10, 400, 20))
  position <- bind_rows(position,tib)

}


position %>%
  slice(-c(639,640)) -> position

data$x <- position$x
data$y <- position$y

data %>%
  write_csv(here::here('2020-11-30_Operation-Fistula/data.csv'))



