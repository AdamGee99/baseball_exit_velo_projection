
library(tidyverse)
library(baseballr)
library(Lahman)
library(here)

#scraping baseball savant statcast

#need to break it up into chunks since API download limit
dates = seq(ymd("2024-03-01"), ymd("2024-11-05"), by = "4 day")

for (i in 1:(length(dates) - 1)) {
  date_range_res = statcast_search(start_date = dates[i], 
                                   end_date = dates[i+1], 
                                   player_type = "batter")
  
  write.csv(date_range_res, file = here("data", "2024", paste0("savant_", i, ".csv")), row.names = FALSE)
}

#heights and weights
people = Lahman::People %>%
  select(birthYear, nameFirst, nameLast, nameGiven, weight, height, bats, throws)

write.csv(people, file = here("data", "people.csv"), row.names = FALSE)
