library(tidyverse)
library(here)
library(GGally)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggdist)
library(sn)
library(scattermore)



############################################## Import Data ############################################## 


files_2024 = list.files(here("data", "2024"))
files_2025 = list.files(here("data", "2025"))


mlb_2024 = files_2024 %>% map_dfr(~read.csv(here("data", "2024", .x)))
mlb_2025 = files_2025 %>% map_dfr(~read.csv(here("data", "2025", .x))) 

head(mlb_2024)
head(mlb_2024)


############################################## Clean ############################################## 

#join seasons
mlb_full = bind_rows(mlb_2024, mlb_2025)

#important variables
keep_cols = c("pitch_type", "game_date", "game_year", "player_name", "batter", "pitcher", "events", "game_type", "stand", "p_throws", "hit_location",
              "bb_type", "balls", "strikes", "game_year", "outs_when_up", "inning", "launch_speed", "launch_angle", "pitch_name", "bat_score", "fld_score",
              "bat_speed", "swing_length", "age_pit", "age_bat", "swing_path_tilt")

mlb_full = mlb_full %>%
  filter(type == "X") %>% #batted balls only
  select(all_of(keep_cols))

#100 batter ids with the most batted balls
batter_ids = mlb_full %>% 
  group_by(batter) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  arrange(desc(n)) %>%
  slice_head(n = 100) %>%
  pull(batter)

mlb_full = mlb_full %>% filter(batter %in% batter_ids)


#join heights and weights
people = read.csv(here("data", "people.csv")) %>%
  filter(birthYear >= 1975)


#remove duplicate names not in 2024/25 data
people = people %>% 
  filter(!(nameGiven %in% c("Joshua Lee", "Artemus Ward", "Josh Harris", "Joshua Dwayne", 
                            "William Michael", "Luis David", "Brian Nikola", "Greg Alan", "Diego",
                            "Jacob Clinton", "Matthew Edward", "Eddy", "Jose Delfin", "Jose Manuel",
                            "Jose Miguel")))

people = people %>%
  mutate(last_first_name = paste0(nameLast, ", ", nameFirst)) %>% #same format as savant
  select(last_first_name, height, weight)

#join height weight
mlb_full = mlb_full %>%
  left_join(people, by = join_by(player_name == last_first_name))

#convert characters to factor, dates to date
mlb_full = mlb_full %>% 
  mutate(game_date = ymd(game_date)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  rename(exit_velo = launch_speed) %>%
  filter(!is.na(exit_velo)) #remove missing responses


#filter for players in both 2024, 20245 seasons
ids_2024 = mlb_full %>% filter(game_year == 2024) %>% pull(batter) %>% unique()
ids_2025 = mlb_full %>% filter(game_year == 2025) %>% pull(batter) %>% unique()
ids_both = intersect(ids_2024, ids_2025)

mlb_full = mlb_full %>% filter(batter %in% ids_both)

#index ids for stan so they are 1, 2, 3, ...
batter_ids = mlb_full$batter %>% unique()
mlb_full = mlb_full %>%
  mutate(stan_batter_id = match(batter, batter_ids))


#save
#write.csv(mlb_full, file = here("data", "mlb_2024_2025.csv"), row.names = FALSE)





############################################## EDA ############################################## 


mlb_full = read.csv(here("data", "mlb_2024_2025.csv"))

#100 players with the most batted balls
mlb_full$player_name %>% unique() %>% length() 


plot_exit_velo_dist = function(df) {
  if(length(unique(df$batter)) == 1) {
    player_name = unique(df$player_name)
  } else {
      player_name = "All Players"
    }
  
  ggplot(df, mapping = aes(x = exit_velo)) +
    geom_histogram(colour = "black", fill = "orange", bins = 30) +
    labs(x = "Exit Velocity (mph)", y = "Count", title = player_name) +
    scale_x_continuous(limits = c(0, 130), n.breaks = 12) +
    theme_bw()
}

plot_exit_velo_dist(mlb_full)
#follows a skew normal distribution

#individual players
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 1))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 2))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 3))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 4))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 5))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 6))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 7))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 8))
plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 9))

#players clearly have different locations in skew normal dist
#scales look slightly different too
#skew looks mostly the same - probably different in reality but computationally would take forever to fit


#static features
static_features = c("player_name", "batter", "age_bat", "height", "weight")


#grouped summary by player
mlb_player_summary = mlb_full %>%
  group_by(batter) %>%
  mutate(mean_exit_velo = mean(exit_velo)) %>%
  slice(1) %>%
  ungroup() %>%
  select(batter, mean_exit_velo, all_of(static_features))
  

summary(mlb_player_summary)
#15 players out of 100 dont have heights and weights


# height effects 
ggplot(data = mlb_full, mapping = aes(x = factor(height), y = exit_velo)) +
  geom_boxplot() + labs(x = "Height (Inches)", y = "Batted Ball Exit Velo (mph)") + theme_bw()

ggplot(data = mlb_player_summary, mapping = aes(x = factor(height), y = mean_exit_velo)) +
  geom_boxplot() + labs(x = "Height (Inches)", y = "Seasonal Mean Batted Ball Exit Velo (mph)") + theme_bw()

#significant hieght effect



# age effects
ggplot(data = mlb_full, mapping = aes(x = factor(age_bat), y = exit_velo)) +
  geom_boxplot() + labs(x = "Age", y = "Batted Ball Exit Velo (mph)") + theme_bw()

ggplot(data = mlb_player_summary, mapping = aes(x = factor(age_bat), y = mean_exit_velo)) +
  geom_boxplot() + labs(x = "Age", y = "Seasonal Mean Batted Ball Exit Velo (mph)") + theme_bw()


#age mostly pretty flat, no global effect


# look into player-level age effects






