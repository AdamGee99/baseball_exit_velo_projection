library(tidyverse)
library(here)
library(GGally)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggdist)
library(sn)
library(scattermore)
source(here("helper.R"))


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

# #100 batter ids with the most batted balls
# batter_ids = mlb_full %>%
#   group_by(batter) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   arrange(desc(n)) %>%
#   slice_head(n = 100) %>%
#   pull(batter)

#batters with at least 100 batted balls
batter_ids = mlb_full %>% group_by(batter) %>% summarise(n = n()) %>% filter(n >= 200) %>% pull(batter)

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
  filter(!is.na(exit_velo))  %>%  #remove missing responses
  filter(!is.na(height) & !is.na(weight)) #remove players with no height/weight


#filter for players in both 2024, 20245 seasons
ids_2024 = mlb_full %>% filter(game_year == 2024) %>% pull(batter) %>% unique()
ids_2025 = mlb_full %>% filter(game_year == 2025) %>% pull(batter) %>% unique()
ids_both = intersect(ids_2024, ids_2025)

mlb_full = mlb_full %>% filter(batter %in% ids_both)

#index ids for stan so they are 1, 2, 3, ...
batter_ids = mlb_full$batter %>% unique()
mlb_full = mlb_full %>%
  mutate(stan_batter_id = match(batter, batter_ids))

id_map = mlb_full %>% select(batter, stan_batter_id) %>% distinct()

#adding specific batter heights and weights not found here
height_weight = mlb_full %>% 
  group_by(stan_batter_id) %>% 
  slice(1) %>% 
  select(stan_batter_id, height, weight) %>% 
  mutate(height = ifelse(!is.na(height), height, case_when(
    stan_batter_id == 3 ~ 71,
    stan_batter_id == 5 ~ 75,
    stan_batter_id == 16 ~ 72,
    stan_batter_id == 21 ~ 71,
    stan_batter_id == 22 ~ 72,
    stan_batter_id == 54 ~ 72,
    stan_batter_id == 62 ~ 68,
    stan_batter_id == 64 ~ 75,
    stan_batter_id == 66 ~ 72,
    stan_batter_id == 67 ~ 71,
    stan_batter_id == 72 ~ 75,
    stan_batter_id == 77 ~ 72,
    stan_batter_id == 84 ~ 73,
    stan_batter_id == 89 ~ 73,
    stan_batter_id == 95 ~ 73,
  )),
  weight = ifelse(!is.na(weight), weight, case_when(
    stan_batter_id == 3 ~ 213,
    stan_batter_id == 5 ~ 215,
    stan_batter_id == 16 ~ 245,
    stan_batter_id == 21 ~ 184,
    stan_batter_id == 22 ~ 195,
    stan_batter_id == 54 ~ 215,
    stan_batter_id == 62 ~ 190,
    stan_batter_id == 64 ~ 228,
    stan_batter_id == 66 ~ 202,
    stan_batter_id == 67 ~ 161,
    stan_batter_id == 72 ~ 217,
    stan_batter_id == 77 ~ 205,
    stan_batter_id == 84 ~ 216,
    stan_batter_id == 89 ~ 235,
    stan_batter_id == 95 ~ 200,
  )))

#join
mlb_full = mlb_full %>% select(-c(height, weight)) %>% left_join(height_weight, by = "stan_batter_id")


#save
#write.csv(mlb_full, file = here("data", "mlb_2024_2025.csv"), row.names = FALSE)





############################################## EDA ############################################## 


mlb_full = read.csv(here("data", "mlb_2024_2025.csv"))

#100 players with the most batted balls
mlb_full$player_name %>% unique() %>% length() 

all_players = plot_exit_velo_dist(mlb_full)
all_players
#follows a skew normal distribution

ggsave(all_players, filename = here("figs", "all_player_exit_velo.png"), dpi = 600, width = 7, height = 4)

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

shohei = plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 226))
kwan = plot_exit_velo_dist(mlb_full %>% filter(stan_batter_id == 240))

ggsave(shohei, filename = here("figs", "shohei_exit_velo.png"), dpi = 600, width = 7, height = 4)
ggsave(kwan, filename = here("figs", "kwan_exit_velo.png"), dpi = 600, width = 7, height = 4)


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
#join stan batter ids
mlb_player_summary = mlb_player_summary %>% left_join(id_map, by = "batter")
  
summary(mlb_player_summary)


# height effects 
ggplot(data = mlb_full, mapping = aes(x = factor(height), y = exit_velo)) +
  geom_boxplot() + labs(x = "Height (Inches)", y = "Batted Ball Exit Velo (mph)") + theme_bw()

ggplot(data = mlb_player_summary, mapping = aes(x = factor(height), y = mean_exit_velo)) +
  geom_boxplot() + labs(x = "Height (Inches)", y = "Seasonal Mean Batted Ball Exit Velo (mph)") + theme_bw()


lm(dat = mlb_full, formula = exit_velo ~ height) %>% summary()
#significant height effect



# weight effects 
# bin by quantile for visualization
ggplot(data = mlb_full, mapping = aes(x = cut(weight, breaks = quantile(weight, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE), y = exit_velo)) +
  geom_boxplot() + labs(x = "Weight Quantile (pounds)", y = "Batted Ball Exit Velo (mph)") + theme_bw()

plot_exit_velo_dist(mlb_full %>% filter(weight <= 180), title = "Lightest Players (0-10% Quantile)")
plot_exit_velo_dist(mlb_full %>% filter(weight > 228), title = "Heaviest Players (90-100% Quantile)")

weight_exit_velo_seasonal = ggplot(data = mlb_player_summary, mapping = aes(x = cut(weight, breaks = quantile(weight, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE), y = mean_exit_velo)) +
  geom_boxplot() + labs(x = "Weight Quantile (pounds)", y = "Seasonal Mean Exit Velo (mph)") + theme_bw()
weight_exit_velo_seasonal

lm(dat = mlb_full, formula = exit_velo ~ weight) %>% summary()
#significant weight effect

ggsave(plot = weight_exit_velo_seasonal, filename = here("figs", "mean_exit_velo_v_weight.png"), dpi = 600, height = 4, width = 7)


ggplot(data = mlb_player_summary, mapping = aes(x = height, y = weight)) + 
  geom_point() + labs(x = "Height (inches)", y = "Weight (lb)") + theme_bw()
#pretty correlated
cor(mlb_player_summary$height, mlb_player_summary$weight)
#0.62


#need quite a few batters to start seeing real effects though
lm(dat = mlb_full %>% filter(stan_batter_id %in% 1:10), formula = exit_velo ~ height + weight) %>% summary()
#eg with only 10 batters the height effect is significantly negative
#with 100 its significantly positive and most likely real

#need to include wide range of heights when we subset the data to capture the effect
lm(dat = mlb_full %>% filter(stan_batter_id %in% 1:100), formula = exit_velo ~ height + weight) %>% summary()




# age effects
ggplot(data = mlb_full, mapping = aes(x = cut(age_bat, breaks = quantile(age_bat, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE), y = exit_velo)) +
  geom_boxplot() + labs(x = "Age Quantile", y = "Batted Ball Exit Velo (mph)") + theme_bw()

age_exit_velo_seasonal = ggplot(data = mlb_player_summary, mapping = aes(x = cut(age_bat, breaks = quantile(age_bat, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE), y = mean_exit_velo)) +
  geom_boxplot() + labs(x = "Age Quantile", y = "Seasonal Mean Exit Velo (mph)") + theme_bw()
age_exit_velo_seasonal

ggsave(plot = age_exit_velo_seasonal, filename = here("figs", "mean_exit_velo_v_age.png"), dpi = 600, height = 4, width = 7)

#age mostly pretty flat, no global effect
#maybe very small effect but probably insignificant

lm(dat = mlb_full, formula = exit_velo ~ age_bat) %>% summary()
lm(dat = mlb_full, formula = exit_velo ~ age_bat + I(age_bat^2)) %>% summary()
#no quadratic age effect

#slight correlation between age and weight
cor(mlb_player_summary$age_bat, mlb_player_summary$weight)
#older players generally heavier


#weight seems to have more signal and is always present even with a small amount of players


# look into player-level age effects







