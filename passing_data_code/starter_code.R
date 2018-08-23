library(tidyverse)

# Read in data and clean data
pass <- readxl::read_excel("~/Dropbox/passing_data_code/passing_data_1516.xlsx") %>% 
  rename(period = Period, 
         time = Time,
         strength = Strength,
         team = Team,
         shooter_number = Shooter,
         shot_type = `Shot Type?`,
         a1 = A1,
         a2 = A2,
         a3 = A3, 
         a1_zone = `A1 Zone`,
         a2_zone = `A2 Zone`,
         a3_zone = `A3 Zone`,
         scoring_chance_pass = `SC?`,
         shot_on_goal = `SOG?`,
         odd_man = Oddman,
         goal = `G?`,
         player_rebound = `RB/2C`,
         rebound_shot_on_goal = `RB/2C SOG?`,
         rebound_goal = `RB/2C G?`,
         home_score = `Home Score State`,
         goalie_number = Goalie,
         game_id = `Game ID`,
         date = Date,
         home_team = `Home:`,
         away_team = `Away:`) %>% 
  mutate(scoring_chance_pass = ifelse(is.na(scoring_chance_pass), 0, 1),
         shot_on_goal = ifelse(is.na(shot_on_goal), 0, 1),
         goal =  ifelse(is.na(goal), 0, 1),
         rebound_shot_on_goal = ifelse(is.na(rebound_shot_on_goal), 0, 1),
         rebound_goal = ifelse(is.na(rebound_goal), 0, 1),
         odd_man = paste0(substr(odd_man, 1, 1), "-",
                                    substr(odd_man, nchar(odd_man), nchar(odd_man)))
  )

# Clean up NA-NA in odd_man column
pass[["odd_man"]] <- ifelse(pass[["odd_man"]] == "NA-NA", "NA", pass[["odd_man"]])

# Team logos 
team_logo <- c("http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Ducks_Primary.png")

# Repeatability of odd-man rush shots
pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  # Further divide odd_man rushes by number of advantage in men
  mutate(odd_man = ifelse(odd_man == "NA-NA", "non_odd_man",
                          ifelse(odd_man %in% c("1-0", "2-1", "3-2", "4-3"), "one_man", 
                                 ifelse(odd_man %in% c("2-0", "3-1", "4-2", "3-0"), "two_plus_man",
                                        "NA")))) %>% 
  filter(strength == "5v5") %>% 
  group_by(team, half, odd_man) %>%
  summarise(shots = n()) %>% 
  filter(!is.na(team), !is.na(half)) %>%
  spread(key = half, value = shots, fill = 0) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  ggimage::geom_image(aes(image = team_logo)) +
  geom_smooth(method = "lm") + 
  facet_wrap(~odd_man, scales = "free")

# Repeatability of shooting percentage: shots_on_goal / goal
pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5") %>% 
  group_by(team, half) %>% 
  summarise(shooting_percentage = round(sum(shot_on_goal) / sum(goal), 1)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = shooting_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") 

# Repeatability of rebound percentage: rebound_shot_on_goal / rebound_goal
pass %>% 
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5") %>% 
  group_by(team, half) %>% 
  summarise(rebound_percentage = sum(rebound_goal) / sum(rebound_shot_on_goal)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = rebound_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") 

# Repeatability of shooting percentage(shots_on_goal / goal) facetted by shot types
pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5") %>% 
  group_by(team, half, shot_type) %>% 
  summarise(shooting_percentage = round(sum(goal) / sum(shot_on_goal), 1)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = shooting_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") +
  facet_wrap(~shot_type, scales = "free",
             nrow = 4, ncol = 4)  

# TODO: I should combine some of these graphs here because there are many that are basically empty
# The letters should all be lowercase...capitalization doesn't mean anything here
pass[["shot_type"]] <- tolower(pass[["shot_type"]])

# Run it again
pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5") %>% 
  group_by(team, half, shot_type) %>% 
  summarise(shooting_percentage = round(sum(goal) / sum(shot_on_goal), 1)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = shooting_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") +
  facet_wrap(~shot_type, scales = "free",
             nrow = 4, ncol = 4)  
# Let's take out NA shot types and 2c shot types. I don't know what 2c shot types mean
pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5",
         !(shot_type %in% c("NA", "2c")),
         !is.na(shot_type)) %>% 
  group_by(team, half, shot_type) %>% 
  summarise(shooting_percentage = round(sum(goal) / sum(shot_on_goal), 1)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = shooting_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") +
  facet_wrap(~shot_type, scales = "free",
             nrow = 4, ncol = 4)  

# Let's change the labels on the facets:
shot_type_names <- list(
  "a" = "Wraparound",
  "b" = "Backhand",
  "o" = "One-Timer",
  "r" = "Rebound",
  "s" = "Slap Shot",
  "t" = "Tip",
  "w" = "Wrist/Snap Shot"
)

shot_type_labeller <- function(variable,value){
  return(shot_type_names[value])
}

pass %>%
  mutate(half = if_else(date > as.Date("2016-01-01"), "second", "first")) %>% 
  filter(strength == "5v5",
         !(shot_type %in% c("NA", "2c")),
         !is.na(shot_type)) %>% 
  group_by(team, half, shot_type) %>% 
  summarise(shooting_percentage = round(sum(goal) / sum(shot_on_goal), 1)) %>% 
  filter(!is.na(team), !is.na(half)) %>% 
  spread(key = half, value = shooting_percentage) %>% 
  ungroup %>% 
  ggplot(aes(x = first, y = second)) +
  geom_label(aes(label = team)) +
  geom_smooth(method = "lm") +
  facet_wrap(~shot_type, scales = "free",
             labeller = shot_type_labeller)

# TODO: I should make something related to the a1,a2,a3 zones.
# Record O, N, or D to indicate which zone the pass originated from. "of," "nf," and "df," are if the A1/A2/A3 passer won the faceoff. 
# "ds" and "ns" tell is the passer from the defensive or neutral zone completed a stretch pass. 
# "orr" is for passes that cross the Royal Road. "op" is for passes back to the point" "oel" is for passes from behind the end line. 
# If pass includes multiple events, i.e. a pass from below the end line back to the point or a faceoff won back to the point, mark them in alphabetical order: "oelp" or "ofp." 
# The last letter will be the lane on the ice in which the pass originates from: Left, Right, or Center ("l", "r", "c"). 
# Faceoff wins will not have anything since they are center by default. 
  
