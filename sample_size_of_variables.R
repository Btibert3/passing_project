# Number of games
pass %>% 
  group_by(team) %>% 
  summarise(num_games = length(unique(game_id))) %>% 
  ggplot(aes(x = team, y = num_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Teams",
       y = "Number of Games") +
  ggtitle("Number of Games in the Passing Data for Each Team")

# Number of shots
pass %>% 
  group_by(team) %>% 
  summarise(num_shots = sum(shot_on_goal)) %>% 
  ggplot(aes(x = team, y = num_shots)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Teams",
       y = "Number of Shots") +
  ggtitle("Number of Shots in the Passing Data for each Team")

# Number of rebound Shot on Goals
pass %>% 
  group_by(team) %>% 
  summarise(num_rebound_shot_on_goal = sum(rebound_shot_on_goal)) %>% 
  ggplot(aes(x = team, y = num_rebound_shot_on_goal)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Teams",
       y = "Number of Rebound Shot on Goals") +
  ggtitle("Number of Rebound Shot on Goals in the Passing Data for each Team")

# Number of Rebound Goals (Goal Scored from Rebounds)
pass %>% 
  group_by(team) %>% 
  summarise(num_rebound_goal = sum(rebound_goal)) %>% 
  ggplot(aes(x = team, y = num_rebound_goal)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Teams",
       y = "Number of Goals from Rebounds") +
  ggtitle("Number of Goals from Rebounds in the Passing Data for each Team")

# Number of Odd Man Rushes
pass %>% 
  mutate(odd_man = ifelse(odd_man == "NA-NA", "non_odd_man",
                          ifelse(odd_man %in% c("1-0", "2-1", "3-2", "4-3"), "one_man", 
                                 ifelse(odd_man %in% c("2-0", "3-1", "4-2", "3-0"), "two_plus_man",
                                        "NA")))) %>%  
  ggplot(aes(x = odd_man, y = ..prop.., group = 1), stat = "count") +
  geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Type of Odd Man Rush",
       y = "Percentage of Odd Man Rushes") +
  ggtitle("Distribution of Odd Man Rushes")


# Zone
pass %>% 
  mutate(a1_zone = substr(pass$a1_zone, start = nchar(pass$a1_zone), stop = nchar(pass$a1_zone)),
         a1_zone = tolower(a1_zone),
         a1_zone = ifelse(a1_zone == "c", "center_lane",
                          ifelse(a1_zone == "f", "faceoff",
                                 ifelse(a1_zone == "l", "left_lane",
                                        ifelse(a1_zone == "p", "point",
                                               ifelse(a1_zone == "r", "right_lane", NA)))))) %>% 
  ggplot(aes(x = a1_zone, y = ..prop.., group = 1), stat = "count") +
  geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Zone the Pass Originated From",
       y = "Percentages") +
  ggtitle("Distribution of Zones")
  
