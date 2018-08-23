suppressMessages(library(tidyverse)) ; library(readxl)

# Read in data
pass <- read_xlsx("Passing_Data.xlsx", sheet = 1)
pass_x <- pass

# Observe / Clean / Manipulate
glimpse(pass_x)
View(head(pass_x))
class(pass_x)
colnames(pass_x)
dim(pass_x)


# How many odd man rushes lead to goals? 
# How many goals are scored from odd man rushes AND rebounds?
# How many scoring chances lead to rebounds and lead to goals?
# Which zone passes (Offensive, Neutral, and Defense) lead to home plate shots?
# Do neutral zone passes OR defensive zone passes have higher shot percentage?
# Which side (left or right) zone passes are more effective?


# Which zone leads to the most scoring chance passes?-------------------------
pass_x %>% 
  rename(scoring_chance = `SC?`,
         a1_zone = `A1 Zone`,
         a2_zone = `A2 Zone`,
         a3_zone = `A3 Zone`) %>% 
  select(a1_zone, scoring_chance) %>% 
  group_by(a1_zone) %>% 
  summarise(scoring_chance_sum = n()) %>% 
  arrange(desc(scoring_chance_sum)) # Top zone: oc (Offensive Center) 
# Second zone: or (Offesnive Right)
# Third zone: ol (Offensive Left)


# How many odd man rushes lead to goals? ----------------------------------
pass_x %>% 
  select(Oddman, `G?`) %>%
  na.omit() %>%
  count()


# How many goals are scored from odd man rushes and rebounds --------------
pass_x %>% 
  group_by(Oddman, `RB/2C G?`) %>%
  summarise(n = n()) %>% 
  na.omit()

# How many scoring chances lead to rebounds and lead to goals?--------------
pass_x %>% 
  select(`SC?`, `RB/2C SOG?`, `RB/2C G?`) %>% 
  group_by(`SC?`) %>% 
  summarise(shots = count(`RB/2C SOG?`, na.rm = T),
            goals = count(`RB/2C G?`, na.rm = T)) # Throws an error!

# Which zone passes (Offensive, Neutral, and Defense) lead to home plate shots?------
pass_x %>% 
  group_by(`A1 Zone`, `A2 Zone`, `A3 Zone`, `SC?`) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  arrange(desc(n)) # I guess A1 is primary passer, A2 is secondary passer, and A3 is 
# tetiary passer? (passes leading to shot attempts)


# Do neutral zone passes OR defensive zone passes have higher shot percentage?-----

# Rename
pass_x <- pass_x %>%
  rename(scoring_chance = `SC?`, shots_on_goal = `SOG?`)
pass_x <- pass_x %>%
  rename(goal = `G?`)

pass_x$scoring_chance <- ifelse(is.na(pass_x$scoring_chance), 0, 1)
pass_x$shots_on_goal <- ifelse(is.na(pass_x$shots_on_goal), 0, 1)
pass_x$goal <- ifelse(is.na(pass_x$goal), 0, 1)

# Shooting percentage
pass_x %>% 
  group_by(`A1 Zone`) %>% 
  summarise(total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goal, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots)) %>%
  arrange(desc(shooting_percentage))



# Which side (left or right) zone passes are more effective?----------------------

# split A1 Zone/ A2 Zone/ A3 Zone on last letter to find out side (left, right, or center)
pass_x$`A1 Zone` <- pass_x$`A1 Zone` %>% 
  str_sub(start = str_length(pass_x$`A1 Zone`))

pass_x %>% 
  group_by(`A1 Zone`) %>% 
  summarise(scoring_chances = sum(scoring_chance, na.rm = T)) %>% 
  arrange(desc(scoring_chances))



# Questions to answer:
# Is team's rebound chances related to shooting percentages?
# Is a rebound percentage predictive of winning in this season / next season
# Rebound percentage (x-axis) vs Shooting percentage (y-axis) over Seasons?
# If you don't have multiple seasons, maybe split the season in first part and second parts
# Meta Analysis- How do I evaluate my evaluation? How repeatable is it over season
# Meta Analysis (Metric used to evaluate whether it is useful)
# Correlation of rebound percentage in two halfs of seasons. 


# 2018-03-06
# Are teams rebound chances related to shooting percentages? --------------

# Mutate shooting percentages by team
pass %>% 
  group_by(team) %>% 
  summarise(total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots)) %>% 
  arrange(desc(shooting_percentage))

# Rebound chances: rebound SOG
pass %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog))

# Combine the two into one dataframe
pass_rebound_shooting <- pass %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog),
            total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots)) %>% 
  select(team, rebound_chances, shooting_percentage)

# Visualization (png file attached in email)
pass_rebound_shooting %>% 
  ggplot(aes(x = rebound_chances, y = shooting_percentage)) + 
  geom_point() +
  geom_smooth() +
  labs(x = "Rebound Shots On Goal",
       y = "Shooting Percentage") +
  ggtitle("Shooting Percentage vs Rebound Shots on Goal")

# Residuals
mod_rebound_shooting <- lm(shooting_percentage ~ rebound_chances, data = pass_rebound_shooting)
pass_rebound_shooting$residual <- resid(mod_rebound_shooting)

# Plot residuals (png file attached in email)
pass_rebound_shooting %>% 
  ggplot(aes(x = rebound_chances, y = residual)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Rebound Shots On Goal",
       y = "Residual") +
  ggtitle("Residuals of \nShooting Percentage vs Rebound Shots on Goal")


# Is a rebound percentage predictive of winning in this season / next season-------------
# Need 1516 wins/loss data


# Rebound percentage (x-axis) vs Shooting percentage (y-axis) over Seasons?--------

# Divide 1516 season into 2 halves- Before All-Star Game and After All-Star Game 
# We assume the All Star game is the midpoint season (Jan 31, 2016)
first_half <- pass %>% 
  filter(as.Date(pass[["date"]]) < "2016-01-31")

second_half <- pass %>% 
  filter(as.Date(pass[["date"]]) > "2016-01-31")

# Visualization (png file attached in email)
first <- first_half %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog),
            total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots)) %>% 
  select(team, rebound_chances, shooting_percentage) %>% 
  ggplot(aes(x = rebound_chances, y = shooting_percentage)) + 
  geom_line() +
  labs(x = "Rebound Shots On Goal",
       y = "Shooting Percentage") +
  ggtitle("Shooting Percentage vs Rebound Shots on Goal \nFirst Half")

second <- second_half %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog),
            total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots)) %>% 
  select(team, rebound_chances, shooting_percentage) %>% 
  ggplot(aes(x = rebound_chances, y = shooting_percentage)) + 
  geom_line() +
  labs(x = "Rebound Shots On Goal",
       y = "Shooting Percentage") +
  ggtitle("Shooting Percentage vs Rebound Shots on Goal \nSecond Half")

# Show both graphs in one graph
gridExtra::grid.arrange(first, second)



# Correlation of rebound percentage in two halfs of seasons-------------------
first_rebound <- first_half %>% 
  group_by(team) %>%
  summarise(rebound_chances = sum(rebound_sog),
            rebound_goals = sum(rebound_goals),
            first_rebound_percentage = rebound_goals / rebound_chances) %>% 
  na.omit()

second_rebound <- second_half %>% 
  group_by(team) %>%
  summarise(rebound_chances = sum(rebound_sog),
            rebound_goals = sum(rebound_goals),
            second_rebound_percentage = rebound_goals / rebound_chances)

# Combine
rebound_percentages_two_halfs <- cbind(first_rebound,
                                       second_rebound[["second_rebound_percentage"]])

# Visualizae rebound percentages in two halfs of seasons (png file attached in email)
rebound_percentages_two_halfs %>% 
  ggplot(aes(x = first_rebound_percentage,
             y = `second_rebound[["second_rebound_percentage"]]`)) +
  geom_point() +
  geom_smooth() +
  labs(x = "First Half Rebound Percentage",
       y = "Second Half Rebound Percentage",
       title = "Second Half vs First Half Rebound Percentage")



# Which period has the most scoring chances / offensive action? -----------
# png file attached in email
pass %>% 
  group_by(period) %>% 
  summarise(sum_scoring_chance = sum(scoring_chance, na.rm = T),
            sum_shots_on_goal = sum(shots_on_goal, na.rm = T),
            sum_goals = sum(goals, na.rm = T)) %>% 
  gather(-period, key = "offense", value = "sum") %>% 
  ggplot(aes(x = period, y = sum)) +
  geom_col(aes(fill = offense), position = "dodge") +
  coord_flip() +
  labs(x = "Period",
       y = "Sum",
       fill = "Offensive Sum",
       title = "Offensive Sums per Period") +
  scale_fill_hue(labels = c("Sum of Goals", "Sum of Scoring Chances",
                            "Sum of Shots on Goal"))


# 03-07

# Repeatability of Shooting Percentage----------------------------------------------

# Find shooting percentages in first half / second half
rep_first <- first_half %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog),
            total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots))


rep_second <- second_half %>% 
  group_by(team) %>% 
  summarise(rebound_chances = sum(rebound_sog),
            total_shots = sum(shots_on_goal, na.rm = T),
            total_goals = sum(goals, na.rm = T),
            shooting_percentage = 100 * (total_goals / total_shots))

# Correlation between first half and second half
cor(x = rep_first[["shooting_percentage"]][-31], y = rep_second[["shooting_percentage"]])


# Repeatability of Rebound Percentage------------------------------------------------

first_rebound <- first_half %>% 
  group_by(team) %>%
  summarise(rebound_chances = sum(rebound_sog),
            rebound_goals = sum(rebound_goals),
            first_rebound_percentage = rebound_goals / rebound_chances) %>% 
  na.omit()

second_rebound <- second_half %>% 
  group_by(team) %>%
  summarise(rebound_chances = sum(rebound_sog),
            rebound_goals = sum(rebound_goals),
            second_rebound_percentage = rebound_goals / rebound_chances)

# Correlation between first half and second half
cor(x = first_rebound[["first_rebound_percentage"]], y = second_rebound[["second_rebound_percentage"]])


