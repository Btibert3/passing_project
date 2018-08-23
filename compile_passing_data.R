library(tidyverse)
library(readxl)

# 15-16 Season ------------------------------------------------------------

pass_1516 <- read_excel("~/Dropbox/passing_project/passing_data_code/passing_data_1516.xlsx") %>% 
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
                          substr(odd_man, nchar(odd_man), nchar(odd_man))),
         date = as.character(date)
  )


# Change NA-NA to NA in odd_man column for less typing
pass_1516[["odd_man"]] <- ifelse(pass_1516[["odd_man"]] == "NA-NA", "NA", pass_1516[["odd_man"]])

# Team logos will be drawn on my graphs using the ggimage package-------------------------------------------------
team_logo <- c("http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Ducks_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Coyotes_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Bruins_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Sabres_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/carolina.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_BlueJackets_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/calgary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/chicago.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/colorado.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Stars_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/detroit.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Oilers_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Panthers_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Kings_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Wild_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/montreal.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/newjersey.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Predators_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NY-Islanders-Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/newyorkr.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Senators_Primary.png",
               "http://www.stickpng.com/assets/images/5a4fbba3da2b4f099b95da1a.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Penguins_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Sharks_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/stlouis.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Lightning_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_MapleLeafs_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/Vancouver_Canucks.png",
               "https://upload.wikimedia.org/wikipedia/en/thumb/a/ac/Vegas_Golden_Knights_logo.svg/184px-Vegas_Golden_Knights_logo.svg.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Jets_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Capitals_Primary.png"
)



# 2016-2017 Season --------------------------------------------------------
file_list_1617 <- list.files(path = "/Users/jasonbaik/Dropbox/passing_project/passing_game_regular_season_1617",
                             full.names = TRUE)

# Import and manipulate data
# Since some games don't have column names, and this makes it hard to manipulate data, take those games out 
file_list_1617 <- file_list_1617[-c(28, 64, 258)]
file_list_1617 <- file_list_1617[-c(228, 186, 298, 119)]
file_list_1617 <- file_list_1617[-295]

pass_list_1617 <- lapply(file_list_1617, function(x) read_excel(x) %>% 
                           rename(#period = Period, 
                                  time = Time,
                                  strength = Strength,
                                  team = Team,
                                  #shooter = Shooter,
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
                                  #home_score = `Home Score State`,
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
                                                   substr(odd_man, nchar(odd_man), nchar(odd_man))),
                                  date = as.character(date) # This allows bind_rows to work
                           ) %>% 
                           select(-player_rebound, -time, -c(a1, a2, a3, game_id,
                                                                       `Home Goalies`, `PBP Time` 
                                                                       )) %>% 
                           mutate(Shooter = as.character(Shooter)))

# Bind all the dataframes in the list into one long dataframe!
pass_1617 <- bind_rows(pass_list_1617)






# 2017-2018 Season--------------------------------------------------------------------------------------
# List all the files in the folder
file_list <- list.files(path = "/Users/jasonbaik/Dropbox/passing_project/passing_game_regular_season_1718",
           full.names = TRUE)

# Import and manipulate data
# Left out "time" and "player_rebound" columns beceause they were causing errors
pass_list <- lapply(file_list, function(x) read_excel(x) %>% 
                      rename(period = Period, 
                             time = Time,
                             strength = Strength,
                             team = Team,
                             #shooter = Shooter,
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
                                              substr(odd_man, nchar(odd_man), nchar(odd_man))),
                             date = as.character(date)
                      ) %>% 
                      select(-player_rebound, -time))

# Bind all the dataframes in the list into one long dataframe!
pass_1718 <- bind_rows(pass_list)





# Data Manipulation -------------------------------------------------------

pass_1516 <- pass_1516 %>% 
  mutate(season = "first")

pass_1617 <- pass_1617 %>% 
  mutate(season = "second")

pass_1718 <- pass_1718 %>% 
  mutate(season = "third")



# Combine 1516, 1617, 1718
pass_final <- bind_rows(pass_1516, pass_1617, pass_1718)

