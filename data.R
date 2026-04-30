library(tidyverse)
library(wehoop) # women's basketball data
library(tidyverse)
library(jsonlite) # to read in data from Torvik
library(stringi) # to remove accents from player names
library(hexbin) # for hexagonal shot charts


# create player stats for all games and vs tournament/top 100 teams
# load in player box score data from the last 5 women's college basketball seasons (2022 through 2026)
player_box_scores <- load_wbb_player_box(seasons = 2022:2026)

# read in USC vs. Washington game on 12-07-25 (for some reason not included in the player box scores)
#usc_vs_uw <- espn_wbb_player_box(401825227)

# read in Notre Dame vs. Ohio State game on 03-23-26 (for some reason not included in the player box scores)
#nd_vs_osu <- espn_wbb_player_box(401856547)

# read in Syracuse vs. UConn game on 03-23-26 (for some reason not included in the player box scores)
#syr_vs_uconn <- espn_wbb_player_box(401856551)

# read in San Francisco vs. Oregon State game on 01-31-26 (for some reason not included in the player box scores)
#sf_vs_ore_st <- espn_wbb_player_box(401828853)

# read in Oregon State vs. Pacific game on 02-14-26 (for some reason not included in the player box scores)
#ore_st_vs_pacific <- espn_wbb_player_box(401828872)

#player_box_scores <- rbind(player_box_scores, usc_vs_uw, nd_vs_osu, syr_vs_uconn, sf_vs_ore_st, ore_st_vs_pacific)

player_stats_total <- player_box_scores |> 
  mutate(across(c(starter, minutes, field_goals_made, field_goals_attempted, three_point_field_goals_made, three_point_field_goals_attempted, free_throws_made, free_throws_attempted, points, rebounds, offensive_rebounds, defensive_rebounds, assists, steals, blocks, turnovers, fouls), ~ replace_na(.x, 0))) |>
  group_by(season, athlete_id) |> 
  summarize(
    team_id = last(team_id),
    athlete_display_name = last(athlete_display_name),
    athlete_jersey = last(athlete_jersey),
    athlete_position_abbreviation = last(athlete_position_abbreviation),
    athlete_position_name = last(athlete_position_name),
    athlete_headshot_href = last(athlete_headshot_href),
    team_display_name = last(team_display_name),
    team_name = last(team_name),
    team_location = last(team_location),
    team_logo = last(team_logo),
    team_abbreviation = last(team_abbreviation),
    team_color = last(team_color),
    team_alternate_color = last(team_alternate_color),
    GP = sum(did_not_play == FALSE),
    GS = sum(starter),
    MIN = sum(minutes),
    FGM = sum(field_goals_made),
    FGA = sum(field_goals_attempted),
    FGM3 = sum(three_point_field_goals_made),
    FGA3 = sum(three_point_field_goals_attempted),
    FTM = sum(free_throws_made),
    FTA = sum(free_throws_attempted),
    PTS = sum(points),
    REB = sum(rebounds),
    OREB = sum(offensive_rebounds),
    DREB = sum(defensive_rebounds),
    AST = sum(assists),
    STL = sum(steals),
    BLK = sum(blocks),
    TO = sum(turnovers),
    FLS = sum(fouls),
    .groups = "drop"
  )

player_stats_total <- player_stats_total |> 
  filter(MIN > 0) |> 
  mutate(
    MIN_pg = (MIN/GP) |> round(digits = 1),
    FGM_pg = (FGM/GP) |> round(digits = 1),
    FGA_pg = (FGA/GP) |> round(digits = 1),
    FGM3_pg = (FGM3/GP) |> round(digits = 1),
    FGA3_pg = (FGA3/GP) |> round(digits = 1),
    FTM_pg = (FTM/GP) |> round(digits = 1),
    FTA_pg = (FTA/GP) |> round(digits = 1),
    PTS_pg = (PTS/GP) |> round(digits = 1),
    REB_pg = (REB/GP) |> round(digits = 1),
    OREB_pg = (OREB/GP) |> round(digits = 1),
    DREB_pg = (DREB/GP) |> round(digits = 1),
    AST_pg = (AST/GP) |> round(digits = 1),
    STL_pg = (STL/GP) |> round(digits = 1),
    BLK_pg = (BLK/GP) |> round(digits = 1),
    TO_pg = (TO/GP) |> round(digits = 1),
    FLS_pg = (FLS/GP) |> round(digits = 1),
    FG_per = (FGM/FGA) |> round(digits = 3),
    FG3_per = (FGM3/FGA3) |> round(digits = 3),
    FT_per = (FTM/FTA) |> round(digits = 3),
    EFG_per = ((FGM + 0.5*FGM3)/FGA) |> round(digits = 3),
    AST_TO_ratio = (AST/TO) |> round(digits = 3),
    FGM2 = FGM - FGM3,
    FGA2 = FGA - FGA3,
    FGM2_pg = (FGM2/GP) |> round(1),
    FGA2_pg = (FGA2/GP) |> round(1),
    FG2_per = (FGM2/FGA2) |> round(3)
  ) |> 
  mutate(across(c(FG_per, FG3_per, FT_per, EFG_per, AST_TO_ratio), ~ replace_na(.x, 0)))

# create vector of tournament teams for each of the last 5 seasons
tournament_teams_2026 <- c("UConn", "UTSA", "Richmond", "Stephen F. Austin", "Samford", "Arizona State", "Iowa State", "Syracuse", "Maryland", "Murray State", "North Carolina", "Western Illinois", "Notre Dame", "Fairfield", "Ohio State", "Howard", "Illinois", "Colorado", "Vanderbilt", "High Point", "South Carolina", "Southern", "Clemson", "USC", "Michigan State", "Colorado State", "Oklahoma", "Idaho", "Washington", "South Dakota State", "TCU", "UC San Diego", "Georgia", "Virginia", "Iowa", "Fairleigh Dickinson", "UCLA", "California Baptist", "Oklahoma State", "Princeton", "Ole Miss", "Gonzaga", "Minnesota", "Green Bay", "Baylor", "Nebraska", "Duke", "Charleston", "Texas Tech", "Villanova", "LSU", "Jacksonville", "Texas", "Missouri State", "Oregon", "Virginia Tech", "Kentucky", "James Madison", "West Virginia", "Miami (OH)", "Alabama", "Rhode Island", "Louisville", "Vermont", "NC State", "Tennessee", "Michigan", "Holy Cross")

tournament_teams_2025 <- c("UC San Diego", "Southern", "Columbia", "Washington", "Iowa State", "Princeton", "High Point", "William & Mary", "UCLA", "Richmond", "Georgia Tech", "Ole Miss", "Ball State", "Baylor", "Grand Canyon", "Florida State", "George Mason", "LSU", "San Diego State", "Michigan State", "Harvard", "NC State", "Vermont", "South Carolina", "Tennessee Tech", "Utah", "Indiana", "Alabama", "Green Bay", "Maryland", "Norfolk State", "West Virginia", "North Carolina", "Oregon State", "Vanderbilt", "Oregon", "Duke", "Lehigh", "Texas", "Illinois", "Creighton", "Tennessee", "South Florida", "Ohio State", "Montana State", "Michigan", "Notre Dame", "Stephen F. Austin", "Louisville", "Nebraska", "TCU", "Fairleigh Dickinson", "USC", "UNC Greensboro", "California", "Mississippi State", "Kansas State", "Fairfield", "Kentucky", "Liberty", "Iowa", "Murray State", "Oklahoma", "Florida Gulf Coast", "Oklahoma State", "South Dakota State", "UConn", "Arkansas State")

tournament_teams_2024 <- c("Sacred Heart", "Presbyterian", "Holy Cross", "UT Martin", "Auburn", "Arizona", "Vanderbilt", "Columbia", "Texas", "Drexel", "Alabama", "Florida State", "Utah", "South Dakota State", "Gonzaga", "UC Irvine", "Tennessee", "Green Bay", "NC State", "Chattanooga", "Iowa State", "Maryland", "Stanford", "Norfolk State", "South Carolina", "North Carolina", "Michigan State", "Oklahoma", "Florida Gulf Coast", "Indiana", "Fairfield", "Nebraska", "Texas A&M", "Oregon State", "Eastern Washington", "Ole Miss", "Marquette", "Notre Dame", "Kent State", "Iowa", "West Virginia", "Princeton", "Colorado", "Drake", "Kansas State", "Portland", "Louisville", "Middle Tennessee", "LSU", "Rice", "Creighton", "UNLV", "UCLA", "California Baptist", "USC", "Texas A&M-Corpus Christi", "Kansas", "Michigan", "Baylor", "Virginia Tech", "Marshall", "Syracuse", "UConn", "Jackson State", "Duke", "Richmond", "Ohio State", "Maine")

tournament_teams_2023 <- c("Illinois", "Mississippi State", "Tennessee Tech", "Monmouth", "Purdue", "St. John's", "Southern", "Sacred Heart", "South Carolina", "Norfolk State", "South Florida", "Marquette", "Oklahoma", "Portland", "UCLA", "Sacramento State", "Creighton", "Notre Dame", "Southern Utah", "Arizona", "West Virginia", "Maryland", "Holy Cross", "Stanford", "Ole Miss", "Gonzaga", "Louisville", "Drake", "Texas", "East Carolina", "Colorado", "Middle Tennessee", "Duke", "Iona", "Florida State", "Georgia", "Iowa", "SE Louisiana", "Indiana", "Oklahoma State", "Miami", "Washington State", "Florida Gulf Coast", "Villanova", "Cleveland State", "Michigan", "UNLV", "LSU", "Hawai'i", "NC State", "Princeton", "Utah", "Gardner-Webb", "Virginia Tech", "Chattanooga", "USC", "South Dakota State", "Iowa State", "Toledo", "Tennessee", "Saint Louis", "North Carolina", "Ohio State", "James Madison", "Baylor", "Alabama", "UConn", "Vermont")

tournament_teams_2022 <- c("Dayton", "DePaul", "Howard", "Incarnate Word", "Longwood", "Mount St. Mary's", "Missouri State", "Florida State", "South Carolina", "Miami", "South Florida", "North Carolina", "Stephen F. Austin", "Arizona", "UNLV", "Georgia", "Iowa State", "UT Arlington", "Colorado", "Creighton", "Iowa", "Illinois State", "Louisville", "UAlbany", "Nebraska", "Gonzaga", "Oregon", "Belmont", "Tennessee", "Buffalo", "BYU", "Villanova", "Michigan", "American", "Ole Miss", "South Dakota", "Baylor", "Hawai'i", "Stanford", "Montana State", "Kansas", "Georgia Tech", "Virginia Tech", "Florida Gulf Coast", "Maryland", "Delaware", "Ohio State", "LSU", "Jackson State", "Utah", "Arkansas", "Texas", "Fairfield", "NC State", "Washington State", "Kansas State", "Notre Dame", "Massachusetts", "Oklahoma", "IU Indianapolis", "Kentucky", "Princeton", "Indiana", "Charlotte", "UCF", "Florida", "UConn", "Mercer")


# create player stats in games vs tournament teams
player_stats_vs_tournament <- player_box_scores |> 
  filter(
    (opponent_team_location %in% tournament_teams_2026 & season == 2026) | 
    (opponent_team_location %in% tournament_teams_2025 & season == 2025) | 
    (opponent_team_location %in% tournament_teams_2024 & season == 2024) | 
    (opponent_team_location %in% tournament_teams_2023 & season == 2023) | 
    (opponent_team_location %in% tournament_teams_2022 & season == 2022)
  ) |> 
  mutate(across(c(starter, minutes, field_goals_made, field_goals_attempted, three_point_field_goals_made, three_point_field_goals_attempted, free_throws_made, free_throws_attempted, points, rebounds, offensive_rebounds, defensive_rebounds, assists, steals, blocks, turnovers, fouls), ~ replace_na(.x, 0))) |>
  group_by(season, athlete_id) |> 
  summarize(
    team_id = last(team_id),
    athlete_display_name = last(athlete_display_name),
    athlete_jersey = last(athlete_jersey),
    athlete_position_abbreviation = last(athlete_position_abbreviation),
    athlete_position_name = last(athlete_position_name),
    athlete_headshot_href = last(athlete_headshot_href),
    team_display_name = last(team_display_name),
    team_name = last(team_name),
    team_location = last(team_location),
    team_logo = last(team_logo),
    team_abbreviation = last(team_abbreviation),
    team_color = last(team_color),
    team_alternate_color = last(team_alternate_color),
    GP = sum(minutes > 0),
    GS = sum(starter),
    MIN = sum(minutes),
    FGM = sum(field_goals_made),
    FGA = sum(field_goals_attempted),
    FGM3 = sum(three_point_field_goals_made),
    FGA3 = sum(three_point_field_goals_attempted),
    FTM = sum(free_throws_made),
    FTA = sum(free_throws_attempted),
    PTS = sum(points),
    REB = sum(rebounds),
    OREB = sum(offensive_rebounds),
    DREB = sum(defensive_rebounds),
    AST = sum(assists),
    STL = sum(steals),
    BLK = sum(blocks),
    TO = sum(turnovers),
    FLS = sum(fouls),
    .groups = "drop"
  )

player_stats_vs_tournament <- player_stats_vs_tournament |> 
  filter(MIN > 0) |> 
  mutate(
    MIN_pg = (MIN/GP) |> round(digits = 1),
    FGM_pg = (FGM/GP) |> round(digits = 1),
    FGA_pg = (FGA/GP) |> round(digits = 1),
    FGM3_pg = (FGM3/GP) |> round(digits = 1),
    FGA3_pg = (FGA3/GP) |> round(digits = 1),
    FTM_pg = (FTM/GP) |> round(digits = 1),
    FTA_pg = (FTA/GP) |> round(digits = 1),
    PTS_pg = (PTS/GP) |> round(digits = 1),
    REB_pg = (REB/GP) |> round(digits = 1),
    OREB_pg = (OREB/GP) |> round(digits = 1),
    DREB_pg = (DREB/GP) |> round(digits = 1),
    AST_pg = (AST/GP) |> round(digits = 1),
    STL_pg = (STL/GP) |> round(digits = 1),
    BLK_pg = (BLK/GP) |> round(digits = 1),
    TO_pg = (TO/GP) |> round(digits = 1),
    FLS_pg = (FLS/GP) |> round(digits = 1),
    FG_per = (FGM/FGA) |> round(digits = 3),
    FG3_per = (FGM3/FGA3) |> round(digits = 3),
    FT_per = (FTM/FTA) |> round(digits = 3),
    EFG_per = ((FGM + 0.5*FGM3)/FGA) |> round(digits = 3),
    AST_TO_ratio = (AST/TO) |> round(digits = 3),
    FGM2 = FGM - FGM3,
    FGA2 = FGA - FGA3,
    FGM2_pg = (FGM2/GP) |> round(1),
    FGA2_pg = (FGA2/GP) |> round(1),
    FG2_per = (FGM2/FGA2) |> round(3)
  ) |> 
  mutate(across(c(FG_per, FG3_per, FT_per, EFG_per, AST_TO_ratio), ~ replace_na(.x, 0)))


# create empty data frames that will be filled with Torvik data
json_tibble_total <- tibble()
json_tibble_top_100_total <- tibble()

# get data from Bart Torvik for season from 2021-22 through 2025-26
for (i in 2022:2026){
  # save women's advanceed players stats from barttovrik.com (in JSON format) as a url and read them in
  json_file <- paste0("https://barttorvik.com/ncaaw/getadvstats.php?year=", i)
  json_data <- jsonlite::fromJSON(json_file, flatten = TRUE)
  
  # convert the data to a tibble
  json_tibble <- as_tibble(json_data)
  
  # get Torvik data just for players vs top 100 opponents
  json_file_top_100 <- paste0("https://barttorvik.com/ncaaw/pslice.php?year=", i, "&top=100")
  json_data_top_100 <- jsonlite::fromJSON(json_file_top_100, flatten = TRUE)
  
  # convert the data to a tibble
  json_tibble_top_100 <- as_tibble(json_data_top_100)
  
  # combine data from different seasons into a single tibble
  json_tibble_total <- rbind(json_tibble_total, json_tibble)
  json_tibble_top_100_total <- rbind(json_tibble_top_100_total, json_tibble_top_100)
}

# rename the variables
torvik_player_tibble <- json_tibble_total |> 
  rename(
    "player_name" = "V1",
    "team" = "V2",
    "conference" = "V3",
    "games_played" = "V4",
    "min_percent" = "V5",
    "off_rtg" = "V6",
    "usage" = "V7",
    "efg_per" = "V8",
    "ts_per" = "V9",
    "or_per" = "V10",
    "dr_per" = "V11",
    "ast_per" = "V12",
    "to_per" = "V13",
    "ft_made" = "V14",
    "ft_att" = "V15",
    "ft_per" = "V16",
    "fgm2" = "V17",
    "fga2" = "V18",
    "fg2_per" = "V19",
    "fgm3" = "V20",
    "fga3" = "V21",
    "fg3_per" = "V22",
    "blk_per" = "V23",
    "stl_per" = "V24",
    "ft_rate" = "V25",
    "class" = "V26",
    "height" = "V27",
    "jersey" = "V28",
    "porpagatu" = "V29",
    "adj_oe" = "V30",
    "fouls_per_40" = "V31",
    "season" = "V32",
    "player_id" = "V33",
    "hometown" = "V34",
    "recruiT_rank" = "V35",
    "ast_to_ratio" = "V36",
    "rim_fgm" = "V37",
    "rim_fga" = "V38",
    "non_rim2_fgm" = "V39",
    "non_rim2_fga" = "V40",
    "rim_fg_per" = "V41",
    "non_rim2_fg_per" = "V42",
    "dunk_fgm" = "V43",
    "dunk_fga" = "V44",
    
    
    "def_rtg_unadjusted" = "V47",
    "def_rtg" = "V48",
    "d_porpagatu" = "V49",
    "stops_maybe" = "V50",
    "old_BPM_torvik" = "V51",
    "old_OBPM_torvik" = "V52",
    "old_DBPM_torvik" = "V53",
    "BPM_torvik" = "V54",
    "min_pg" = "V55",
    "OBPM_torvik" = "V56",
    "DBPM_torvik" = "V57",
    "or_pg" = "V58",
    "dr_pg" = "V59",
    "reb_pg" = "V60",
    "ast_pg" = "V61",
    "stl_pg" = "V62",
    "blk_pg" = "V63",
    "pts_pg" = "V64",
    "role" = "V65",
    "fga3_per_100_poss" = "V66"
  ) |> 
  mutate(
    across(c(4:25, 29:32, 35:64, 66), as.numeric),
    total_prpg = porpagatu + d_porpagatu,
    fga = fga2 + fga3,
    fgm = fgm2 + fgm3,
    fg_per = (fgm/fga) |> round(3)
  ) |> 
  select(-c(V45, V46))

torvik_player_tibble <- torvik_player_tibble |> 
  mutate(
    team = ifelse(team |> str_ends("St."), str_replace_all(team, "St.", "State"), team)
  )


# rename the variables for vs top 100 opponents
torvik_players_vs_top_100 <- json_tibble_top_100_total |> 
  rename(
    "player_name" = "V1",
    "team" = "V2",
    "conference" = "V3",
    "games_played" = "V4",
    "min_percent" = "V5",
    "off_rtg" = "V6",
    "usage" = "V7",
    "efg_per" = "V8",
    "ts_per" = "V9",
    "or_per" = "V10",
    "dr_per" = "V11",
    "ast_per" = "V12",
    "to_per" = "V13",
    "ft_made" = "V14",
    "ft_att" = "V15",
    "ft_per" = "V16",
    "fgm2" = "V17",
    "fga2" = "V18",
    "fg2_per" = "V19",
    "fgm3" = "V20",
    "fga3" = "V21",
    "fg3_per" = "V22",
    "blk_per" = "V23",
    "stl_per" = "V24",
    "ft_rate" = "V25",
    "class" = "V26",
    "height" = "V27",
    "jersey" = "V28",
    "porpagatu" = "V29",
    "adj_oe" = "V30",
    "fouls_per_40" = "V31",
    "season" = "V32",
    "player_id" = "V33",
    
    "recruiT_rank" = "V35",
    "ast_to_ratio" = "V36",
    "rim_fgm" = "V37",
    "rim_fga" = "V38",
    "non_rim2_fgm" = "V39",
    "non_rim2_fga" = "V40",
    "rim_fg_per" = "V41",
    "non_rim2_fg_per" = "V42",
    "dunk_fgm" = "V43",
    "dunk_fga" = "V44",
    
    
    "def_rtg_unadjusted" = "V47",
    "def_rtg" = "V48",
    "d_porpagatu" = "V49",
    "stops_maybe" = "V50",
    "old_BPM_torvik" = "V51",
    "old_OBPM_torvik" = "V52",
    "old_DBPM_torvik" = "V53",
    "BPM_torvik" = "V54",
    "min_pg" = "V55",
    "OBPM_torvik" = "V56",
    "DBPM_torvik" = "V57",
    "or_pg" = "V58",
    "dr_pg" = "V59",
    "reb_pg" = "V60",
    "ast_pg" = "V61",
    "stl_pg" = "V62",
    "blk_pg" = "V63",
    "pts_pg" = "V64",
    "role" = "V65",
    "fga3_per_100_poss" = "V66"
  ) |> 
  mutate(
    across(c(4:25, 29:32, 35:64, 66), as.numeric),
    total_prpg = porpagatu + d_porpagatu,
    fga = fga2 + fga3,
    fgm = fgm2 + fgm3,
    fg_per = (fgm/fga) |> round(3)
  ) |> 
  select(-c(V45, V46))

torvik_players_vs_top_100 <- torvik_players_vs_top_100 |> 
  mutate(
    team = ifelse(team |> str_ends("St."), str_replace_all(team, "St.", "State"), team)
  )

# define differences in team names between ESPN and Torvik
team_aliases_torvik <- tribble(
  ~torvik_name,                           ~espn_name,
  "Cal St. Northridge",                   "Cal State Northridge",
  "Cal St. Fullerton",                    "Cal State Fullerton",
  "Cal St. Bakersfield",                  "Cal State Bakersfield",
  "UMKC",                                 "Kansas City",
  "Nicholls State",                       "Nicholls",
  "McNeese State",                        "McNeese",
  "N.C. State",                           "NC State",
  "Arkansas Pine Bluff",                  "Arkansas-Pine Bluff",
  "Albany",                               "UAlbany",
  "Bethune Cookman",                      "Bethune-Cookman",
  "Texas A&M Corpus Chris",               "Texas A&M-Corpus Christi",
  "Mississippi",                          "Ole Miss",
  "Connecticut",                          "UConn",
  "Gardner Webb",                         "Gardner-Webb",
  "Grambling State",                      "Grambling",
  "Hawaii",                               "Hawai'i",
  "Illinois Chicago",                     "UIC",
  "LIU",                                  "Long Island University",
  "Louisiana Monroe",                     "UL Monroe",
  "Miami FL",                             "Miami",
  "Nebraska Omaha",                       "Omaha",
  "USC Upstate",                          "South Carolina Upstate",
  "Southeastern Louisiana",               "SE Louisiana",
  "Southeast Missouri",                   "Southeast Missouri State",
  "Stephen F Austin",                     "Stephen F. Austin",
  "Sam Houston State",                    "Sam Houston",
  "San Jose State",                       "San José State",
  "Seattle",                              "Seattle U",
  "Tennessee Martin",                     "UT Martin",
  "American",                             "American University",
  "Appalachian State",                    "App State",
  "Cal Baptist",                          "California Baptist",
  "FIU",                                  "Florida International",
  "IU Indy",                              "IU Indianapolis",
  "Loyola MD",                            "Loyola Maryland",
  "Miami OH",                             "Miami (OH)",
  "Penn",                                 "Pennsylvania",
  "Queens",                               "Queens University",
  "St. Thomas",                           "St. Thomas-Minnesota"
)

# clean the team names for the Torvik data
torvik_player_tibble <- torvik_player_tibble |>
  left_join(team_aliases_torvik, by = c("team" = "torvik_name")) |>
  mutate(team = coalesce(espn_name, team)) |>
  select(-espn_name)

torvik_players_vs_top_100 <- torvik_players_vs_top_100 |>
  left_join(team_aliases_torvik, by = c("team" = "torvik_name")) |>
  mutate(team = coalesce(espn_name, team)) |>
  select(-espn_name)

d1_teams <- as.vector(torvik_player_tibble$team)

player_stats_total <- player_stats_total |> 
  filter(team_location %in% d1_teams)

player_stats_vs_tournament <- player_stats_vs_tournament |> 
  filter(team_location %in% d1_teams)

# save the player names in a tibble with the IDs to merge back 
save_player_names <- player_stats_total |> 
  select(athlete_id, athlete_display_name, season)

save_player_names_vs_good_teams <- player_stats_vs_tournament |> 
  select(athlete_id, athlete_display_name, season)

# remove apostrophes, hyphens, accents, and Jrs/Srs from player names in the data from ESPN
player_stats_total <- player_stats_total |> 
  mutate(
    athlete_display_name = athlete_display_name |>
      str_remove_all("[-']") |>
      stri_trans_general("Latin-ASCII") |> 
      str_to_title()
  ) |> 
  mutate(
    athlete_display_name = case_when(
      athlete_display_name |> str_ends(", Jr.") ~ athlete_display_name |> str_replace_all(", Jr.", ""),
      athlete_display_name |> str_ends(" Jr.") ~ athlete_display_name |> str_replace_all(" Jr.", ""),
      athlete_display_name |> str_ends(", Sr.") ~ athlete_display_name |> str_replace_all(", Sr.", ""),
      athlete_display_name |> str_ends(" Sr.") ~ athlete_display_name |> str_replace_all(" Sr.", ""),
      .default = athlete_display_name
    )
  )

player_stats_vs_tournament <- player_stats_vs_tournament |> 
  mutate(
    athlete_display_name = athlete_display_name |>
      str_remove_all("[-']") |>
      stri_trans_general("Latin-ASCII") |> 
      str_to_title()
  ) |> 
  mutate(
    athlete_display_name = case_when(
      athlete_display_name |> str_ends(", Jr.") ~ athlete_display_name |> str_replace_all(", Jr.", ""),
      athlete_display_name |> str_ends(" Jr.") ~ athlete_display_name |> str_replace_all(" Jr.", ""),
      athlete_display_name |> str_ends(", Sr.") ~ athlete_display_name |> str_replace_all(", Sr.", ""),
      athlete_display_name |> str_ends(" Sr.") ~ athlete_display_name |> str_replace_all(" Sr.", ""),
      .default = athlete_display_name
    )
  )

# remove apostrophes, hyphens, and accents from player names in the Torvik data
torvik_player_tibble <- torvik_player_tibble |> 
  mutate(
    player_name = player_name |>
      str_remove_all("[-']") |>
      stri_trans_general("Latin-ASCII") |> 
      str_to_title()
  ) |> 
  mutate(
    player_name = case_when(
      player_name |> str_ends(", Jr.") ~ player_name |> str_replace_all(", Jr.", ""),
      player_name |> str_ends(" Jr.") ~ player_name |> str_replace_all(" Jr.", ""),
      player_name |> str_ends(", Sr.") ~ player_name |> str_replace_all(", Sr.", ""),
      player_name |> str_ends(" Sr.") ~ player_name |> str_replace_all(" Sr.", ""),
      .default = player_name
    )
  )

torvik_players_vs_top_100 <- torvik_players_vs_top_100 |> 
  mutate(
    player_name = player_name |>
      str_remove_all("[-']") |>
      stri_trans_general("Latin-ASCII") |> 
      str_to_title()
  ) |> 
  mutate(
    player_name = case_when(
      player_name |> str_ends(", Jr.") ~ player_name |> str_replace_all(", Jr.", ""),
      player_name |> str_ends(" Jr.") ~ player_name |> str_replace_all(" Jr.", ""),
      player_name |> str_ends(", Sr.") ~ player_name |> str_replace_all(", Sr.", ""),
      player_name |> str_ends(" Sr.") ~ player_name |> str_replace_all(" Sr.", ""),
      .default = player_name
    )
  )

# combine box score stats with stats from Torvik
player_stats_total <- player_stats_total |> 
  left_join(torvik_player_tibble, by = c("team_location" = "team",
                                         "athlete_display_name" = "player_name",
                                         "season" = "season",
                                         "athlete_jersey" = "jersey")) 

# convert athlete_display_name to "player"
player_stats_total <- player_stats_total |> 
  rename("player" = "athlete_display_name")

# go back to original player names
player_stats_total <- save_player_names |> 
  left_join(player_stats_total, by = c("athlete_id", "season")) |> 
  select(-player)

# combine box score stats with stats from Torvik for good teams
player_stats_vs_tournament <- player_stats_vs_tournament |> 
  left_join(torvik_players_vs_top_100, by = c("team_location" = "team",
                                              "athlete_display_name" = "player_name",
                                              "season" = "season",
                                              "athlete_jersey" = "jersey")) 

# save the last observation to handle many-to-many joins (there were three) except for Kaiden Glenn, who the first observation is saved for
player_stats_vs_tournament <- player_stats_vs_tournament |>
  group_by(athlete_id, season) |>
  slice(if (first(athlete_display_name) == "Kaiden Glenn" & first(season) == 2025) 1 else n()) |>
  ungroup()

# convert athlete_display_name to "player"
player_stats_vs_tournament <- player_stats_vs_tournament |> 
  rename("player" = "athlete_display_name")

# go back to original player names
player_stats_vs_tournament <- save_player_names_vs_good_teams |> 
  left_join(player_stats_vs_tournament, by = c("athlete_id", "season")) |> 
  select(-player)


# create variable of a player's class for the upcoming 2027 season
player_stats_total <- player_stats_total |> 
  mutate(
    class2027 = case_when(
      class == "Fr" & season == 2026  ~ "Sophomore",
      class == "So"  & season == 2026 ~ "Junior",
      class == "Jr"  & season == 2026 ~ "Senior",
      class == "Sr"  & season == 2026 ~ "Grad/Done",
      class == "Fr" & season == 2025  ~ "Junior",
      class == "So"  & season == 2025 ~ "Senior",
      class == "Jr"  & season == 2025 ~ "Grad/Done",
      class == "Fr"  & season == 2024 ~ "Senior",
      class == "So"  & season == 2024 ~ "Grad/Done",
      class == "--" | is.na(class)    ~ "--",
      TRUE                            ~ "Grad/Done"
    )
  )

# set a team's color to black if they do not have one
player_stats_total <- player_stats_total |> 
  mutate(
    team_color = ifelse(is.na(team_color), "000000", team_color)
  )

# set a team's alternate color to white if they do not have one
player_stats_total <- player_stats_total |> 
  mutate(
    team_alternate_color = ifelse(is.na(team_alternate_color), "ffffff", team_alternate_color)
  )

# player_stats_total |>
#  filter(team_location == "West Virginia") |>
#  select(team_color, team_alternate_color)

# for Ole Miss switch primary and alternate colors
player_stats_total <- player_stats_total |> 
  mutate(
    team_color = ifelse(team_location == "Ole Miss", "cf142b", team_color),
    team_alternate_color = ifelse(team_location == "Ole Miss", "13294b", team_alternate_color),
  )

# for Wake Forest switch primary and alternate colors
player_stats_total <- player_stats_total |> 
  mutate(
    team_color = ifelse(team_location == "Wake Forest", "2c2a29", team_color),
    team_alternate_color = ifelse(team_location == "Wake Forest", "ceb888", team_alternate_color),
  )

# for Michigan  switch primary and alternate colors
player_stats_total <- player_stats_total |> 
  mutate(
    team_color = ifelse(team_location == "Michigan", "ffcb05", team_color),
    team_alternate_color = ifelse(team_location == "Michigan", "00274c", team_alternate_color),
  )

# for West Virginia  switch primary and alternate colors
player_stats_total <- player_stats_total |> 
  mutate(
    team_color = ifelse(team_location == "West Virginia", "002855", team_color),
    team_alternate_color = ifelse(team_location == "West Virginia", "eaaa00", team_alternate_color),
  )

# fix athlete_position_name so all players are list as a Guard, Forward, or Center
player_stats_total <- player_stats_total |>
  mutate(
    athlete_position_name = case_when(
      athlete_position_name %in% c("Guard", "Point Guard", "Shooting Guard") ~ "Guard",
      athlete_position_name %in% c("Forward", "Small Forward", "Power Forward") ~ "Forward",
      athlete_position_name == "Center" ~ "Center",
      athlete_display_name == "Olivia Debortoli" & team_location == "Buffalo" & season == 2023 ~ "Forward",
      athlete_display_name == "Sharmela Reid" & team_location == "New Hampshire" & season == 2024 ~ "Forward",
      athlete_display_name == "Lauren Grover" & team_location == "Santa Clara" & season == 2025 ~ "Forward",
      athlete_display_name == "Liz Martino" & team_location == "Memphis" & season == 2025 ~ "Guard",
      athlete_display_name == "Mattie Olson" & team_location == "Montana State" & season == 2023 ~ "Guard",
      athlete_display_name == "Thalia Shepard" & team_location == "Merrimack" & season == 2024 ~ "Guard",
      TRUE ~ "Other"
    )
  )

player_stats_vs_tournament <- player_stats_vs_tournament |>
  mutate(
    athlete_position_name = case_when(
      athlete_position_name %in% c("Guard", "Point Guard", "Shooting Guard") ~ "Guard",
      athlete_position_name %in% c("Forward", "Small Forward", "Power Forward") ~ "Forward",
      athlete_position_name == "Center" ~ "Center",
      athlete_display_name == "Olivia Debortoli" & team_location == "Buffalo" & season == 2023 ~ "Forward",
      athlete_display_name == "Sharmela Reid" & team_location == "New Hampshire" & season == 2024 ~ "Forward",
      athlete_display_name == "Lauren Grover" & team_location == "Santa Clara" & season == 2025 ~ "Forward",
      athlete_display_name == "Liz Martino" & team_location == "Memphis" & season == 2025 ~ "Guard",
      athlete_display_name == "Mattie Olson" & team_location == "Montana State" & season == 2023 ~ "Guard",
      athlete_display_name == "Thalia Shepard" & team_location == "Merrimack" & season == 2024 ~ "Guard",
      TRUE ~ "Other"
    )
  )

# calculate the percentiles within positions for player stats in each season
# Stats to calculate percentiles for (per-game and rate stats make most sense)
percentile_stats_total <- c(
  "min_pg", "pts_pg", "reb_pg", "or_pg", "dr_pg","ast_pg", "stl_pg", "blk_pg", "TO_pg","fg_per", "fg3_per", "ft_per", 
  "fgm", "fga", "fgm2", "fga2", "fg2_per", "fgm3", "fga3", "ft_made", "ft_att", "efg_per",
  "rim_fgm", "rim_fga", "rim_fg_per", "non_rim2_fgm", "non_rim2_fga", "non_rim2_fg_per",
  "usage", "AST_TO_ratio", "ts_per", "dr_per", "or_per", "stl_per", "blk_per", "to_per", "fouls_per_40",
  "porpagatu", "d_porpagatu", "total_prpg", "OBPM_torvik", "DBPM_torvik", "BPM_torvik"
)

percentile_stats_vs_tournament <- c(
  "MIN_pg", "PTS_pg", "REB_pg", "OREB_pg", "DREB_pg","AST_pg", "STL_pg", "BLK_pg", "TO_pg","FG_per", "FG3_per", "FT_per",
  "FGM", "FGA", "FGM2", "FGA2", "FG2_per", "FGM3", "FGA3", "FTM", "FTA", "EFG_per", 
  "usage", "AST_TO_ratio", "efg_per", "ts_per", "dr_per", "or_per", "stl_per", "blk_per", "to_per", "fouls_per_40",
  "porpagatu", "d_porpagatu", "total_prpg", "OBPM_torvik", "DBPM_torvik", "BPM_torvik"
)

# Minimum minutes per game threshold to be included in percentile calculation
MIN_PG_THRESHOLD <- 10

# Helper: calculate percentile of a value x within vector v
# (what fraction of peers score <= x, i.e. higher = better for most stats)
calc_percentile <- function(x, v) {
  v_clean <- v[!is.na(v)]
  if (length(v_clean) == 0 || is.na(x)) return(NA_real_)
  round(mean(v_clean <= x) * 100, 1)
}

# for turnovers and fouls, LOWER is better — we'll flip those after
flip_stats <- c("TO_pg", "fouls_per_40", "to_per")

# build position-season peer groups, then calculate percentiles
player_percentiles_total <- player_stats_total |>
  filter(MIN_pg >= MIN_PG_THRESHOLD) |>
  group_by(season, athlete_position_name) |>
  mutate(
    across(
      all_of(percentile_stats_total),
      ~ {
        stat_name <- cur_column()
        col_vals <- .x  # capture .x in the current scope before map_dbl
        pctile <- map_dbl(col_vals, \(val) calc_percentile(val, col_vals))
        if (stat_name %in% flip_stats) 100 - pctile else pctile
      },
      .names = "pctile_{.col}"
    )
  ) |>
  ungroup() |>
  select(athlete_id, season, starts_with("pctile_"))

player_percentiles_vs_tournament <- player_stats_vs_tournament |>
  filter(MIN_pg >= MIN_PG_THRESHOLD) |>
  group_by(season, athlete_position_name) |>
  mutate(
    across(
      all_of(percentile_stats_vs_tournament),
      ~ {
        stat_name <- cur_column()
        col_vals <- .x  # capture .x in the current scope before map_dbl
        pctile <- map_dbl(col_vals, \(val) calc_percentile(val, col_vals))
        # Flip so that lower TO/fouls = higher (better) percentile
        if (stat_name %in% flip_stats) 100 - pctile else pctile
      },
      .names = "pctile_{.col}"
    )
  ) |>
  ungroup() |>
  select(athlete_id, season, starts_with("pctile_"))

# join percentiles back onto the main data frame
player_stats_total <- player_stats_total |>
  left_join(player_percentiles_total, by = c("athlete_id", "season"))

player_stats_vs_tournament <- player_stats_vs_tournament |>
  left_join(player_percentiles_vs_tournament, by = c("athlete_id", "season"))

# keep only last observation in cases of many-to-many joins
# player_stats_total <- player_stats_total |>
#   group_by(athlete_id, season) |>
#   slice_tail(n = 1) |>
#   ungroup()
# 
# player_stats_vs_tournament <- player_stats_vs_tournament |>
#   group_by(athlete_id, season) |>
#   slice_tail(n = 1) |>
#   ungroup()

# get shot charts
# hexagonal shot charts
# load NCAA women's basketball play-by-play data
wbb_pbp <- load_wbb_pbp(seasons = 2022:2026)

# add player names and headshots to play-by-play data
wbb_pbp <- wbb_pbp |> 
  left_join(player_box_scores, by = c(
    "game_id" = "game_id",
    "athlete_id_1" = "athlete_id", 
    "team_id" = "team_id",
    "season" = "season",
    "season_type" = "season_type",
    "game_date" = "game_date",
    "game_date_time" = "game_date_time"
  )
  )

# get just shooting plays from the play-by-play data and adjust their coordinates (if the shot coordinates are available)
wbb_shots <- wbb_pbp |> 
  filter(
    shooting_play == TRUE, 
    !(type_text %in% c("MadeFreeThrow", "MissedFreeThrow")),
    is.na(coordinate_x_raw) == FALSE,
    is.na(coordinate_y_raw) == FALSE
  ) |> 
  mutate(
    loc_x = -1*(coordinate_x_raw - 25),
    loc_y = coordinate_y_raw + 5
  ) |> 
  dplyr::select(
    team_location,
    athlete_display_name,
    text,
    loc_x,
    loc_y,
    scoring_play,
    team_logo,
    athlete_jersey,
    athlete_headshot_href,
    athlete_position_name,
    athlete_position_abbreviation,
    athlete_id_1,
    team_id,
    team_name,
    team_abbreviation,
    team_slug,
    team_color,
    opponent_team_location,
    game_date,
    season
  )

wbb_shots <- wbb_shots |>
  mutate(
    shot_distance = sqrt(loc_x^2 + loc_y^2) - 5.25,
    shot_zone_range = case_when(
      shot_distance <= 4 ~ "Restricted Area",
      shot_distance <= 8 ~ "In The Paint (Non-RA)",
      shot_distance <= 16 ~ "Mid-Range",
      shot_distance <= 22.1458 ~ "Mid-Range",
      TRUE ~ "3PT"
    ),
    shot_zone_area = case_when(
      loc_x < -8 ~ "Left Side",
      loc_x >  8 ~ "Right Side",
      TRUE       ~ "Center"
    ),
    shot_zone_area = case_when(
      shot_zone_range == "3PT" & abs(loc_x) > 22 ~ "Corner",
      TRUE ~ shot_zone_area
    ),
    shot_made_numeric = as.integer(scoring_play),
    shot_value = case_when(
      grepl("three", text) ~ 3L,
      grepl("Three", text) ~ 3L,
      TRUE                 ~ 2L
    )
  )

# get national averages in FG% from different shot zones (average in a season)
averages <- wbb_shots |> 
  group_by(
    season, 
    shot_zone_range, 
    shot_zone_area
  ) |> 
  summarize(
    fgm = sum(shot_made_numeric),
    fga = n(),
    league_pct = fgm/fga,
    .groups = "drop"
  )

# save files to run in the Shiny app
saveRDS(player_stats_total, "Data/player_stats_total.rds")
saveRDS(player_stats_vs_tournament, "Data/player_stats_vs_tournament.rds")
saveRDS(wbb_shots, "Data/wbb_shots.rds")
saveRDS(averages, "Data/averages.rds")
