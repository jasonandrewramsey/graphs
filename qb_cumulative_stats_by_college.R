# !diagnostics off

# Packages & Dependencies -------------------------------------------------

# packages required for build
require(pacman)
pacman::p_load(tidyverse, httr, jsonlite, zoo, reshape2, lubridate, doSNOW, excel.link, caret)

#devtools::install_github("beanumber/teamcolors")
library(teamcolors)

# Functions ---------------------------------------------------------------

#Passer rating function
passer_rating <- function(comp, att, yds, td, int) {
  a = ((comp/att) - 0.3) * 5
  b = ((yds/att) - 3) * 0.25
  c = (td/att) * 20
  d = 2.375 - ((int/att) * 25)
  
  comb <- c(a, b, c, d)
  if(any(is.na(comb))) {
    comb[is.na(comb)] <- 0
  }
  
  if(any(comb > 2.375)) {
    comb[comb > 2.375] <- 2.375
  }
  if(any(comb < 0)) {
    comb[comb < 0] <- 0
  }
  
  (sum(comb)/6) * 100
}


# Data Load and Prep ------------------------------------------------------

# Sourced from Pro football focus
player_info <- readRDS('./player_info.rds')
# note: pff data is only consistent starting in ~2010 but the player info..
# contains player information on any player that had data since ~2010 (some big games are recorded starting in 2008)
# so the data being displayed is ** approximately ** career stats of all QB's active since >= 2010

# Sourced from Pro football reference
player_game_level_details <- 
  readRDS('./player_game_level_details.rds')
# note: necessary because PFF does not have data until ~2010


# Player Game Level Details summary
player_gld_summary <-
player_game_level_details %>%
  dplyr::select(season, week, game_file_id, pfr_team_id, Player, PASS_CMP, PASS_ATT, PASS_YDS, PASS_TDS, PASS_INT, PASS_SCK, PASS_SCK_YDS) %>%
  distinct() %>%
  group_by(Player) %>%
  summarise(
    season_strt = min(season),
    season_end = max(season),
    team_cnt = length(unique(pfr_team_id)),
    game_cnt = length(unique(game_file_id)),
    PASS_CMP = sum(PASS_CMP, na.rm = T),
    PASS_ATT = sum(PASS_ATT, na.rm = T),
    PASS_YDS = sum(PASS_YDS, na.rm = T),
    PASS_TDS = sum(PASS_TDS, na.rm = T),
    PASS_INT = sum(PASS_INT, na.rm = T),
    PASS_SCK = sum(PASS_SCK, na.rm = T),
    PASS_SCK_YDS = sum(PASS_SCK_YDS, na.rm = T)
  ) %>%
  # considering QB's that have had more than 10 pass attempts in their career
  filter(PASS_ATT > 10) %>%
  as.data.frame %>%
  mutate(full_name = Player,
         full_name_cln = full_name %>% str_to_lower() %>% str_replace_all(., c('[[:punct:]]'=''))) %>%
  dplyr::select(-Player)


# Pro football focus qb specific dataset w/ colleges
qb_info <-
player_info %>%
  filter(position == 'QB') %>%
  dplyr::select(id, first_name, last_name, college, draft_round) %>%
  distinct() %>%
  mutate(full_name = paste(first_name, last_name, sep = ' '),
         full_name_cln = full_name %>% str_to_lower() %>% str_replace_all(., c('[[:punct:]]'='')))

# Find names in pff that have no mapping key in pfr
qb_info %>%
  filter(full_name_cln %in% player_gld_summary$full_name_cln == F)
## note: its 5th rounders - undrafted so i am not considering it too much

# Merge on name (lol)
qb_stats_by_college <-
qb_info %>%
  filter(full_name_cln %in% player_gld_summary$full_name_cln) %>%
  left_join(player_gld_summary, by = 'full_name_cln') %>%
  group_by(college) %>%
  summarise(
    player_cnt = length(unique(id)),
    game_cnt = sum(game_cnt),
    PASS_CMP = sum(PASS_CMP, na.rm = T),
    PASS_ATT = sum(PASS_ATT, na.rm = T),
    PASS_YDS = sum(PASS_YDS, na.rm = T),
    PASS_TDS = sum(PASS_TDS, na.rm = T),
    PASS_INT = sum(PASS_INT, na.rm = T),
    PASS_SCK = sum(PASS_SCK, na.rm = T),
    PASS_SCK_YDS = sum(PASS_SCK_YDS, na.rm = T)
  ) %>%
  as.data.frame %>%
  filter(game_cnt > 10) %>%
  rowwise() %>%
  mutate(PASS_RATE = passer_rating(PASS_CMP, PASS_ATT, PASS_YDS, PASS_TDS, PASS_INT)) %>%
  as.data.frame %>%
  arrange(desc(PASS_RATE))


# Graph -------------------------------------------------------------------

# schools in scope 
prospectus_schools <- c('Clemson Tigers', 'North Dakota State Bison', 'Ohio State Buckeyes', 'Alabama Crimson Tide', 'Florida Gators', 'BYU Cougars')


# prep data for graph
graph_data <-
qb_stats_by_college %>%
  # trim some of it -- plot too cluttered
  filter(PASS_ATT > 452, # not biased
         game_cnt > 30) %>%
  
  left_join(
    teamcolors %>%
      filter(league == 'ncaa') %>%
      mutate(name = str_replace_all(name, c('Ohio State$'='Ohio State Buckeyes', 'BYU$'='BYU Cougars'))) %>%
      dplyr::select(college = name, mascot, primary, secondary),
    by = 'college'
  ) %>%
  arrange(PASS_RATE) %>%
  mutate(mascot = ifelse(is.na(mascot), 'dummy$', paste(mascot)),
         prospectus_school = college %in% prospectus_schools,
         college = str_replace(college, mascot, ''),
         color = ifelse(prospectus_school == F, paste('#DCDCDC'), paste(primary)),
         college = fct_reorder(college, PASS_RATE),
         college_label_prospectus = ifelse(prospectus_school == T, paste(college), ''),
         college_label_not_prospectus = ifelse(prospectus_school == T, '', paste(college)),
         label_passrate_not_prospectus = ifelse(prospectus_school == T, '',  paste(floor(PASS_RATE))),
         #label_passrate_not_prospectus = ifelse(prospectus_school == T, '', ifelse(floor(PASS_RATE) == lead(floor(PASS_RATE)), '', paste(floor(PASS_RATE)))),
         #label_passrate_not_prospectus = ifelse(is.na(label_passrate_not_prospectus), paste(floor(PASS_RATE)), paste(label_passrate_not_prospectus)),
         label_passrate_prospectus = ifelse(prospectus_school == T, paste(floor(PASS_RATE)), ''),
         label_passrate_y = 0.5)

pros_colors <- graph_data$color
names(pros_colors) <- names(graph_data$college)

graph <-
graph_data %>%
  ggplot() +
  geom_col(aes(x = college, y = PASS_RATE, fill = college), width = 0.9, position = "identity") +
  #passrate label
  geom_text(aes(x = college, y = label_passrate_y, label = label_passrate_not_prospectus), size = 2.5, hjust = 0) +
  geom_text(aes(x = college, y = label_passrate_y, label = label_passrate_prospectus), size = 2.5, hjust = 0, color = "white") +
  # college label -- non-prospectus
  geom_text(aes(x = college, y = PASS_RATE*1.01, label = college_label_not_prospectus), size = 2, 
            hjust = 0, color = '#A0A0A0') +
  # college label -- prospectus
  geom_text(aes(x = college, y = PASS_RATE*1.01, label = college_label_prospectus), size = 2.5, 
            hjust = 0) +
  
  
  scale_y_continuous(limit = c(0, 120), expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(name = "color_grp", values = pros_colors) +
  guides(fill = F) +
  
  coord_flip() +
  labs(title = 'NFL QB Rating by Player College',
       subtitle = 'Active NFL Players since 2010') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.title = element_text(family = "sans", size = 16, margin=margin(0,5,0,0)),
        plot.subtitle = element_text(family = "sans", size = 10, margin=margin(2,0,0,0)),
        )

# save
ggsave(filename = 'graphic.tiff', width = 10, height = 10, device='tiff', dpi=700, compression = "lzw")
