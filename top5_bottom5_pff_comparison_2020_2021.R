## !diagnostics off
options(dplyr.summarise.inform = FALSE)
setwd('H:/sports/nfl')

# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rvest, httr, jsonlite, zoo, reshape2, lubridate, rtweet)

# Load Taxonomy tables
taxonomy_tables <- 
  list(
    game_taxonomy = readRDS('./data_preprocess/data/pff/reference_data/game_taxonomy.rds'),
    team_taxonomy = readRDS('./data_preprocess/data/pff/reference_data/team_taxonomy.rds')
  ) %>%
  map(., ~filter(., season >= 2006))

## Offense grades
off_df <-
  readRDS('./data_preprocess/data/pff/player_stats/offense.rds') %>%
  mutate(game_id = str_replace_all(file_path, c('./data_extract/data/pff/player_stats/offense/offense/'='', '.rds'=''))) %>%
  dplyr::select(player_id, game_id, franchise_id, grade=grades_offense, snaps=snap_counts_total) %>%
  distinct()

## Defense grades
def_df <-
  readRDS('./data_preprocess/data/pff/player_stats/defense.rds') %>%
  mutate(game_id = str_replace_all(file_path, c('./data_extract/data/pff/player_stats/defense/defense/'='', '.rds'=''))) %>%
  dplyr::select(player_id, game_id, franchise_id, grade=grades_defense, snaps=snap_counts_defense) %>%
  distinct()

# Combined
df <-
  list(
    off_df,
    def_df
  ) %>%
  invoke(rbind, .) %>%
  inner_join(
    taxonomy_tables$game_taxonomy %>%
      dplyr::select(game_id, week, season) %>%
      mutate(game_id = paste(game_id)),
    by = 'game_id'
  )


# Tmp structure function
compute.grds <- function(.data) {
  .data %>%
    group_by(player_id) %>%
    mutate(total_snaps = sum(snaps)) %>%
    as.data.frame %>%
    mutate(pct = snaps/total_snaps) %>%
    group_by(player_id) %>%
    summarise(
      n = n(),
      snaps = sum(snaps),
      wavg_grade = sum(pct*grade)
    ) %>%
    as.data.frame
}

# Weighted Avg PFF Grade 2020
grade_2020 <-
  df %>%
  filter(season == 2020,
         week <=4,
         week >0) %>%
  compute.grds()

# Weighted Avg PFF Grade 2021
grade_2021 <-
  df %>%
  filter(season == 2021,
         week <=4,
         week >0) %>%
  compute.grds()

# Filter out snap count threshold (aggregate mean)
# Merge with player reference data
grades_full <-
  grade_2020 %>%
  as.data.frame %>%
  filter(snaps >= mean(snaps)) %>%
  inner_join(
    grade_2021 %>%
      as.data.frame %>%
      filter(snaps >= mean(snaps)),
    by = 'player_id',
    suffix = c('_2020', '_2021')
  ) %>%
  left_join(
    readRDS('./data_preprocess/data/pff/reference_data/player_taxonomy.rds') %>% 
      dplyr::select(last_name, first_name, player_id, position),
    by = 'player_id'
  ) %>%
  rowwise() %>%
  mutate(name = paste0(str_sub(first_name, 1, 1), '. ', last_name)) %>%
  as.data.frame %>%
  mutate(delta = wavg_grade_2021-wavg_grade_2020)

# Join keys
keys <- c('player_id', 'position', 'name', 'rnk')

# Prep is different for increase/decrease
incr_grades_prep <-
  grades_full %>%
  group_by(position) %>%
  arrange(desc(delta)) %>%
  mutate(rnk = row_number()) %>%
  as.data.frame %>%
  filter(rnk <= 5) %>%
  dplyr::select(player_id, position, name, rnk, primary=wavg_grade_2020, inc=delta) %>%
  melt(., keys) 

decr_grades_prep <-
  grades_full %>%
  group_by(position) %>%
  arrange(delta) %>%
  mutate(rnk = row_number()) %>%
  as.data.frame %>%
  filter(rnk <= 5) %>%
  dplyr::select(player_id, position, name, rnk, primary=wavg_grade_2020, dec=delta) %>%
  melt(., keys) 

grades_prep <-
  list(
    incr_grades_prep,
    decr_grades_prep
  ) %>%
  invoke(rbind, .) %>%
  mutate(player_id = paste(player_id)) %>%
  filter(position != 'FB')

# Graphing frame
graph_df <-
  grades_prep %>%
  left_join(
    grades_prep %>%
      filter(variable != 'primary') %>%
      group_by_at(keys) %>%
      summarise(diff = sum(value)) %>%
      as.data.frame,
    by = keys
  ) %>%
  group_by_at(keys) %>%
  mutate(total = sum(value)) %>%
  as.data.frame %>%
  split(., .$position) %>%
  map(., ~mutate(., name = paste(name)) %>%
        arrange(desc(diff)) %>%
        mutate(player_id = factor(player_id, levels = unique(.$player_id)))) %>%
  invoke(rbind, .) %>%
  rowwise() %>%
  mutate(value_label = 
           ifelse(variable == 'inc', paste0('+', scales::number(value, accuracy = .1)),
                  ifelse(variable == 'dec', scales::number(value, accuracy = .1), '')),
         value_label_pos = ifelse(variable == 'primary', 0, ifelse(variable == 'inc', total+4, diff*1.175))) %>%
  as.data.frame


## Add name position (should be done earlier but im lazy)
graph_df <- 
  graph_df %>%
  left_join(
    graph_df %>%
      group_by_at(keys) %>%
      summarise(mn = min(value),
                mx = max(value)) %>%
      as.data.frame %>%
      mutate(direction = ifelse(mn <= 0, 'dec', 'inc')) %>%
      dplyr::select(-mn),
    by = keys) %>%
  mutate(name_pos = ifelse(direction == 'dec', mx, total),
         incr_name = ifelse(direction == 'inc', name, ''),
         decr_name = ifelse(direction != 'sup', name, ''))







graph_df %>%
  mutate(variable = factor(variable, c('inc', 'dec', 'primary'))) %>%
  ggplot() +
  geom_col(aes(x = player_id, y = value, fill = variable), position = "stack", colour = 'azure4') +
  
  # primary label
  geom_text(aes(x = player_id, y = 0, label = scales::number(total, accuracy = .01)),
            vjust = -1, size = 3) +
  
  # name label -- incr
  #geom_text(aes(x = player_id, y = name_pos, label = incr_name),
  #size = 3, angle = 315, hjust = -.1) +
  geom_text(aes(x = player_id, y = 90, label = decr_name),
            size = 3, angle = 45, hjust = .1, vjust = -1) +
  
  # Increase/Decrease Label
  geom_text(aes(x = player_id, y = value_label_pos, label = value_label),
            size = 3) +
  geom_hline(yintercept = 0) +
  
  scale_fill_manual(values = c('deepskyblue2', 'firebrick2', 'gainsboro')) +
  scale_y_continuous(limits = c(-43, 140)) +
  facet_wrap(~position, scales = "free_x", ncol = 3) +
  labs(title = '2021 vs. 2020 - Change in PFF Grades',
       subtitle = 'Comparing Snapcount Weighted Average PFF Grades until Week 4 of the Respective NFL Season') +
  guides(fill = F) +
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



ggsave(filename = 'graphic.png', width = 20, height = 20)

