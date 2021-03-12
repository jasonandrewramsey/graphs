# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, gganimate, magick)

# Data Load ---------------------------------------------------------------
player_info <- readRDS('./data/player_info.rds')
offense_grades <- readRDS('./data/offense_grades.rds')
game_taxonomy <- readRDS('./data/game_taxonomy.rds')


# Data Prep ---------------------------------------------------------------
# in scope data frame (since 2010)
ol_df <-
game_taxonomy %>%
  mutate(game_id = id) %>%
  filter(season > 2010) %>%
  dplyr::select(game_id, season) %>%
  distinct() %>%
  inner_join(
    offense_grades %>%
      filter(position %in% c('LT', 'LG', 'C', 'RG', 'RT')) %>%
      dplyr::select(player_id, game_id, franchise_id, position, status, off_snap_count_total) %>%
      distinct(),
    by = 'game_id'
  )

# determine player's position for each season
ol_pos <-
  ol_df %>%
  group_by(player_id, position, season) %>%
  summarise(
    total_snaps = sum(off_snap_count_total)
  ) %>%
  as.data.frame %>%
  group_by(player_id, season) %>%
  arrange(desc(total_snaps)) %>%
  summarise_all(first) %>%
  as.data.frame

# determine the player's team for each season
ol_team <-
  ol_df %>%
  group_by(player_id, franchise_id, season) %>%
  summarise(
    total_snaps = sum(off_snap_count_total)
  ) %>%
  as.data.frame %>%
  group_by(player_id, season) %>%
  arrange(desc(total_snaps)) %>%
  summarise_all(first) %>%
  as.data.frame

# join for ol_pos + ol_team and player_info for ol_meta
ol_meta <-
  ol_pos %>%
  dplyr::select(player_id, season, position) %>%
  left_join(
    ol_team %>% 
      dplyr::select(player_id, season, franchise_id),
    by = c('player_id', 'season')
    ) %>%
  left_join(
    player_info %>%
      mutate(player_id = id) %>%
      dplyr::select(player_id, first_name, last_name, 
                    draft_round, draft_selection, draft_season, 
                    draft_team_id) %>%
      mutate(draft_round = ifelse(is.na(draft_round), 'UDFA', paste(draft_round))) %>%
      left_join(
        data.frame(
          draft_round = c(seq(from = 1, to = 7, by = 1) %>% paste, 'UDFA'),
          grouped_draft_round = c('1st Rnd', rep('2nd-4th Rnd', 3), rep('5th-7th Rnd', 3), 'UDFA'),
          stringsAsFactors = F
        ),
        by = 'draft_round'
      ),
    by = 'player_id'
  ) %>%
  mutate(acquisition_type = ifelse(franchise_id == draft_team_id, 'Draft', 'Trade/FA'))

# graph data table
ol_graph_data <-
ol_df %>%
  group_by(status, player_id, season) %>%
  mutate(
    game_cnt = n(),
            snap_cnt = sum(off_snap_count_total)
    ) %>%
  filter(status == 'S',
         game_cnt > 1) %>%
  as.data.frame %>%
  dplyr::select(player_id, season) %>%
  distinct() %>%
  left_join(
    ol_meta,
    by = c('player_id', 'season')
  ) %>%
  group_by(position, grouped_draft_round, season) %>%
  summarise(
    player_count = n()
  ) %>%
  as.data.frame %>%
  group_by(position, season) %>%
  mutate(percent_players = player_count/sum(player_count)) %>%
  as.data.frame %>%
  mutate(position = factor(position, levels = c('LT', 'LG', 'C', 'RG', 'RT')),
         label_pct = scales::percent(percent_players, accuracy = 2))



# graph -------------------------------------------------------------------

p <-
  ggplot(ol_graph_data, aes(x = position, y = percent_players, fill = grouped_draft_round, label = label_pct)) +
  geom_col(colour = "gray40") +
  geom_text(position = position_stack(vjust = 0.5), 
            #colour = "#3B3B3B", fontface = "bold"
            ) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limit = c(0, 1.1)) +
  scale_fill_manual(values = c('darkslategray4', 'lightcyan3', 'steelblue', 'skyblue4')) +
  labs(title = "{current_frame}: What round the {current_frame} NFL Offensive Line Starter's were drafted by Position",
       subtitle = "Percent of starting OL by position and draft round",
       caption = "All players who started at least one game in 2020\nPosition is determined by the position the player played the majority of snaps in the season") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", color = "#3B3B3B"),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#3B3B3B"),
        plot.subtitle = element_text(color = "#3B3B3B", size = 9),
        plot.caption = element_text(color = "#3B3B3B"),
        legend.text = element_text(color = "#3B3B3B", size = 9),
        legend.title = element_blank(),
        legend.position = c(0.7, .96),
        legend.direction = "horizontal") +
    #transition_time(season)
  transition_manual(season)


animate(p, 
        duration = 30, 
        nframes = 24, 
        renderer = magick_renderer())

anim_save("./ol_draft_rnd.gif", animation = last_animation())
