library(tidyverse)
library(nbaplotR)

setwd('nba-ratings')

nba_ratings_raw <- read_csv(
  'nba_finals_tv_ratings.csv'
) %>%
  rename('year' = 'Year')

games <- seq(1,7)

for (game_number in games) {
  
  new_colname <- paste0(
    'game_',
    game_number,
    '_viewers_m'
  )
  
  old_colname <- paste0(
    'Game ',
    game_number
  )
  
  this_game_millions <- nba_ratings_raw %>%
    separate(
      col = old_colname,
      sep = '\\(',
      into = c('junk1', 'this_game_junk')
    ) %>%
    separate(
      col = this_game_junk,
      sep = 'M',
      into = c(new_colname, 'junk2')
    ) %>%
    select(year, new_colname)
  
  if (game_number == 1) {
    year_vs_viewers_df <- this_game_millions
  } else {
    this_game_millions <- this_game_millions %>% select(new_colname)
    year_vs_viewers_df <- cbind(
      year_vs_viewers_df,
      this_game_millions
    )
  }
}


year_vs_viewers_df <- year_vs_viewers_df %>%
  mutate(
    across(seq(1,8),
    as.numeric
  )) %>%
  left_join(
    nba_ratings_raw %>%
      select(year, champ, runner),
    by = 'year'
  )

year_vs_viewers_df <- year_vs_viewers_df %>%
  rowwise() %>%
  mutate(
    max_viewers = max(
      c_across(
        starts_with("game_")
        ),
      na.rm = TRUE)
    ) %>%
  ungroup() %>%
  mutate(
    year_label = substr(as.character(year), 3, 4)
    ) %>%
  filter(year > 1990)

ggplot(
  year_vs_viewers_df,
  aes(
    x = year
  )
) +
  geom_bar(
    aes(
      y = max_viewers,
      fill = max_viewers
    ),
    stat = 'identity',
    width = 0.7,
    alpha = 0.6
  ) +
  geom_nba_logos(
    aes(
     team_abbr = champ,
     y = max_viewers + 2
    ),
    width = 0.028,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_nba_logos(
    aes(
      team_abbr = runner,
      y = max_viewers + 1
    ),
    width = 0.015,
    hjust = 0,
    vjust = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 1990.5,
    y = 40,
    label = 'million viewers of most-watched Finals game',
    hjust = 0,
    vjust = 0.5,
    color = '#aaaaaa',
    size = 2.7
  ) +
  scale_fill_gradient(
    low = '#cccccc',
    high = '#cc4444'
  ) +
  scale_x_continuous(
    name = '',
    limits = c(1990.5, 2024.5),
    breaks = seq(1991, 2024, 1),
    labels = rev(year_vs_viewers_df$year_label),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    name = '',
    limits = c(0, 41),
    breaks = seq(0, 40, 5),
    expand = c(0,0)
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.2
    ),
    axis.text = element_text(
      color = '#aaaaaa'
      ),
    axis.title = element_blank()
  )
