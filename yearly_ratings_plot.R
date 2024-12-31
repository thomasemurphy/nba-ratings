library(tidyverse)

years <- seq(2024, 1986)

# In the table within the "Game-by-game breakdown by year (1974–present)" section
# of this wikipedia page (https://en.wikipedia.org/wiki/NBA_Finals_television_ratings)
# (columns Year...Game 7), can you give me an R-type list like c(element_1, element_2...)
# of the "Avg" column, for example 11.31, 11.64, etc

avg_viewership <- c(
  11.31, 11.64, 12.40, 9.91, 7.45, 15.14, 17.56, 20.38, 20.28, 19.94, 15.54, 17.47, 16.88, 17.39, 18.14, 14.35, 14.94, 9.29, 12.97, 12.54, 17.94, 9.86, 15.68, 19.00, 17.40, 16.01, 29.04, 25.59, 24.86, 20.10, 17.25, 27.21, 20.84, 23.91, 17.19, 21.26, 21.70, 24.12, 14.43
  )

mvps <- data.frame(
  year = c(1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995,
           1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
           2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
           2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  player = c("Larry Bird", "Magic Johnson", "James Worthy", "Joe Dumars", "Isiah Thomas",
             "Michael Jordan", "Michael Jordan", "Michael Jordan", "Hakeem Olajuwon", "Hakeem Olajuwon",
             "Michael Jordan", "Michael Jordan", "Michael Jordan", "Tim Duncan", "Shaquille O'Neal",
             "Shaquille O'Neal", "Shaquille O'Neal", "Tim Duncan", "Chauncey Billups", "Tim Duncan",
             "Dwyane Wade", "Tony Parker", "Paul Pierce", "Kobe Bryant", "Kobe Bryant",
             "Dirk Nowitzki", "LeBron James", "LeBron James", "Kawhi Leonard", "Andre Iguodala",
             "LeBron James", "Kevin Durant", "Kevin Durant", "Kawhi Leonard", "LeBron James",
             "Giannis Antetokounmpo", "Stephen Curry", "Nikola Jokić", "Jaylen Brown"),
  team = c("Boston Celtics", "Los Angeles Lakers", "Los Angeles Lakers", "Detroit Pistons", "Detroit Pistons",
           "Chicago Bulls", "Chicago Bulls", "Chicago Bulls", "Houston Rockets", "Houston Rockets",
           "Chicago Bulls", "Chicago Bulls", "Chicago Bulls", "San Antonio Spurs", "Los Angeles Lakers",
           "Los Angeles Lakers", "Los Angeles Lakers", "San Antonio Spurs", "Detroit Pistons", "San Antonio Spurs",
           "Miami Heat", "San Antonio Spurs", "Boston Celtics", "Los Angeles Lakers", "Los Angeles Lakers",
           "Dallas Mavericks", "Miami Heat", "Miami Heat", "San Antonio Spurs", "Golden State Warriors",
           "Cleveland Cavaliers", "Golden State Warriors", "Golden State Warriors", "Toronto Raptors", "Los Angeles Lakers",
           "Milwaukee Bucks", "Golden State Warriors", "Denver Nuggets", "Boston Celtics")
)


ratings <- data.frame(
  year = years,
  m_viewers = avg_viewership
)

super_bowl_ratings <- read_csv(
  'super_bowl_viewers.csv'
) %>%
  rename(c('m_viewers' = 'Viewers (millions)'))

world_series_ratings <- read_csv(
  'world_series.csv'
) %>%
  rename(c('m_viewers' = 'Viewers (millions)'))

nba_vs_nfl <- super_bowl_ratings %>%
  left_join(
    ratings,
    by = c('Year' = 'year'),
    suffix = c('_nfl', '_nba')
    ) %>%
  left_join(
    world_series_ratings,
    by = 'Year'
  ) %>%
  mutate(
    # m_viewers_nba = m_viewers_nba * 5.5,
    m_viewers_mlb = as.numeric(m_viewers)
    )

ggplot(
  data = nba_vs_nfl,
  mapping = aes(
    x = Year
  )
) +
  geom_point(
    aes(y = m_viewers_nba),
    color = '#882288'
  ) +
  geom_line(
    aes(y = m_viewers_nba),
    linetype = 2,
    color = '#bb22bb',
    size = 0.5
  ) +
  geom_point(
    aes(y = m_viewers_nfl),
    color = '#118888'
  ) +
  geom_line(
    aes(y = m_viewers_nfl),
    linetype = 2,
    color = '#11bbbb',
    size = 0.5
  ) +
  geom_point(
    aes(y = m_viewers_mlb),
    color = '#888822'
  ) +
  geom_line(
    aes(y = m_viewers_mlb),
    linetype = 2,
    color = '#bbbb22',
    size = 0.5
  ) +
  scale_y_continuous(
    name = 'millions of viewers (allegedly)',
    limits = c(0, 210)
  ) +
  scale_x_continuous(
    name = '',
    breaks = seq(1986, 2024, 2)
  ) +
  theme_bw()
