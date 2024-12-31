# Install necessary packages (if not already installed)
if (!require("rvest")) install.packages("rvest")
if (!require("dplyr")) install.packages("dplyr")

# Load libraries
library(rvest)
library(dplyr)

# Specify the URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/NBA_Finals_television_ratings"

# Read the HTML content of the page
page <- read_html(url)

# Extract the table with the "Game-by-game breakdown by year (1974–present)" data
tables <- html_table(page, fill = TRUE)

# Assuming the desired table is the longest one (inspect manually if needed)
table_lengths <- sapply(tables, nrow)
nba_finals_table <- tables[[which.max(table_lengths)]]

# Inspect the first few rows of the table
head(nba_finals_table)

# Save the table as a CSV file
output_file <- "nba_finals_tv_ratings.csv"
write.csv(nba_finals_table, output_file, row.names = FALSE)

cat("Table saved as", output_file)
