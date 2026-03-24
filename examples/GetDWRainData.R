library(tidyverse)

dwairstrip <- 014626 # From 1960 - 1970 & from 2000, use 014626 dwairstrip
dwtown <- 014618 # From 1889 - 1960 & 1970 - 2000, use 014618 dwtown
email <- "mickwelli@hotmail.com"

start_town_1 <- 18890101
end_town_1 <- 19591231

start_air_1 <- 19600101
end_air_1 <- 19691231

start_town_2 <- 19700101
end_town_2 <- 19991231

# end day is today
today_formatted <- format(Sys.Date(), "%Y%m%d")

start_air_2 <- 20000101
end_air_2 <- today_formatted

# 1. Get town period 1 (1889 - 1960)

town_1_url <- paste0(
  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?",
  "start=", start_town_1,
  "&finish=", end_town_1,
  "&station=", dwtown,
  "&format=alldata&username=", email
)

# Read the first 100 lines as text
all_lines <- readLines(town_1_url, n = 100)

# Find the line index where the actual data starts.
skip_value <- grep("^[0-9]{8}", all_lines)[1] - 3

# We skip the header and use 'fill = TRUE' because of the way whitespace is handled
df_town_1 <- read.table(town_1_url, skip = skip_value, header = TRUE, fill = TRUE)[-1,] %>%
  select("Date", "Rain")

# 2. Get airstrip period 1 (1960 - 1970)

air_1_url <- paste0(
  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?",
  "start=", start_air_1,
  "&finish=", end_air_1,
  "&station=", dwairstrip,
  "&format=alldata&username=", email
)

# Read the first 100 lines as text
all_lines <- readLines(air_1_url, n = 100)

# Find the line index where the actual data starts.
skip_value <- grep("^[0-9]{8}", all_lines)[1] - 3

# We skip the header and use 'fill = TRUE' because of the way whitespace is handled
df_air_1 <- read.table(air_1_url, skip = skip_value, header = TRUE, fill = TRUE)[-1,] %>%
  select("Date", "Rain")

# 3. Get town period 2 (1970 - 2000)

town_2_url <- paste0(
  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?",
  "start=", start_town_2,
  "&finish=", end_town_2,
  "&station=", dwtown,
  "&format=alldata&username=", email
)

# Read the first 100 lines as text
all_lines <- readLines(town_2_url, n = 100)

# Find the line index where the actual data starts.
skip_value <- grep("^[0-9]{8}", all_lines)[1] - 3

# We skip the header and use 'fill = TRUE' because of the way whitespace is handled
df_town_2 <- read.table(town_2_url, skip = skip_value, header = TRUE, fill = TRUE)[-1,] %>%
  select("Date", "Rain")

# 4. Get airstrip period 2 (2000 - 2026)

air_2_url <- paste0(
  "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?",
  "start=", start_air_2,
  "&finish=", end_air_2,
  "&station=", dwairstrip,
  "&format=alldata&username=", email
)

# Read the first 100 lines as text
all_lines <- readLines(air_2_url, n = 100)

# Find the line index where the actual data starts.
skip_value <- grep("^[0-9]{8}", all_lines)[1] - 3

# We skip the header and use 'fill = TRUE' because of the way whitespace is handled
df_air_2 <- read.table(air_2_url, skip = skip_value, header = TRUE, fill = TRUE)[-1,] %>%
  select("Date", "Rain")

# 5. Combine each dataset
df <- bind_rows(df_town_1, df_air_1, df_town_2, df_air_2)

# Convert the date column to a proper Date object
df$Date <- as.Date(as.character(df$Date), format = "%Y%m%d")

# Data missingness
# 1. Create the complete sequence of dates
all_dates <- seq(min(df$Date), max(df$Date), by = "day")

# 2. Check if the lengths match
if (length(all_dates) == nrow(df)) {
  message("Sequence is perfect. No days missing.")
} else {
  missing_count <- length(all_dates) - nrow(df)
  message("Warning: ", missing_count, " days are missing from the sequence.")
}

df <- df %>%
  mutate(
    Year  = year(Date),
    Month = month(Date),
    Day   = day(Date)
  )

df %>% saveRDS('data/Rain_DW.rds')
