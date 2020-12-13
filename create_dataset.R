library(stringr)
library(rjson)
library(plyr)

## Helper function to convert good_name to Goodname
##
column_fmt <- function(s) {
  r <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
  r <- str_replace_all(r, "_", "")
  
  return(r)
}

## Script configuration
##

# CSVs location
csv_directory <- "C:/Users/alexandra/Desktop/ΔΙΠΛΩΜΑΤΙΚΗ/alexandra/rstudio"

# Starting Date for combined values
starting_date <- as.Date("2015-08-07")

# Ending Date for combined values
ending_date   <- as.Date("2019-12-31")

# Goods list in format: <category, good>
goods <- list(
  c("cryptocurrencies", "bitcoin"),
  c("cryptocurrencies", "dash"),
  c("cryptocurrencies", "ethereum"),
  c("cryptocurrencies", "litecoin"),
  c("cryptocurrencies", "monero"),
  c("commodities", "copper"),
  c("commodities", "gold"),
  c("commodities", "oil"),
  c("commodities", "palladium"),
  c("commodities", "platinum"),
  c("commodities", "silver"),
  c("stock_exchange", "down_jones"),
  c("stock_exchange", "snp"),
  c("stock_exchange", "sse")
)

## Helper function to prepare Shanghai Stock Index which is fetched from
## https://www.investing.com/indices/shanghai-composite-historical-data
##
prepare_shanghai <- function() {
  # This is required to transform Date correctly
  Sys.setlocale("LC_TIME","English")
  
  f_original <- paste(csv_directory, "stock_exchange", "sse_original.csv", sep="/")
  f_new      <- paste(csv_directory, "stock_exchange", "sse.csv", sep="/")
  
  df         <- read.csv(file=f_original)
  df$Date    <- as.Date(df$Date, format="%b %d, %Y")
  df         <- df[order(df$Date),]
  write.csv(df, f_new, row.names=FALSE, quote=FALSE)
}

get_bitcoin_hashrate <- function(ts) {
  f       <- paste(csv_directory, "other", "bitcoin_metadata.csv", sep="/")
  df      <- read.csv(file=f)
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  df      <- df[order(df$Date),]
  df      <- df[df$Date >= starting_date & df$Date <= ending_date,]
  df      <- merge(ts, df, by="Date", all.x=TRUE)
  
  return(df$Network.Hashrate.Terahashs)
}

get_wikipedia_views <- function() {
  r       <- fromJSON(file="https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/Bitcoin/daily/2015010100/2020010100")
  df      <- ldply(r$items, data.frame)
  df$Date <- as.Date(df$timestamp, format="%Y%m%d00")
  df      <- df[df$Date >= starting_date & df$Date <= ending_date,]
  
  return(df$views)
}

## Script Code
##
prepare_shanghai()
ts            <- data.frame(Date=seq(starting_date, ending_date, by="day"))
all_df        <- data.frame(ts)
names(all_df) <- c("Date")

for (g in goods) {
  category <- g[1]
  name     <- g[2]
  
  # Read CSV
  f                                      <- paste(csv_directory, category, paste(name, ".csv", sep=""), sep="/")
  df                                     <- read.csv(file=f)
  # Get all dates between starting and ending dates
  df$Date                                <- as.Date(df$Date, format="%Y-%m-%d")
  df                                     <- df[df$Date >= starting_date & df$Date <= ending_date,]
  # Insert missing dates with empty values
  df                                     <- merge(ts, df, by="Date", all.x=TRUE)
  # Get Close price of each good and name the column after the good's name
  all_df$temp                            <- df$Close
  names(all_df)[names(all_df) == "temp"] <- column_fmt(name)
}

all_df$`Bitcoin Hashrate` <- get_bitcoin_hashrate(ts)
all_df$`Wikipedia Views`  <- get_wikipedia_views()

# Substitute "null" occurences with empty values
all_df[]                 <- lapply(all_df, as.character)
all_df[all_df == "null"] <- NA

# Write combined goods' dataset to all.csv
write.csv(all_df, paste(csv_directory, "all.csv", sep="/"), row.names=FALSE)
