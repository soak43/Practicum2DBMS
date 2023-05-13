# Part 2: Load Data Warehouse

# Author - Sayali Kedar Oak
# Course - CS5200 Database Management Systems
# Practicum 2

#Libraries
library(RMariaDB)
library(RSQLite)
library(sqldf)

# establishing a MySQL connection
db_user <- 'root'
db_pass <- 'root123'
db_name <- 'Practicum2DB'
db_host <- 'localhost'
db_port <- 3306
sql_dbcon <- dbConnect(drv = RMariaDB::MariaDB(), user = db_user, password = db_pass,
                 dbname = db_name, host = db_host, port = db_port)


#sql_dbcon= dbConnect(RMySQL::MySQL(),host='localhost',port=3306,user='root',password='root123')

# dropping database if exists
dbExecute(sql_dbcon,"DROP DATABASE IF EXISTS Practicum2DB")
dbGetQuery(sql_dbcon,"SHOW DATABASES")

# creating a database in MySQL
dbExecute(sql_dbcon,"CREATE DATABASE Practicum2DB")
dbGetQuery(sql_dbcon,"SHOW DATABASES")

# Initializing the created DB for further use
dbExecute(sql_dbcon,"USE Practicum2DB")

# dropping tables
dbExecute(sql_dbcon,"DROP TABLE IF EXISTS JournalFacts")
dbExecute(sql_dbcon,"DROP TABLE IF EXISTS TimeDimension")
dbExecute(sql_dbcon,"DROP TABLE IF EXISTS JournalDimension")


# creating Journal Dimension
dbExecute(sql_dbcon, "CREATE TABLE JournalDimension(
          JournalID INTEGER PRIMARY KEY,
          JournalTitle TEXT)")

# creating Time Dimension
dbExecute(sql_dbcon, "CREATE TABLE TimeDimension(
          TimeID INTEGER PRIMARY KEY AUTO_INCREMENT,
          Year INTEGER,
          Month INTEGER,
          Quarter INTEGER)")

## Creating Journal Facts table

dbExecute(sql_dbcon,"CREATE TABLE JournalFacts(
          JournalFactID INTEGER PRIMARY KEY AUTO_INCREMENT,
          JournalID INTEGER,
          TimeID INTEGER,
          NumArticlesPerQuarter INTEGER,
          NumArticlesPerYear INTEGER,
          NumUniqueAuthorsPerQuarter INTEGER,
          NumUniqueAuthorsPerMonth INTEGER,
          FOREIGN KEY (JournalID) REFERENCES JournalDimension(JournalID),
          FOREIGN KEY (TimeID) REFERENCES TimeDimension(TimeID))")

# Connecting to SQLite database from part 1 to fetch the data

sqlite_con <- dbConnect(RSQLite::SQLite(), "publications_practicum2.db")


#dbListTables(sqlite_con)

### Populating Dimension tables in MySQL using data from SQLite DB


dbSendQuery(sql_dbcon, "SET GLOBAL local_infile = true;")

## Creating Dataframes

journal_df <- dbGetQuery(sqlite_con,"SELECT JournalID, JournalTitle FROM Journal",row.names=FALSE)
head(journal_df)

## Copying from dataframes into MySQL tables 

dbWriteTable(sql_dbcon,"journaldimension",journal_df,append=T,row.names=FALSE)

## checking if the dimension tables were populated with data

d <- dbGetQuery(sql_dbcon, "SELECT * FROM JournalDimension LIMIT 10")
print(d)

# Create a dataframe
df <- dbGetQuery(sqlite_con, "SELECT J.JournalID, IssnTypeID, ISSN, J.JournalTitle, ISOAbbreviation, JI.JournalIssueID, CitedMediumID, Volume, Issue, A.PMID, A.ArticleTitle, A.SeasonID, A.PubDate, A.MedlineDate, AL.AuthorID
FROM Journal J
INNER JOIN JournalIssue JI ON J.JournalID = JI.JournalID
INNER JOIN Article A ON JI.JournalIssueID = A.JournalIssueID
INNER JOIN AuthorList AL ON A.PMID = AL.PMID
GROUP BY J.JournalID
LIMIT 100;",row.names=F)

# Add 2 columns, quarter and year
df$quarter <- NA
df$year <- NA
df$month <- NA

# Calculate the year and quarter for every column

for(i in 1:nrow(df)){
  
  df[i,'year'] <- as.integer(substr(df[i,'PubDate'], 1, 4))
  if(is.na(df[i,'year'])){
    df[i,'year'] <- 0
  }
  month <- substr(df[i,'PubDate'], 6, 7)
  df[i,'quarter'] <- as.integer(as.integer(month)/4) + 1
  if(is.na(df[i,'quarter'])){
    df[i,'quarter'] <- 0
  }
  df[i,'month'] <- as.integer(month)
  if(is.na(df[i,'month'])){
    df[i,'month'] <- 0
  }
}
head(df)

# Load the year, month, quarter columns into time_dimension table
options(sqldf.driver = 'SQLite')
time_df <- sqldf("SELECT DISTINCT year, month, quarter FROM df",row.names = FALSE)

#dbExecute(sql_dbcon,"USE Practicum2DB")
# Write to the timedimension table
RMariaDB::dbWriteTable(sql_dbcon, "timedimension", time_df, append=T, row.names=FALSE)

# Load the time dimension table into a dataframe for easy querying

time_data_frame <- dbGetQuery(sql_dbcon, "SELECT * FROM TimeDimension")
head(time_data_frame)
# Calculate facts
#detach("package:RMySQL", unload=TRUE)
new_df_NumArticlesPerQuarter <- sqldf("SELECT quarter, Count(PMID) as articlesPerQuarter FROM df GROUP BY quarter",row.names = FALSE)
new_df_NumArticlesPerYear <- sqldf("SELECT year, Count(PMID) as articlesPerYear FROM df GROUP BY year",row.names = FALSE)
new_df_NumUniqueAuthorsPerQuarter <- sqldf(paste0("SELECT DISTINCT COUNT(AuthorID) as NumUniqueAuthorsPerQuarter, quarter FROM df GROUP BY quarter"),row.names = FALSE)
new_df_NumUniqueAuthorsPerMonth <- sqldf(paste0("SELECT month, COUNT(AuthorID) AS NumUniqueAuthorsPerMonth FROM df GROUP BY month"),row.names = FALSE)


# Load the data frame with values

df$NumArticlesPerQuarter <- NA
df$NumArticlesPerYear <- NA
df$NumUniqueAuthorsPerMonth <- NA
df$NumUniqueAuthorsPerQuarter <- NA

for(i in 1:nrow(df)){
  
  y <- df[i,'year']
  m <- df[i,'month']
  q <- df[i,'quarter']
  df[i,'NumArticlesPerQuarter'] <- sqldf(paste0("SELECT articlesPerQuarter FROM new_df_NumArticlesPerQuarter WHERE quarter = ",q),row.names = FALSE)
  df[i,'NumArticlesPerYear'] <- sqldf(paste0("SELECT articlesPerYear FROM new_df_NumArticlesPerYear WHERE year = ",y),row.names = FALSE)
  df[i,'NumUniqueAuthorsPerMonth'] <- sqldf(paste0("SELECT NumUniqueAuthorsPerMonth FROM new_df_NumUniqueAuthorsPerMonth WHERE month = ",m),row.names = FALSE)
  df[i,'NumUniqueAuthorsPerQuarter'] <- sqldf(paste0("SELECT NumUniqueAuthorsPerQuarter FROM new_df_NumUniqueAuthorsPerQuarter WHERE quarter = ",q),row.names = FALSE)
}
head(df)



# Load the timeID in the dataframe 

for(i in 1:nrow(df)){
  
  df[i,'timeID'] <- sqldf(paste0("SELECT DISTINCT TimeID FROM time_data_frame 
                                              WHERE year = ",df[i,'year']," 
                                              AND month = ",df[i,'month']," 
                                              AND quarter = ",df[i,'quarter']),row.names = FALSE)
}
head(df)

# Store this df in a temporary table in sqlite

dbWriteTable(sqlite_con,"tmp",df,overwrite=T,row.names=F)

# Get the required columns from the tmp table

final_df <- dbGetQuery(sqlite_con, "SELECT JournalID, timeID, 
                       NumArticlesPerQuarter, NumArticlesPerYear, 
                       NumUniqueAuthorsPerMonth, NumUniqueAuthorsPerQuarter
                       FROM tmp",row.names=FALSE)


# Check if required - dbExecute(sql_dbcon,"USE Practicum2DB")
RMariaDB::dbWriteTable(sql_dbcon, "journalfacts", final_df, overwrite=T, row.names=FALSE)

## Disconnecting MySQL and SQLite connection
dbDisconnect(sqlite_con)
dbDisconnect(sql_dbcon)
