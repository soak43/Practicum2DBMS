## Part 1 : LOAD XML DATA INTO DATABASE

# Author - Sayali Kedar Oak
# Course - CS5200 Database Management Systems
# Practicum 2

# Libraries

library(RSQLite)
library(XML)
library(stringr)

# Connecting to sqlite

dbcon <- dbConnect(RSQLite::SQLite(), "publications_practicum2.db")

# Drop all pre-existing tables

dbExecute(dbcon, "DROP TABLE IF EXISTS AffiliationLookup")
dbExecute(dbcon, "DROP TABLE IF EXISTS ISSNTypeLookup")
dbExecute(dbcon, "DROP TABLE IF EXISTS SeasonLookup")
dbExecute(dbcon, "DROP TABLE IF EXISTS CitedMediumLookup")
dbExecute(dbcon, "DROP TABLE IF EXISTS Author")
dbExecute(dbcon, "DROP TABLE IF EXISTS AuthorList")
dbExecute(dbcon, "DROP TABLE IF EXISTS Journal")
dbExecute(dbcon, "DROP TABLE IF EXISTS JournalIssue")
dbExecute(dbcon, "DROP TABLE IF EXISTS Article")

# Create Affiliation lookup table

sqlcmd <- "CREATE TABLE AffiliationLookup(
          AffID INTEGER PRIMARY KEY AUTOINCREMENT,
          Affiliation TEXT)"
dbExecute(dbcon,sqlcmd)

# Create Cited Medium lookup table

sqlcmd <- "CREATE TABLE CitedMediumLookup(
            CitedMediumID INTEGER PRIMARY KEY AUTOINCREMENT,
            CitedMedium VARCHAR(50))"
dbExecute(dbcon,sqlcmd)

# Create Seasons lookup table

sqlcmd <- "CREATE TABLE SeasonLookup(
            SeasonID INTEGER PRIMARY KEY AUTOINCREMENT,
            Season VARCHAR(20))"
dbExecute(dbcon,sqlcmd)

# Create ISSNType Lookup table

sqlcmd <- "CREATE TABLE ISSNTypeLookup(
            ISSNTypeID INTEGER PRIMARY KEY AUTOINCREMENT,
            ISSNType VARCHAR(50))"
dbExecute(dbcon,sqlcmd)

# Create Author table

sqlcmd <- "CREATE TABLE Author(
            AuthorID INTEGER PRIMARY KEY,
            ValidYN BOOLEAN DEFAULT 1,
            LastName VARCHAR(50),
            ForeName VARCHAR(50),
            Initials VARCHAR(3),
            Suffix VARCHAR(5),
            CollectiveName VARCHAR(100),
            AffiliationID INTEGER,
            FOREIGN KEY (AffiliationID) REFERENCES Affiliation(AffID))"
dbExecute(dbcon,sqlcmd)

# Create Journal table

sqlcmd <- "CREATE TABLE Journal(
            JournalID INTEGER PRIMARY KEY AUTOINCREMENT,
            IssnTypeID INTEGER,
            ISSN VARCHAR(50),
            JournalTitle TEXT,
            ISOAbbreviation TEXT,
            FOREIGN KEY (IssnTypeID) REFERENCES ISSNTypeLookup(ISSNTypeID))"
dbExecute(dbcon,sqlcmd)

# Create Journal Issue table

sqlcmd <- "CREATE TABLE JournalIssue(
            JournalIssueID INTEGER PRIMARY KEY AUTOINCREMENT,
            JournalID INTEGER,
            CitedMediumID INTEGER,
            Volume VARCHAR(50),
            Issue VARCHAR(50),
            FOREIGN KEY (CitedMediumID) REFERENCES CitedMediumLookup(CitedMediumID),
            FOREIGN KEY (JournalID) REFERENCES Journal(JournalID))"
dbExecute(dbcon,sqlcmd)

# Create article table 

sqlcmd <- "CREATE TABLE Article(
            PMID INTEGER PRIMARY KEY,
            ArticleTitle TEXT,
            SeasonID INTEGER,
            PubDate DATE,
            MedlineDate TEXT,
            JournalIssueID INTEGER,
            FOREIGN KEY (SeasonID) REFERENCES SeasonLookup(SeasonID),
            FOREIGN KEY (JournalIssueID) REFERENCES JournalIssue(JournalIssueID))"
dbExecute(dbcon,sqlcmd)

#Create Author List table

sqlcmd <- "CREATE TABLE AuthorList(
            PMID INTEGER,
            AuthorID INTEGER,
            FOREIGN KEY (PMID) REFERENCES Article(PMID),
            FOREIGN KEY (AuthorID) REFERENCES Author(AuthorID))"
dbExecute(dbcon,sqlcmd)



## Validate XML


url <- "http://practicum2dbms.s3.amazonaws.com/pubmed22n0001-tf.xml"

doc <- xmlParse(url,validate = TRUE, useInternalNodes = TRUE)

root_node <- xmlRoot(doc)


# GENERATE DATE FORMAT

generateDate <- function(pubDate) {
  day <- 0
  month <- 0
  year <- 0
  if(is.null(pubDate[['Day']])){}
  else{
    day <- xmlValue(pubDate[['Day']])
  }
  
  if(is.null(pubDate[['Month']])){}
  else{
    month <- MonthNo(xmlValue(pubDate[['Month']]))
  }
  if(is.null(pubDate[['Year']])){
    year <- 9999
  }
  else{
    year <- xmlValue(pubDate[['Year']])
  }
  full_date <- paste0(year,"/",month,"/",day)
  return(full_date)
}


# FIND AUTHOR IF EXISTS

find_author <- function(author){
  
  #print("Inside Author")
  
  # Forename
  
  if(is.na(author[['ForeName']]) || is.null(author[['ForeName']])){
    foreName <- ""
  }
  else{
    foreName <- xmlValue(author[['ForeName']])
  }
  
  #print(paste0("Forename = ",foreName))
  
  # Last Name
  
  if(is.na(author[['LastName']]) ||is.null(author[['LastName']]) ){
    lastName <- ""
  }
  else{
    lastName <- xmlValue(author[['LastName']])
  }
  
  #print(paste0("Last name = ",lastName))
  
  # Initials
  
  if(is.na(author[['Initials']]) || is.null(author[['Initials']])){
    initials <- ""
  }
  else{
    initials <- xmlValue(author[['Initials']])
  }
  
  #print(paste0("Initials = ",initials))
  
  # Suffix
  
  if(is.na(author[["Suffix"]]) || is.null(author[["Suffix"]])){
    suffix <- ""
  }else{
    suffix <- xmlValue(author[["Suffix"]])
  }
  
  #print(paste0("suffix = ",suffix))
  
  # Collective Name
  
  if(is.na(author[["CollectiveName"]]) || is.null(author[["CollectiveName"]])){
    collectiveName <- ""
  }else{
    collectiveName <- xmlValue(author[["CollectiveName"]])
  }
  
  #print(paste0("collective name = ",collectiveName))
  
  sqlcmd <- sprintf('SELECT AuthorID FROM Author WHERE ForeName = "%s" 
            AND LastName = "%s" AND Initials = "%s" AND
            Suffix = "%s" AND CollectiveName = "%s"',foreName, lastName, initials, suffix, collectiveName)
  
  result <- dbGetQuery(dbcon,sqlcmd)
  return(as.integer(result))
  
}


# FIND AFFILIATION ID IF IT EXISTS

get_aff_id <- function(aff){
  
  sqlcmd <- sprintf("SELECT affID FROM affiliationLookup WHERE affiliation = '%s'",aff)
  result <- dbGetQuery(dbcon,sqlcmd)
  
  if(is.null(result) || is.null(result)){
    # Add affiliation
    sqlcmd <- sprintf("INSERT INTO affiliationLookup(affiliation) VALUES ('%s')",aff)
    dbExecute(dbcon, sqlcmd)
    sqlcmd <- sprintf("SELECT affID FROM affiliationLookup WHERE affiliation = '%s'",aff)
    result <- dbGetQuery(dbcon,sqlcmd)
  }
  
  return(as.integer(result))
}


# Get month number

MonthNo <- function(month){
  num <- "0"
  switch(month, 
         Jan={num <- "01"},
         Feb={num <- "02"},
         Mar={num <- "03"},
         Apr={num <- "04"},
         May={num <- "05"},
         Jun={num <- "06"},
         Jul={num <- "07"},
         Aug={num <- "08"},
         Sep={num <- "09"},
         Oct={num <- "10"},
         Nov={num <- "11"},
         Dec={num <- "12"})
  return (num)
}

# Find if issn type exist in the issn type lookup table


find_ISSN_Type <- function(issnT){
  
  #print("Inside issn type")
  
  sqlcmd <- sprintf("SELECT IssnTypeID FROM IssnTypeLookup WHERE IssnType = '%s'",issnT)
  
  r_issn <- dbGetQuery(dbcon,sqlcmd)
  #print(as.integer(r_issn))
  
  if(is.na(as.integer(r_issn)) || is.null(as.integer(r_issn))){
    # no issn type found
    # insert issn type
    #print("Inside if")
    sqlcmd <- sprintf("INSERT INTO IssnTypeLookup(IssnType) VALUES('%s')",issnT)
    dbExecute(dbcon,sqlcmd)
    #print("Db execute done")
    sqlcmd <- sprintf("SELECT IssnTypeID FROM IssnTypeLookup WHERE IssnType = '%s'",issnT)
    r_issn <- dbGetQuery(dbcon,sqlcmd)
  }
  #print("Returning issn")
  #print(as.integer(r_issn))
  return(as.integer(r_issn))

}


# Find if journal exists in the journal table

find_journal <- function(j_node, issnid){
  
  #print("Inside find journal")
  
  if(is.na(xmlValue(j_node[["Title"]])) || is.null(xmlValue(j_node[["Title"]]))){
    t <- ""
  }else{
    t <- removeQuotes(xmlValue(j_node[["Title"]]))
  }
  
  if(is.na(xmlValue(j_node[["ISSN"]])) || is.null(xmlValue(j_node[["ISSN"]]))){
    i <- ""
  }else{
    i <- xmlValue(j_node[["ISSN"]])
  }
  
  if(is.na(xmlValue(j_node[["ISOAbbreviation"]])) || is.null(xmlValue(j_node[["ISOAbbreviation"]]))){
    iso <- ""
  }else{
    iso <- xmlValue(j_node[["ISOAbbreviation"]])
  }
  
  sqlcmd <- sprintf("SELECT JournalID FROM Journal WHERE JournalTitle = '%s' AND 
                    IssnTypeID = %d AND ISSN = '%s' AND ISOAbbreviation = '%s'",
                    t,as.integer(issnid),i,iso)
  #print(sqlcmd)
  
  j_id <- dbGetQuery(dbcon,sqlcmd)
  
  # Journals with different ISSN but same name are considered as different journals
  
  if(is.na(as.integer(j_id)) || is.null(as.integer(j_id))){
    #insert into journal
    sqlcmd <- sprintf("INSERT INTO Journal(JournalTitle,IssnTypeID, ISSN, 
                      ISOAbbreviation) VALUES ('%s', %d, '%s', '%s')",t,
                      as.integer(issnid),i,iso)
    dbExecute(dbcon,sqlcmd)
    sqlcmd <- sprintf("SELECT JournalID FROM Journal WHERE JournalTitle = '%s' AND 
                    IssnTypeID = %d AND ISSN = '%s' AND ISOAbbreviation = '%s'",
                      t,as.integer(issnid),i,iso)
    j_id <- dbGetQuery(dbcon,sqlcmd)
  }
  #print("Returning fin journal")
  return(as.integer(j_id))
    
}

# Get cited medium id, if it does not exist, insert into the lookup table

get_cited_medium_id <- function(cm){
  
  sqlcmd <- sprintf("SELECT CitedMediumID FROM CitedMediumLookup WHERE CitedMedium = '%s'",cm)
  cmid <- dbGetQuery(dbcon,sqlcmd)
  #print(as.integer(cmid))

  if(is.na(as.integer(cmid)) || is.null(as.integer(cmid))){
    # cited medium needs to be inserted into the table
    sqlcmd <- sprintf("INSERT INTO CitedMediumLookup(CitedMedium) VALUES('%s')",cm)
    dbExecute(dbcon,sqlcmd)
    sqlcmd <- sprintf("SELECT CitedMediumID FROM CitedMediumLookup WHERE CitedMedium = '%s'",cm)
    cmid <- dbGetQuery(dbcon,sqlcmd)
  }
  return(as.integer(cmid))
}


find_season <- function(s){
  
  sqlcmd <- "SELECT SeasonID FROM SeasonLookup WHERE Season = ?"
  season_id <- dbGetQuery(dbcon,sqlcmd,params=as.list(s))
  if(is.na(as.integer(season_id)) || is.null(as.integer(season_id))){
    
    #insert season
    sqlcmd <- "INSERT INTO SeasonLookup(Season) VALUES(?)"
    dbExecute(dbcon,sqlcmd,params=as.list(s))
    sqlcmd <- "SELECT SeasonID FROM SeasonLookup WHERE Season = ?"
    season_id <- dbGetQuery(dbcon,sqlcmd,params=as.list(s))
    
  }
  return(as.integer(season_id))
}

find_journal_issue_id <- function(JI_node, citedMedID, journalID){
  
  # If volume or issue is not present - set as -1
  # Volume
  
  if(is.na(JI_node[["Volume"]]) || is.null(JI_node[["Volume"]])){
    volume <- ""
  }else{
    volume <- xmlValue(JI_node[["Volume"]])
  }

  #print(paste0("volume = ",volume))
  #print(typeof(volume))
  
  # Issue
  if(is.na(JI_node[["Issue"]]) || is.null(JI_node[["Issue"]])){
    issue <- ""
  }else{
    issue <- xmlValue(JI_node[['Issue']])
  }
  
  #print(paste0("issue = ",issue))
  
  # Check is the journal issue entry is already present in the journal issue table
  
  sqlcmd <- sprintf("SELECT JournalIssueID FROM JournalIssue WHERE JournalID = %d AND Volume = '%s' AND Issue = '%s' AND CitedMediumID = %d",journalID,volume,issue,citedMedID)
  #print(sqlcmd)
  journalid_result <- dbGetQuery(dbcon,sqlcmd)
  
  if(is.na(as.integer(journalid_result))){
    # Insert into journal issue table
    sqlcmd <- sprintf("INSERT INTO JournalIssue(JournalID, Volume, Issue, CitedMediumID) VALUES(%d,'%s','%s',%d)",journalID,volume,issue,citedMedID)
    #print(sqlcmd)
    dbExecute(dbcon,sqlcmd)
    # Find journal issue id
    sqlcmd <- sprintf("SELECT JournalIssueID FROM JournalIssue WHERE JournalID = %d AND Volume = '%s' AND Issue = '%s' AND CitedMediumID = %d",journalID,volume,issue,citedMedID)
    journalid_result <- dbGetQuery(dbcon,sqlcmd)
  }
  
  return(as.integer(unlist(journalid_result)))
  
}


removeQuotes <- function(str){
  x <- str_replace_all(str, "'", "''")
  return(x)
}


i <- 1
extractData <- function(root_node){
  
  size <- xmlSize(root_node)
  
  for(i in 1:size){
    
    article <- root_node[[i]]
    
    #article_id <- strtoi(xmlAttrs(publication))
    
    #article <- root_node[["Article"]]
    
    if(is.na(article)){
      next
    }else{
      pmid <- as.integer(xmlAttrs(article))        #check
    }
    
    print(paste0("pmid = ",pmid))
    #print(typeof(pmid))
    
    #parse the journal from here
    pubDetails <- article[["PubDetails"]]
    
    journal <- pubDetails[["Journal"]]
    
    if(!is.na(journal)){
      
      # ISSN type
      
      if(is.null(xmlAttrs(journal[[1]]))){
        issntype <- ""
      }else{
        issntype <- xmlAttrs(journal[[1]])
      }
      
      # Find ISSN type in ISSN lookup Table
      
      issntypeid <- find_ISSN_Type(issntype)
      
      #print(paste0("issntype = ",issntype))
      
      # Check if Journal exists
      
      journal_id <- find_journal(journal,issntypeid)
      
      #print("Got journal id")
      #print(journal_id)
      
      
      # Journal Issue
      
      journal_issue <- journal[['JournalIssue']]
      
      # Cited medium
      
      if(is.null(xmlAttrs(journal[[2]]))){
        cited_medium <- "unknown"
      }else{
        cited_medium <- xmlAttrs(journal[[2]])
      }
      
      #print(paste0("cited medium = ",cited_medium))
      
      # Get cited medium ID
      
      cited_med_id <- get_cited_medium_id(cited_medium)
      
      #print(cited_med_id)
      #print(typeof(cited_med_id))
      
      # Insert into Journal Issue Table If the entry is not already present
      
      jid <- as.integer(find_journal_issue_id(JI_node = journal_issue,citedMedID = as.integer(cited_med_id),journalID = as.integer(journal_id)))
      
      
      # Publication Date
      
      pubDate <- generateDate(journal_issue[['PubDate']])
      
      #print(paste0("P Date = ",pubDate))
      
      # Season
      
      pDate <- journal_issue[['PubDate']]
      
      if(is.null(pDate[['Season']]) || is.na(pDate[['Season']])){
        season <- "unknown"
      }else{
        season <- xmlValue(pDate[['Season']])
      }
      #print("Season = ")
      #print(season)
      
      # Check if season is present in the season lookup table
      
      s_id <- as.integer(find_season(season))
      #print(typeof(s_id))
      #print(s_id)
      
      # Medline Date
      
      if(is.na(pDate[["MedlineDate"]]) || is.null(pDate[["MedlineDate"]])){
        medDate <- "unknown"
      }else{
        medDate <- xmlValue(pDate[["MedlineDate"]])
      }
      
      #print(paste0("medline date = ", medDate))
      
    }
    
    
    # Article Title
    
    if(is.null(pubDetails[['ArticleTitle']]) || is.na(pubDetails[['ArticleTitle']])){
      article_title <- ""
    }else{
      article_title <- removeQuotes(xmlValue(pubDetails[['ArticleTitle']]))
    }
    
    #print(paste0("aticle title  = ",article_title))
    
    #print(typeof(pmid))
    #print(typeof(s_id))
    #print(typeof(jid))
    #print(jid)

    
    sqlcmd <- sprintf("INSERT INTO Article(PMID, ArticleTitle, SeasonID, PubDate, MedlineDate, JournalIssueID) VALUES (%d,'%s',%d,'%s','%s',%d)",pmid, article_title,s_id,pubDate,medDate,jid)
    #print(sqlcmd)
    dbExecute(dbcon,sqlcmd)
    
    #Author List
    
    author_list <- pubDetails[['AuthorList']]
    
    
    if(!is.null(author_list) && !is.na(author_list)){
      author_size <- xmlSize(author_list)
      
      for(j in 1:author_size){
        author <- author_list[[j]]
        if(is.na(author)){
          next
        }
        # Find Author
        
        #print("Before find author")
        
        author_id <- find_author(author)
        
        #print("Done with find_author")
        
        if(is.na(author_id) || is.null(author_id)){
          
          # Check is affiliation is present
          if(is.na(author[["AffiliationInfo"]]) || is.null(author[["AffiliationInfo"]])){
            affiliation <- "unknown"
          }else{
            affInfo <- author[["AffiliationInfo"]]
            affiliation <- xmlValue(affInfo[["Affiliation"]])
          }
          
          #print(paste0("Affiliation = ",affiliation))
          # Get affiliation id
          
          aff_id <- get_aff_id(affiliation)
          if(is.na(aff_id) || is.null(aff_id)){
            affiliation_ID <- NULL
          }else{
            affiliation_ID <- as.integer(aff_id)
          }
          #print(paste0("Aff id = ",affiliation_ID))
          #print(typeof(affiliation_ID))
          
          # If/else block to check the null attributes of the Author table
          
          # Forename
          
          if(is.na(author[['ForeName']]) || is.null(author[['ForeName']])){
            foreName <- ""
          }
          else{
            foreName <- xmlValue(author[['ForeName']])
          }
          
          #print(paste0("Forename = ",foreName))
          
          # Last Name
          
          if(is.na(author[['LastName']]) ||is.null(author[['LastName']]) ){
            lastName <- ""
          }
          else{
            lastName <- removeQuotes(xmlValue(author[['LastName']]))
          }
          
          #print(paste0("Last name = ",lastName))
          
          # Initials
          
          if(is.na(author[['Initials']]) || is.null(author[['Initials']])){
            initials <- ""
          }
          else{
            initials <- xmlValue(author[['Initials']])
          }
          
          #print(paste0("Initials = ",initials))
          
          # Suffix
          
          if(is.na(author[["Suffix"]]) || is.null(author[["Suffix"]])){
            suffix <- ""
          }else{
            suffix <- xmlValue(author[["Suffix"]])
          }
          
          #print(paste0("suffix = ",suffix))
          
          # Collective Name
          
          if(is.na(author[["CollectiveName"]]) || is.null(author[["CollectiveName"]])){
            collectiveName <- ""
          }else{
            collectiveName <- xmlValue(author[["CollectiveName"]])
          }
          
          #print(paste0("collective name = ",collectiveName))
          
          # ValidYN
          
          if(is.null(xmlAttrs(author[[1]])) || is.na(xmlAttrs(author[[1]]))){
            valid_YN <- "Y"
          }else{
            valid_YN <- xmlAttrs(author[[1]])
          }
          
          #print(paste0("Valid YN = ",valid_YN))
          #print(typeof(valid_YN))
          # Convert validYN to boolean value
          if(valid_YN == 'Y'){
            vYN <- as.integer(1)
          }else{
            vYN <- as.integer(0)
          }
          
          if(is.null(affiliation_ID)){
            insert_author_sqlcmd <- sprintf("INSERT INTO Author(ValidYN, LastName, ForeName, Initials, Suffix, CollectiveName) VALUES (%d,'%s','%s','%s','%s','%s')",vYN, lastName, foreName,initials, suffix, collectiveName)
            #print(insert_author_sqlcmd)
          }else{
            # Insert into author table
            insert_author_sqlcmd <- sprintf("INSERT INTO Author(ValidYN, LastName, ForeName, Initials, Suffix, CollectiveName, AffiliationID) VALUES (%d,'%s','%s','%s','%s','%s',%d)",vYN, lastName, foreName,initials, suffix, collectiveName,affiliation_ID)
            #print(insert_author_sqlcmd)
          }
          dbGetQuery(dbcon,insert_author_sqlcmd)
          
          author_id <- find_author(author)
          
        }
      
        #print(vYN)
        #print(typeof(vYN))
        #print(lastName)
        #print(typeof(lastName))
        #print(foreName)
        #print(typeof(foreName))
        #print(initials)
        #print(typeof(initials))
        #print(suffix)
        #print(typeof(suffix))
        #print(collectiveName)
        #print(typeof(collectiveName))
        #print(affiliation_ID)
        #print(typeof(affiliation_ID))
        
        # insert into Author List table
        
        insert_authorlist_sqlcmd <- sprintf("INSERT INTO AuthorList(PMID, AuthorID) VALUES(%d,%d)",pmid,author_id)
        #print(insert_authorlist_sqlcmd)
        dbExecute(dbcon,insert_authorlist_sqlcmd)
        
        
      }
    }
  }
  
}

# Call extract data

extractData(root_node)

s <- dbDisconnect(dbcon)
