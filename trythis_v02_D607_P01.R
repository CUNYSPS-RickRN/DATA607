# try this V2 D607_P01  another input method  RickRN
library(tidyverse)

theURL <- c("https://raw.githubusercontent.com/CUNYSPS-RickRN/DATA607/master/tournamentinfo.txt")

chess_V2_df <- data.frame(read_csv(file=theURL, col_names=FALSE, skip = 3, comment = "--"))  # file contains rows of all dashes, comment= will remove these rows

x = -1  # prime index for odd numbered rows
y = 0   # prime index for even numbered rows

OutputCSV_df <- data.frame()
OutputCSV_df

Opponents_df <- data.frame()
Opponents_df


for (i in 1:64)
  {

# odd numbered rows contain player name
  x= x + 2

  chess_V2_df$X1[x]

# Extract Player Number
  str_chess_pattern = "^([0-9]{1,2})"  # pattern for player number
  strChessData <- chess_V2_df$X1[x]
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect player number
  str_view_all(str_trim(strChessData), str_chess_pattern)

  t_playernum <- as.numeric(str_extract(str_trim(strChessData), str_chess_pattern))


# Extract Player Name
  str_chess_pattern = c("([A-Za-z]+\\s[A-Za-z]+)(\\s[A-Za-z]+){0,}") # pattern for player name
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect player name
  str_view_all(str_trim(strChessData), str_chess_pattern)
  c_playername <- str_extract(str_trim(strChessData), str_chess_pattern)


# Extract Total Points
  str_chess_pattern = c("([0-9]+\\.[0-9]+)") # pattern for Total Pts
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect chess rounds
  str_view_all(str_trim(strChessData), str_chess_pattern)
  c_totpoints <- (str_extract(str_trim(strChessData), str_chess_pattern))
# need to convert c_totpoints to numeric

# Extract Chess Rounds Opponents
  str_chess_pattern = c("(W|L|D)(\\s+)[0-9]{1,2}") # pattern for chess rounds
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect chess rounds
  str_view_all(str_trim(strChessData), str_chess_pattern)

  str_locate_all(str_trim(strChessData), str_chess_pattern) # locate Rounds

  chess_rounds <- unlist(str_extract_all(str_trim(strChessData), str_chess_pattern)) # locate Round ?stpositions

# work on pulling out opponent numbers
  str_chess_pattern = c("[0-9]{1,2}") # pattern for chess rounds
  str_detect(str_trim(chess_rounds), str_chess_pattern)  # Detect chess rounds
  str_view_all(str_trim(chess_rounds), str_chess_pattern)

  str_locate_all(str_trim(chess_rounds), str_chess_pattern) # locate Round ?stpositions
  chess_rounds <- as.numeric(unlist(str_extract_all(str_trim(chess_rounds), str_chess_pattern))) # locate Round ?stpositions





## 

# even numbered rows have location / ranking info
  y = y + 2

# Extract Player Location (State/Province)
  str_chess_pattern = "^([A-Za-z]{1,2})"  # pattern for Player State/Prov
  strChessData <- chess_V2_df$X1[y]
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect player State/Prov
  str_view_all(str_trim(strChessData), str_chess_pattern)
  c_loc <- str_extract(str_trim(strChessData), str_chess_pattern)


# Extract Player Pre-rating
# pre-rating part 1
  str_chess_pattern = "(R:)(\\s+)([0-9]{1,4})"  # pattern for player pre-rating
  str_detect(str_trim(strChessData), str_chess_pattern)  # Detect player pre-rating
  str_view_all(str_trim(strChessData), str_chess_pattern)

  c_prerating <- str_extract(str_trim(strChessData), str_chess_pattern)

# pre-rating part 2
  str_chess_pattern = "[0-9]{1,4}"  # pattern for numeric portion player pre-rank
  str_detect(str_trim(c_prerating), str_chess_pattern)  # Detect player pre-rating
  str_view_all(str_trim(c_prerating), str_chess_pattern)

  c_prerating <- as.numeric(str_extract(str_trim(c_prerating), str_chess_pattern))
# figure out how to extract number portion more cleanly
  str(c_prerating)


# 
# put player pre-rating into a list  64 players and their pre-rating values
# chess_preratings_list <- c_prerating

#put player opponents 64 players x  7 rounds chess, games played
#[1] "39,21,18,14,7,12,4"

  this_Opponents_df <- data.frame(t_playernum, chess_rounds, c_prerating=NA)
  Opponents_df <- bind_rows(Opponents_df, this_Opponents_df)  # add "bind" row to df



# expected output ".csv file" from as a dataframe 64 players x 5 output values
  t_playernum          # be sure to remove from CSV file
  c_playername
  c_loc
  c_totpoints
  c_prerating
  c_avg_opp_prerating <- NA

  theCSV <- data.frame(t_playernum, c_playername, c_loc, c_totpoints, c_prerating, c_avg_opp_prerating )
#  length(theCSV)
#  names(theCSV)
#  dim(theCSV)
#  theCSV_row <- str_flatten(theCSV,collapse = ",")
#  theCSV_row

# bind_rows(theCSV, output)
  OutputCSV_df <- bind_rows(OutputCSV_df, theCSV)  # add "bind" row to df

}

OutputCSV_df

# Calculate Mean of opponent's pre-ratings
thisval_df <- data.frame()  # define df
thisval_df

#thisval <- OutputCSV_df$c_prerating[Opponents_df$chess_rounds[1]]
#OutputCSV_df$c_avg_opp_prerating[1] <- mean(thisval$c_prerating)

for (i in 1:nrow(Opponents_df))
  {
#  
  Opponents_df$c_prerating[i] <- OutputCSV_df$c_prerating[Opponents_df$chess_rounds[i]]
}


# get subset of each player's opponents then calc mean of opponents' pre-ratings
for (i in 1:nrow(OutputCSV_df))
  {
    thisval <- subset(Opponents_df, t_playernum == i, select = c_prerating)
    print (i)
    print (thisval)
    
    OutputCSV_df$c_avg_opp_prerating[i] <- round(mean(thisval$c_prerating))
    
}


# Key datasets
print (OutputCSV_df)
Opponents_df

# (TBD) remove temporary playernum vector prior to creating .csv
write.csv(OutputCSV_df)

