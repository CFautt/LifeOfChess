library('bigchess')
library('rchess')
library('stringr')
library('seqinr')

full.path.to.pgn <- "ENTER PATH TO PGN FILE HERE"
output.filename <- "ENTER DESIRED FASTA FILENAME HERE"

# load one PGN:
PGN <- read.pgn(full.path.to.pgn) 
# remove all but first 9 columns
PGN <- PGN[,1:9]

# These will be a list of lists of white and black moves. eventually these will be our new columns
white <- list()
black <- list()


# 'game' represents the movetext for a single game, so this loops over all games' movetexts
for (game in PGN$Movetext)
{
  
  # Split each move using spaces as separators
  all.moves <- strsplit(game, " ")
  
  # Black's moves are easier to process because they don't have the move numbers 
  # attached to them. I make a list of black moves from the even numbered items in all.moves
  b <- list(all.moves[[1]][c(F,T)])
  black <- c(black,b)
  
  # I do the same for white, grabbing the odd numbered items in all.moves, 
  # but I also need to remove the move number and period after it using lapply and gsub.
  w <- list(all.moves[[1]][c(T,F)])
  w <- lapply(w, function(x) gsub("^.*\\.","",as.character(x)))
  white <- c(white,w)
  
}

# now I just make new columns in the df and assign our newly created move lists to them.
PGN$White.moves <- white
PGN$Black.moves <- black

# We're done! 

# This column:
PGN$Movetext[1]

# Became these two columns:
PGN$White.moves[1]
PGN$Black.moves[1]



# We need a place to put our stuff
FEN <- c()

# For every game in df: 1) make a move for white and record the board position, 2) check if there's a reply from black, and if so, 3) make black's move and record the position. Repeat until all moves are played. TRYCATCH: rarely, there is an error in a game's notation which makes a move not legal/not a real move. So I wrap any move attempt in a TryCatch statement and throw out the offending game altogether as invalid.

for (game in 1:nrow(PGN))
{
  # create a place to store FEN positions for the game, 
  # create new chessboard with starting position
  game.FEN <- list()
  chss <- Chess$new()
  chss$load("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") #starting position
  game.FEN <- c(game.FEN,chss$fen())
  # assign some ugly things some pretty names 
  w.moves <- PGN$White.moves[game]
  b.moves <- PGN$Black.moves[game]
  
  valid.game = T
  
  for (i in 1:length(w.moves[[1]]))
  {
    
    
    if (valid.game) 
    {
      tryCatch(
        {
          # try to make a legal move for white
          chss$move(w.moves[[1]][i])
        },
        error = function(e){
          valid.game = FALSE
        }
      )
      # record the position after white's move
      game.FEN <- c(game.FEN,chss$fen())
      
      # check if there is a move for black
      if (i <= length(b.moves[[1]])) 
      {
        
        tryCatch(
          {
            # try to make a legal move for black
            chss$move(b.moves[[1]][i])
          },
          error = function(e){
            valid.game = FALSE
          }
        )
        # record the position after black's move
        game.FEN <- c(game.FEN,chss$fen())
      }
    } else 
    {
      game.FEN <- c("INVALID")
      break
    }
  }
  
  # add the game's FENs to a running list
  FEN <- c(FEN,list(game.FEN))
}

# Add it all to the df
PGN$FEN <- FEN


# We need a place to put our stuff
FASTA.colmn <- c()

for (game in 1:nrow(PGN))
{
  # assign a nicer name to the game's list of FENs
  FENs <- PGN$FEN[game]
  
  FASTAs <- str_replace_all(FENs[[1]], c("r" = "A", 
                                         "n" = "C", 
                                         "b" = "D", 
                                         "q" = "E", 
                                         "k" = "F", 
                                         "p" = "G",
                                         "B" = "V", 
                                         "1" = "I", 
                                         "2" = "II", 
                                         "3" = "III", 
                                         "4" = "IIII", 
                                         "5" = "IIIII", 
                                         "6" = "IIIIII", 
                                         "7" = "IIIIIII", 
                                         "8" = "IIIIIIII", 
                                         "/" = ""))
  
  # now that all board positions for this game are 64 chacters long, I can trim 
  # the metadata off the end based on this character length
  
  for (i in 1:length(FASTAs)) 
  {
    FASTAs[i] <- substr(FASTAs[i], 1, 64)
  }
  
  #now let's collapse the vector of individual positions into one string that 
  #represents the whole game
  FASTA <- paste(FASTAs, sep = '', collapse = '')
  # and add the new FASTA sequence to the running list
  FASTA.colmn <- c(FASTA.colmn, FASTA)
}


# and lastly add the FASTA sequences as a new column to our df
PGN$FASTA <- FASTA.colmn


# for the seqinr function 'write.fasta', it expects a vector of sequences, 
# and a vector of names, so let's make those
names <- c()
sequences <- c()

# One more time, I will loop through all the games
for (game in 1:nrow(PGN)){
  
  # This is all the information about the games I care about:
  white <- PGN$White[game]
  black <- PGN$Black[game]
  result <- PGN$Result[game]
  site <- PGN$Site[game]
  date <- PGN$Date[game]
  game.ID<- paste(white,black,date, sep = "-")
  
  # I can name the game whatever I want, but I want to be able to parse it easily later, 
  # so I am going to choose " | " as a unique separater between each piece of information
  game.name <- paste(game.ID, white, black, result, site, date, sep = " | ")
  
  # this line isn't necessary really, but it makes it more ecplicit what I'm doing
  game.sequence <- PGN$FASTA[game]
  
  # add name and sequence to their respective list
  names <- c(names, game.name)
  sequences <- c(sequences, game.sequence)
}

# Now that we have our lists, we can finally build the FASTA file.
write.fasta(as.list(sequences), names, output.filename, open = "w", as.string = FALSE)
