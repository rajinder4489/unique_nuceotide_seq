### User defined variables start ###

no_of_words_to_generate <- 96
word_length <- 6
min_diff <- 3 #minimum difference in letters (wrt position) in the words

letters <- c("A", "T", "G", "C")

### User defined variables end ###

### Main starts ###

distance <- word_length - min_diff

all_words <- c()
discarded_words <- c()
counter <- 0

while(length(all_words) < no_of_words_to_generate)
{
  word <- ""
  while(nchar(word) < word_length)
  {
    i <- sample(1:4, 1)     #randomly selecting a letter to make a 6 letter word
    word <- paste(word, letters[i], sep="")
  }
  
  if(length(all_words) == 0)
  {
    all_words <- c(all_words, word)   #Add the first word
  }
  else
  {
    if(word %in% discarded_words)   #If already in discarded do not process this word further
    {next}
    else
    {
      if(min(sapply(all_words, function(x) adist(word, x))) >= distance)    #Discarding on the basis on distance between the new word and processed words 
      {
        all_words <- c(all_words, word)
      }
      else  #discarded
      {
        discarded_words <- c(discarded_words , word)
        counter <- counter+1
        
        if(counter == length(letters)^word_length)  #if all possible combinations are generated and still no of words to generate is not achieved, break the loop
        {break}
      }
    }
  }
}
print(all_words)
print(paste("Words found:", length(all_words), sep=" "))

### Main ends ###