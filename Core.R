install.packages("stringi")
install.packages("coreNLP")
install.packages("readxl")
install.packages("quanteda")
library(stringi)
library(coreNLP)
library(readxl)
library(quanteda)

##READ IN DATA##
testdata <- data.frame(read_excel("News sample data LCM.xlsx"))
  

#' Step 1 of syntaxLCM
#' AnnotatedCorpus annotates your text to provide you with part of speech tags and dependency tree features.
#' Inputs: datacolumn (the data$column your text is in) and data (the name of your dataset)
#' Outputs: a data frame called "syntaxfeatures_output" that has 4 columns: one for the text analyzed,
#' one for POS tags, one for dependency features, and one for all generated features



#' @export
ParsedCorpus <- function(datacolumn, data){
  syntaxfeatures <- as.data.frame(matrix(NA,
                                         ncol = 4,
                                         nrow = nrow(data)))
  colnames(syntaxfeatures) <- c("text", "POStags", "dependencies", "allfeatures")
  syntaxfeatures$text <- datacolumn
  syntaxfeatures$POStags<- as.character(syntaxfeatures$POStags)
  syntaxfeatures$dependencies<- as.character(syntaxfeatures$dependencies)
  syntaxfeatures$allfeatures<- as.character(syntaxfeatures$allfeatures)
  datacolumn<- as.character(datacolumn)
  for (i in 1:length(datacolumn)){
    if (i %% 100 == 0){
      print(paste0(i," done"))
    }
    string <- datacolumn[i]
    output <- annotateString(string)
    token <- getToken(output)
    tokens <- paste(token$POS,
                    sep = " ")
    depend <- getDependency(output,
                            type = "basic")
    depends <- paste(depend$type,
                     sep = " ")
    dependlist <- paste(depend$type,
                        collapse = " ")
    poslist <- paste(token$POS,
                     collapse = " ")
    syntaxfeatures$POStags[i] <- poslist
    syntaxfeatures$dependencies[i] <- dependlist
    syntaxfeatures$allfeatures[i] <- paste(syntaxfeatures$POStags[i], syntaxfeatures$dependencies[i],
                                           collapse = " ")
  }
  syntaxfeatures.output <<- syntaxfeatures
}
#-------------------------------------------------------------------------#
# Calculate Syntax-LCM scores
#-------------------------------------------------------------------------#

initCoreNLP()
ParsedCorpus(testdata$Headline,testdata)

testdata2df <- cbind(testdata, syntaxfeatures.output)


syntaxCorpus <- function(datacolumn){
  library(tm)
  library(stringi)
  abstract.corpus <- Corpus(VectorSource(datacolumn))
  abstract.corpus.clean <- abstract.corpus %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  abstract.dtm <- DocumentTermMatrix(abstract.corpus.clean,
                                     control = list(wordLengths = c(1, Inf)))
  abstract.matrix <- as.matrix(abstract.dtm)
  syntax.dtm <<- as.data.frame(abstract.matrix)
}

syntaxCorpus(testdata2df$allfeatures) # Run this if you want to add columns with counts for each feature


# Load in LIWC dictionaries
LCMdict <- dictionary(file = "LCM.dic",
                      format = "LIWC")

syntaxLCM <- function(syntaxcol, textcol, dict){
  library(quanteda)
  library(stringi)
  liwcscore <- dfm(textcol, dictionary = dict)
  liwcdf <- convert(liwcscore, to = "data.frame")
  colnames(liwcdf) <- c("textnum","DAV2","IAV2","SV2")
  colnames(liwcdf)
  syntaxLCM.output <- as.data.frame(matrix(NA, ncol = 5, nrow = length(syntaxcol)))
  colnames(syntaxLCM.output) <- c("absadjcount", "absverbcount", "concount", "totalcount", "syntaxLCM")
  abstractadjlex <- c("amod", "compound", "cop", "expl", "JJ", "nmodnpmod") # Abstract adjective features
  abstractverblex <- c("vbz", "vbn", "mark", "xcomp", "auxpass") # Abstract verb features
  concretelex <- c( "appos","advcl","case","conj","csubj","discourse","mwe", "nnps","nsubj","nummod", "vbg") #concrete features
  for (r in 1:length(syntaxcol)){
    print(r)
    words <- strsplit(syntaxcol[r], " ")
    words <- unlist(words, use.names = FALSE)
    contained <- sapply(abstractadjlex, grepl, words)
    syntaxLCM.output$absadjcount[r] <- sum(contained[contained == "TRUE"])
    contained <- sapply(abstractverblex, grepl, words)
    syntaxLCM.output$absverbcount[r] <- sum(contained[contained == "TRUE"])
    contained <- sapply(concretelex, grepl, words)
    syntaxLCM.output$concount[r] <- sum(contained[contained == "TRUE"])
    syntaxLCM.output$totalcount[r] <- sum(syntaxLCM.output$absadjcount[r], syntaxLCM.output$absverbcount[r], syntaxLCM.output$concount[r], liwcdf$SV2[r], liwcdf$DAV2[r], liwcdf$IAV2[r], na.rm = TRUE)
  }
  syntaxLCM.output$syntaxLCM <- ((syntaxLCM.output$absadjcount*4) + (liwcdf$SV2*3) + (syntaxLCM.output$absverbcount*2) + ((liwcdf$IAV2)*2) + (syntaxLCM.output$concount) + (liwcdf$DAV2)) / (syntaxLCM.output$totalcount)
  syntaxLCM.output <<- syntaxLCM.output
}

syntaxLCM(syntaxcol = testdata2df$allfeatures, textcol = testdata2df$Headline, dict = LCMdict)
testdata2df <- cbind(testdata2df, syntaxLCM.output, syntax.dtm)

#-------------------------------------------------------------------------#
# Calculate LIWC LCM Scores
#-------------------------------------------------------------------------#


liwcscore <- dfm(testdata2df$Headline, dictionary = LCMdict)
liwcdf <- as.data.frame(liwcscore)
colnames(liwcdf) <- c("Text", "DAV2","IAV2","SV2")
colnames(liwcdf)
testdata2df <- cbind(testdata2df,liwcdf)
testdata2df$liwcscore <- (((testdata2df$nn) * 5 + 
                             (testdata2df$jj) * 4 + 
                             (testdata2df$DAV2) * 1 + 
                             (testdata2df$IAV2) * 2 + 
                             (testdata2df$SV2) * 3)) / (testdata2df$nn + 
                                                          testdata2df$jj + 
                                                          testdata2df$DAV2 + 
                                                          testdata2df$IAV2 + 
                                                          testdata2df$SV2)

testdata2df$zliwcscore <- scale(testdata2df$liwcscore, 
                           center = TRUE, 
                           scale = TRUE)
