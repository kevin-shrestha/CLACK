library(devtools)
library(tidyverse)
library(quanteda)
library(jsonlite)
library(rlist)

library(wordcloud)
library(RColorBrewer)

# Package environment variables
env = new.env()

# Number of prefix/suffix context words to seek for while analysing keyword in context.
# By default, searches for 4 words before and after the keyword
env$kwic.size = 4

# Regex expression to remove words before or after a punctuation while analysing keyword in context
# By default removes everything before a fullstop for prefix and removes everything after a fullstop for suffix
env$kwic.regex.remove.pre = ".*\\."
env$kwic.regex.remove.post = "\\..*"

#' Set number of prefix and suffix context words to be analysed for keyword in context
#' @param integer input integer value > 0
#' @return numeric value 0 if successfully changed or -1 if not successful
#' @seealso \code{\link{kwic}} value set by this function is used by "window" parameter of the function
#' @export
#' @examples
#' Set_KWIC_Size(4)
Set_KWIC_Size <- function(size){
  if(is.integer(size)) {
    if (size > 0) {
      env$kwic.size = size
      return(0)
    } else {
      print("ERROR: Input needs to be greater than 0")
      return(-1)
    }

  }
  print("ERROR: Input needs to be an integer value")
  return(-1)
}

#' Set regex string to cleanse prefix words extracted
#' @param regex.string string - regex expression to be used
#' @return NULL
#' @export
#' @examples
#' Set_KWIC_Prefix_Remove_Regex(".*\\.")
Set_KWIC_Prefix_Remove_Regex <- function(regex.string){
  env$kwic.regex.remove.pre = regex.string
}

#' Set regex string to cleanse suffix words extracted
#' @param regex.string string - regex expression to be used
#' @return NULL
#' @export
#' @examples
#' Set_KWIC_Suffix_Remove_Regex("\\..*")
Set_KWIC_Suffix_Remove_Regex <- function(regex.string){
  env$kwic.regex.remove.post = regex.string
}

#' Checks if classification exists for a keyword
#' @description Check if any classification can be applied to the text corpus for a given keyword and its context descriptions. Meant to be used internally by other functions.
#' @param text character vector - contains text corpus to be searched
#' @param checking.df dataframe - containing json keyword config information
#' @return list of lists containing classifications found
#' @export
#' @examples
#' Check_Keyword(text_corpus, json_dataframe)
Check_Keyword <- function(text, checking.df){
  text <- tolower(text)
  output.list <- list()

  # Extracting information from dataframe containing data from json input
  title <- checking.df[1,"title"]
  keywords <- unlist(checking.df[1,"keywords"])
  approx.match <- unlist(checking.df[1,"approx.match"])
  false.positive.matches <- unlist(checking.df[1,"false.positive.matches"])
  prefix <- unlist(checking.df[1,"prefix"])
  suffix <- unlist(checking.df[1,"suffix"])
  prefix.approx.match <- unlist(checking.df[1,"prefix.approx.match"])
  suffix.approx.match <- unlist(checking.df[1,"suffix.approx.match"])
  prefix.descriptions <- unlist(checking.df[1,"prefix.descriptions"])
  suffix.descriptions <- unlist(checking.df[1,"suffix.descriptions"])
  prefix.class <- unlist(checking.df[1,"prefix.class"])
  suffix.class <- unlist(checking.df[1,"suffix.class"])
  ignore.prefix <- unlist(checking.df[1,"ignore.prefix"])
  ignore.suffix <- unlist(checking.df[1,"ignore.suffix"])
  ignore.prefix.approx.match <- unlist(checking.df[1,"ignore.prefix.approx.match"])
  ignore.suffix.approx.match <- unlist(checking.df[1,"ignore.suffix.approx.match"])


  # Error checking to check whether length of arrays match up
  if(length(keywords) != length(approx.match)){
    print(paste("ERROR: unmatched length for keyword list and approx.match list for Title:",title))
  }

  if(length(prefix.approx.match) != length(prefix) || length(prefix.descriptions) != length(prefix) || length(prefix.class) != length(prefix)){
    print(paste("ERROR: unmatched prefix information error for Title:",title))
  }
  if(length(suffix.approx.match) != length(suffix) || length(suffix.descriptions) != length(suffix) || length(suffix.class) != length(suffix)){
    print(paste("ERROR: unmatched prefix information error for Title:",title))
  }

  # Extract all possible correct and incorrect representations of the keywords in the text
  checklist = list()

  for(keyword.index in 1:length(keywords)) {
    input.text = text
    checklist = unlist(list(checklist, Get_Occurrences(input.text, keywords[keyword.index], approx.match[keyword.index])))
  }

  # Remove false positive keywords that were picked up by fuzzy text matching
  checklist = checklist[!checklist %in% false.positive.matches]
  if(is.null(checklist) || is_empty(checklist)){
    return(output.list)
  }

  # For each item in the checklist of keywords to look for, analyse its context
  for(keyword.index in 1:length(checklist)){
    word = checklist[keyword.index]
    word = gsub("([\\])","", word)
    word = paste("*",word, "*", sep="")

    # get suffix and prefixes of the keyword
    kwic_output <- kwic(as.character(text), phrase(as.character(word)), window = env$kwic.size, valuetype = "glob")

    if(nrow(kwic_output)>0){

      for(i in 1: nrow(kwic_output)){
        # remove everything before a fullstop in prefix and remove everything after a fullstop in the suffix
        pre <- gsub(env$kwic.regex.remove.pre,"",kwic_output[i,"pre"])
        post <- gsub(env$kwic.regex.remove.post,"",kwic_output[i, "post"])


        # for each item on the list of negation prefix to check, check if the item exists, ignore the current occurence of the keyword
        go.next = FALSE
        if(length(ignore.prefix) > 0){
          for(index in 1:length(ignore.prefix)){
            if(agrepl(paste("\\b\\Q",ignore.prefix[index],"\\E\\b",sep=""), pre, ignore.case=TRUE, fixed = FALSE, max.distance = ignore.prefix.approx.match[index])){
              go.next = TRUE
            }
          }
        }

        # for each item on the list of negation suffix to check, check if the item exists, ignore the current occurence of the keyword
        if(length(ignore.suffix) > 0){
          for(index in 1:length(ignore.suffix)){
            if(agrepl(paste("\\b\\Q",ignore.suffix[index],"\\E\\b",sep=""), post, ignore.case=TRUE, fixed = FALSE, max.distance = ignore.suffix.approx.match[index])){
              go.next = TRUE
            }
          }
        }

        # for each item on the list of prefix to check, check if the item exists, if so append an item to the list thats being returned to capture its occurence
        if(length(prefix) > 0 && !go.next){
          for(index in 1:length(prefix)){
            if(agrepl(paste("\\b\\Q",prefix[index],"\\E\\b",sep=""), pre, ignore.case=TRUE, fixed = FALSE, max.distance = prefix.approx.match[index])){
              output.list = output.list %>% list.append( list(category=prefix.class[index], priority=0, description=prefix.descriptions[index]) )
            }
          }
        }

        # for each item on the list of suffix to check, check if the item exists, if so append an item to the list thats being returned to capture its occurence
        if(length(suffix) > 0 && !go.next){
          for(index in 1:length(suffix)){
            if(agrepl(paste("\\b\\Q",suffix[index],"\\E\\b",sep=""), post, ignore.case=TRUE, fixed = FALSE, max.distance = suffix.approx.match[index])){
              output.list = output.list %>% list.append( list(category=suffix.class[index], priority=0, description=suffix.descriptions[index]) )
            }
          }
        }

      }



    }

  }
  return(output.list)
}

#' Function that extracts all perturbations of a given word in a text corpus accounting for mispellings
#' @description Function that extracts all perturbations of a given word in a text corpus accounting for mispellings
#' @param text Character vector - contains text corpus to be searched
#' @param word string - keyword to be looked for
#' @param distance numerical - sensitivity of the fuzzy text matching (see agrep argument max.distance)
#' @return character vector containing all perturbations of the keyword found in the text corpus
#' @export
#' @examples
#' Get_Occurences("Pt has heavy chest pain", "chest pain", 0.1)
Get_Occurrences <- function(text, word, distance){

  text = paste(text, collapse = " ", sep = " ")

  final.list  = list()
  continue.search = TRUE
  while(continue.search){
    # Extract occurence of the word using fuzzy matching
    temp.list = unlist(regmatches(text, aregexec(paste("\\b\\Q",word,"\\E\\b",sep=""), text, max.distance = distance, ignore.case = T)))

    # check if any occurence of the word exists and if it doesnt stop searching
    if(length(temp.list) == 0){
      continue.search = FALSE
    } else {
      final.list = unlist(list(final.list,temp.list))
      # replace the occurence in the text
      text = gsub(paste("\\b\\Q",temp.list,"\\E\\b",sep=""),"",text,ignore.case = T)
    }
  }
  return(final.list)
}

#' Perform the classification of the text corpuses
#' @description Perform the classification of the text corpuses
#' @param text.df dataframe - dataframe containing the text corpuses
#' @param column.name string - name of the column containing the text corpus within the provided dataframe
#' @param json.file.path string - full file path of the json file
#' @return dataframe provided in the first argument with columns CLASSIFICATION and CLASSIFICATION_REASON added
#' @export
#' @examples
#' Classify_Text(text.corpus.dataframe, "CHARTED_VALUE", "../keywords.json")
Classify_Text <- function(text.df,column.name, json.file.path) {

  # Convert column of interest into character column and add 2 more columns, one for classification and other for justification of the classification
  converted.column.name = rlang::sym(column.name)
  text.df <- text.df %>% mutate(!!converted.column.name := as.character(!!converted.column.name)) %>% tibble::add_column(CLASSIFICATION="") %>% tibble::add_column(CLASSIFICATION_REASON="")

  # convert text json input into a dataframe
  json.input = jsonlite::fromJSON(txt = paste(readLines(json.file.path), collapse="") )

  # for each row of the dataframe we would like to run the text analyis, run the Check_Keyword function
  for(i in 1:nrow(text.df)){

    listings <- list()
    for(j in 1:nrow(json.input)){
      listapp = Check_Keyword(tolower(iconv(text.df[i,c(column.name)], to="UTF-8")), json.input[j,] )
      if(!is.null(listapp)){
        for(item in listapp){
          listings <- listings %>% list.append(item)
        }
      }
    }

    # Save output to the CLASSIFICATION and CLASSIFICATION_REASON column
    if (length(listings) > 0){
      listings <- bind_rows(listings)
      text.df[i,"CLASSIFICATION"] <- paste(unique(unlist(listings["category"])), sep=",", collapse=",")
      text.df[i,"CLASSIFICATION_REASON"] <- paste(unique(unlist(listings["description"])), sep=",", collapse=",")
    } else {
      text.df[i,"CLASSIFICATION"] <- "Unclassified"
    }
  }
  return(text.df)

}


#' Create frequency list for keyword perturbation in text corpus
#' @description Create frequency list fo all different representations/mispellings/perturbations that exist in an array or single text block
#' @param textblock.array character vector - containing list of texts to be searched
#' @param word string - keyword to be looked for
#' @param distance numerical - sensitivity of the fuzzy text matching (see agrep argument max.distance)
#' @return dataframe containing keyword perturbations and its frequency
#' @export
#' @examples
#' Create_Perturbation_Freq_List(text.corpus.dataframe$column.of.interest, "chest pain", 0.1)
Create_Perturbation_Freq_List <- function (textblock.array, word, distance){
  text = paste(textblock.array, collapse = " ", sep = " ")
  final.list  = list()
  continue.search = TRUE
  while(continue.search){
    # Extract occurence of the word using fuzzy matching
    temp.list = unlist(regmatches(text, aregexec(paste("\\b\\Q",word,"\\E\\b",sep=""), text, max.distance = distance, ignore.case = T)))

    # replace the occurence in the text
    text = sub(paste("\\b\\Q",temp.list,"\\E\\b",sep=""),"",text,ignore.case = T)

    # check if any occurence of the word exists and if it doesnt stop searching
    if(length(temp.list) == 0){
      continue.search = FALSE
    } else {
      final.list = unlist(list(final.list,temp.list))
    }
  }

  output = data.frame(keyword.found = tolower(final.list)) %>% group_by(keyword.found) %>% summarise(n=n())
  return(output)
}


#' Plot a word cloud
#' @description Plot a word cloud
#' @param words character vector - contains words to be plotted
#' @param freq character vector - contains word's frequency
#' @param min.freq integer - words with frequency below min.freq will not be plotted
#' @param max.words integer - maximum number of words to be plotted. least frequent terms dropped
#' @param random.order boolean - plot words in random order. If false, they will be plotted in decreasing frequency
#' @param rot.per numerical - proportion words with 90 degree rotation
#' @param colors RColorBrewer - color words from least to most frequent
#' @return NULL
#' @export
#' @examples
#' Create_Wordcloud(words = wordslist, freq = freqlist, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#' Create_Wordcloud(words = wordslist, freq = freqlist)
Create_Wordcloud<- function(words, freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")){
  wordcloud(words = words, freq = freq, min.freq = min.freq , max.words=max.words, random.order=random.order, rot.per=rot.per, colors=colors)
}
