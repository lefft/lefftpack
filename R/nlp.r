# currently have: 
#   - text2bigram()
#   - scrub_doc()
#   - tf()
#   - idf()
#   - tf_idf()
#   - remove_stops()
#   - word_count_est()
#   - tokenize_text()
# 
# working on integrating: 
#   - bigram_plot()
#   - document_intersection()
# 
# TODO: 
#   - add more gram size options to everything
#   - call scrub_doc() inside of text2bigram()
#   - eliminate all non-base calls unless performance wd suffer greatly
#     (currently just have a couple of dplyr:: calls)
#   - better arg passing -- use `...` more
# 
# notes: 
#   - refactor + use `tokenize_text()` as primary workhorse (july25/2018)
#   - only `gram_size`=1 currewntly supported for tf-idf (aug13/2017)
#   - for doc intersection, need to convert to base-only internals




### tokenize_text(strings, ngram, split_re=" ", ...) -------------------------

#' Tokenize text 
#' 
#' Split each element of a character vector by `split_re` into its constituent 
#' `ngram` tokens. 
#'
#' @param strings character vector of text documents to be tokenized. 
#' @param ngram positive integer specifying size of ngram chunks. 
#' @param split_re regular expression denoting the token boundary to split strings by. 
#' @param ... named arguments passed to `strs;lit()` 
#'
#' @return if `length(strings)==1`, returns a character vector of `ngram` tokens. If `length(strings) > 1`, returns a list each of whose elements is a character vector of `ngram` tokens. 
#' @export
#'
#' @examples {
#'   string <- "hai mi name timi + me girl nam dootza--tza"
#'   tokenize_text(string, 1)
#'   tokenize_text(string, 2)
#'   lapply(1:3, function(x) tokenize_text(string, x))
#'   tokenize_text(string, 2, "[ -]")
#'   tokenize_text("me.lava.me.dootzi", 3, "\\.")
#'   tokenize_text("me.lava.me.dootzi", 3, ".", fixed=TRUE)
#'   tokenize_text(rep("me.lava.me.dootzi", 2), 3, ".", fixed=TRUE)
#'   tokenize_text(c(string, "waow me fillin heppi meby beby"), 3)
#'   tokenize_text(c(string, "waow me fillin heppi meby beby", NA), 3)
#'   tokenize_text(c(string, "waow me fillin heppi meby beby", ""), 3)
#'   tokenize_text(NA, 3)
#' }
tokenize_text <- function(strings, ngram, split_re=" ", ...){
  # TODO: smthg like: `gsub(paste0("(", split_re, ")+"), split_re, strings)`
  if (length(strings) > 1)
    return(lapply(strings, function(s) tokenize_text(s, ngram, split_re, ...)))
  if (is.na(strings)) return(NA_character_)
  unigrams <- strsplit(as.character(strings), split=split_re, ...)[[1]]
  ngram <- as.integer(ngram)
  if (ngram > length(unigrams)) return(character(0))
  stopifnot(ngram >= 1L, ngram <= length(unigrams))
  start_idxs <- 1:(length(unigrams)-(ngram-1))
  ngrams <- sapply(start_idxs, function(idx){
    paste(unigrams[idx:(min(idx+(ngram-1), length(unigrams)))], collapse=" ")})
  return(as.character(ngrams))
}



### word_count_est(doc, unique=FALSE) -----------------------------------------

#' estimate the number of words in a document (total or unique)
#'
#' @param doc string or vector of strings or list of strings
#' @param unique boolean
#'
#' @return numeric, word count (total or unique)
#' @export
#'
#' @examples word_count_est(doc="my name is tim and blah blah blah", unique=TRUE)
word_count_est <- function(doc, unique=FALSE){
  doc <- paste(doc, collapse=" ")
  words <- unlist(strsplit(doc, split=" "))
  if (!unique){
    return(length(words))
  } else {
    return(length(unique(words)))
  }
}

### remove_stops(doc, stops) --------------------------------------------------

#' remove a list of words from a document (case- and punctuation-sensitive)
#' (to deal with punctuation etc., call `scrub_doc()` on the `doc` arg before)
#'
#' @param doc 
#' @param stops 
#'
#' @return the document, with all elements of `stops` removed
#' @export
#'
#' @examples remove_stops(doc="hi i am tim, tim like goat", stops=c("is", "tim"))
remove_stops <- function(doc, stops){
  doc_string <- paste(doc, collapse=" ")
  doc_words <- unlist(strsplit(doc_string, split=" "))
  # remove empty chars
  doc_words <- doc_words[!doc_words %in% c("", " ")]
  # remove the stop words
  doc_words <- doc_words[!doc_words %in% stops]
  # put it back together
  doc_reassembled <- paste(doc_words, collapse=" ")
  # and return
  return(doc_reassembled)
}




### tf(doc, term, gram_size=1) ------------------------------------------------

#' term frequency: get the frequency of term in doc (for gram_size chunks)
#' NOTE: only `gram_size=1` is implemented right now (i.e. words)
#' NOTE: want to add option of stemming directly from a call to `tf()` 
#'
#' @param doc 
#' @param term 
#' @param lower 
#' @param toss_punct 
#' @param toss_num 
#' @param gram_size 
#'
#' @return numeric, frequency of term in document 
#' @export
#'
#' @examples tf(doc="blah blah im tim my name is tim", term="tim")
tf <- function(doc, term, 
               lower=TRUE, toss_punct=TRUE, toss_num=TRUE, gram_size=1){
  # clean it up before tokenizing
  doc <- scrub_doc(doc, lower, toss_punct, toss_num)
  # tokenize it
  tokens <- unlist(strsplit(doc, split=" "))
  # get the doc length [build in >1 grams later]
  doc_length <- length(tokens)
  # occurrences of term [ASSUMES LOWERCASE]
  term_occurrences <- sum(tokens==term)
  # term frequency
  term_freq <- term_occurrences / doc_length
  # return the term freq
  return(term_freq)
}



### idf(docs, term, gram_size=1) ----------------------------------------------

#' inverse document frequency: log of num docs / num docs w term
#'
#' @param docs 
#' @param term 
#' @param lower 
#' @param toss_punct 
#' @param toss_num 
#' @param gram_size 
#'
#' @return num, the inverse doc freq
#' @export
#'
#' @examples idf(docs=c("hey im tim", "me too!", "tim tim is tim",) term="tim")
idf <- function(docs, term, 
                lower=TRUE, toss_punct=TRUE, toss_num=TRUE, gram_size=1){
  # number of docs
  n_docs <- length(docs)
  # scrub the docs before comparing
  docs_scrubbed <- lapply(docs, function(d){
    scrub_doc(d, lower=lower, toss_punct=toss_punct, toss_num=toss_num)
  })
  # number of docs containing term
  # or:  sum(sapply(docs_scrubbed, function(d) grepl(pattern=term, d)))
  n_term_docs <- sum(grepl(term, docs_scrubbed))
  # send a message if term doesnt appear in docs
  if (n_term_docs==0){
    message("watch out -- term doesnt appear in docs! (so idf==`Inf`)")
  }
  # compute idf and return it
  inv_doc_freq <- log(n_docs / n_term_docs)
  return(inv_doc_freq)
}


### tf_idf(doc, docs, term, gram_size=1) --------------------------------------

#' tf_idf: how important is a word in a doc, relative to a set of docs (tf*idf)
#'
#' @param doc 
#' @param docs 
#' @param term 
#' @param lower 
#' @param toss_punct 
#' @param toss_num 
#' @param gram_size 
#'
#' @return num, term freq times inv doc freq
#' @export
#'
#' @examples tf_idf(doc="im tim", docs=c("it is tim", "im tim"), term="tim")
tf_idf <- function(doc, docs, term, 
                   lower=TRUE, toss_punct=TRUE, toss_num=TRUE, gram_size=1){
  # calculate term frequency
  term_freq <- tf(doc, term, 
                  lower=lower, toss_punct=toss_punct, toss_num=toss_num)
  # calculate inverse document frequency
  inv_doc_freq <- idf(docs, term, 
                      lower=lower, toss_punct=toss_punct, toss_num=toss_num)
  # return their product
  return(term_freq * inv_doc_freq)
}


### scrub_doc(doc, lower, toss_punct, toss_num) -------------------------------

#' quick func to take a messy doc, clean it up, and return it as a string 
#' (tokenizes as an intermediate step, but meant for just quick cleaning)
#'
#' @param doc 
#' @param lower 
#' @param toss_punct 
#' @param toss_num 
#'
#' @return the doc as a string, cleaned up
#' @export
#'
#' @examples scrub_doc(doc="H3LL0 me NamE is. t1m timmy", lower=TRUE, toss_punct=TRUE, toss_num=TRUE)
scrub_doc <- function(doc, lower, toss_punct, toss_num){
  doc_string <- paste(doc, collapse=" ")
  doc_words <- unlist(strsplit(doc_string, split=" "))
  if (lower){doc_words <- tolower(doc_words)} 
  if (toss_punct){doc_words <- gsub("[[:punct:]]", "", doc_words)}
  if (toss_num){doc_words <- gsub("[0-9]", "", doc_words)}
  doc_words <- doc_words[!doc_words %in% c("", " ")]
  return(paste(doc_words, collapse=" "))
}

### text2bg_count(text_string) ------------------------------------------------

#' convert a document (or doc filepath) to a df of bigrams or bigram counts
#'
#' @param doc 
#' @param is_path 
#' @param stops 
#' @param lower 
#' @param toss_punct 
#' @param toss_num 
#' @param return_counts 
#' @param quiet 
#'
#' @return a data frame with columns for bigram, w1, w2, and count (or just the bigrams and document location indices if `return_counts=FALSE`)
#' @export
#'
#' @examples text2bigram(doc="INDUSTRIAL SOCIETY AND ITS FUTURE  Introduction  1. The Industrial Revolution and its consequences have been a disaster for the human race.")
text2bigram <- function(doc,           # doc is the only obligatory argument
                        is_path=FALSE, # you can also feed in an external file
                        stops=NULL, lower=TRUE, toss_punct=TRUE, toss_num=TRUE,
                        return_counts=TRUE, quiet=FALSE){
  
  # if `doc` is a filepath, read it in as `doc` and proceed
  if (is_path){
    doc <- readLines(doc)
  }
  
  # get the doc as a single character string
  doc_string <- paste(doc, collapse=" ")
  
  # convert to char vector of 'words' -- sequences separated by " "
  doc_words <- unlist(strsplit(doc_string, split=" "))
  
  # lowercase everything (if lower==TRUE)
  if (lower){
    doc_words <- tolower(doc_words)
  } 
  
  # remove punctuation (if toss_punct==TRUE)
  if (toss_punct){
    doc_words <- gsub("[[:punct:]]", "", doc_words)
  }
  
  # toss numerals (if toss_num==TRUE) 
  if (toss_num){
    doc_words <- gsub("[0-9]", "", doc_words)
  }
  
  # remove empty chars, no matter wha
  doc_words <- doc_words[!doc_words %in% c("", " ")]
  
  # how many bigrams will there be (including potential stopwords)??
  bg_len_out <- length(doc_words) - 1
  
  # how many leading zeros do we need in the id's?
  lz <- nchar(as.character(bg_len_out))
  
  # make a df with all the bigrams, sequentially as they appear
  bg_df <- data.frame(
    # index each bigram with its location in the text
    idx = paste0("bg_", sprintf(paste0("%0", lz, "d"), seq_len(bg_len_out))),
    w1  = doc_words[1:(bg_len_out)], 
    w2  = doc_words[2:(bg_len_out + 1)], 
    stringsAsFactors=FALSE
  )
  
  # if stoplist is supplied, remove the rows with stops
  if (!is.null(stops)){
    
    # how many bigrams before removing stops?
    nrow_withstops <- nrow(bg_df)
    
    # remove rows with a stopword as first or second component
    bg_df <- bg_df[!(bg_df$w1 %in% stops | bg_df$w2 %in% stops), ]
    
    # just some fyi action, if !quiet
    if (!quiet){
      message(paste0(nrow(bg_df), " bigrams left ", 
                     "(out of ", nrow_withstops, " raw), ", 
                     " after removing ", length(stops), " stopwords"))
    }
  }
  
  # add bigram as a column to split + count over 
  bg_df$bigram <- paste(bg_df$w1, bg_df$w2, sep=" ")
  
  # if you just want the tokenized text + not counts, just return the bigram df
  if (!return_counts){
    return(bg_df)
  }
  
  # if we're still going, then compute bigram frequencies
  # NOTE: CAREFUL W USING DPLYR VERBS HERE -- I THINK ITS OKAY BUT CHECK AGAIN
  bg_counts <- data.frame(dplyr::summarize(
    dplyr::group_by(bg_df, bigram), count = length(bigram)
  ))
  
  # then sort so highest counts are first, ties are alphabetically broken
  bg_counts <- bg_counts[order(bg_counts$bigram, decreasing=FALSE), ]
  bg_counts <- bg_counts[order(bg_counts$count, decreasing=TRUE), ]
  
  # strip row names
  row.names(bg_counts) <- NULL
  
  # aaaaand return the sorted bigram freq df!
  return(bg_counts)
}





#' get one of (currently) five different lists of stopwords
#'
#' @param stop_list can be: 'una', 'bible', 'snowball', 'onix', or 'SMART'
#'
#' @return a vector of common English function words to remove from text before computing on it 
#' @export
#'
#' @examples get_stop_list(stop_list="una")
get_stop_list <- function(stop_list=c("una","bible",
                               "snowball","onix","SMART")){
  if (length(stop_list) != 1){
    message(paste0(
      "gotta pick a list of stopwords!\n  ",
      "u can has: 'una', 'bible', 'snowball', 'onix', or 'SMART'"))
    return(NULL)
  }
  if (stop_list=="bible"){
    # a list im making up for bible
    bible_stops <- c(
      "the","and","of","to","that","shall","unto","for","his","a","they",
      "be","is","him","them","it","with","all","thou","thy","was","which",
      "my","me","but","ye","their","have","thee","from","as","are","when",
      "this","out","were","by","you","up","there","hath","then","had","into",
      "on","also","an","at","or","said","saith","about","with","i",
      "we","us","did","our","these","those","if",
      "will","went","in" # <-- dk about deez
    )
    return(bible_stops)
  }
  if (stop_list=="una"){
    # a list i made up based on top words in `una` (looked ahead)
    # (get uniques in case i make dupes on accident when adding...)
    una_stops <- unique(c(
      "the","of","to","a","and","that","be","it","or","as","which","in","he",
      "there","we","been","who","do","does","this","they","is","are","at",
      "have","if","for","an","so","by","their","with","on","when","than",
      "about","but","you","would","one","its","has","may","i",
      # these mite be interesting tho...
      "from","can","how","much","no","these","into","many"
    ))
    return(una_stops)
  } else {
    # get the tt stop sets
    tt_stops <- tidytext::stop_words
    return(tt_stops$word[tt_stops$lexicon==stop_list])
  }
}


### doc_intersection(doc1, doc2, ----------------------------------------------
###                  doc1_name="doc1", doc2_name="doc2", stoplist="")
# *** this one needs to be base-ified before can be included...



### bigram_plot(bigram_df, top_n=nrow(bigram_df), tit="", subtit="", ----------
#               arrow=nice_arrow(), remove_stops=FALSE, stops=NULL, 
#               text_size=5, vjust=1, hjust=1)
# *** this one needs to be base-ified before can be included...







