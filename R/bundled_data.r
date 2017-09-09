#' word frequency dataset (5k most frequent english words in COCA) 
#'
#' a dataset containing corpus freqency, pos, freq rank, and dispersion for the 5k most frequent words in the corpus of contemporary american english (COCA)
#'
#' @format a data frame with 5000 rows and five columns
#' \describe{
#'   \item{Rank}{a word's frequency rank in COCA}
#'   \item{Word}{an english word}
#'   \item{PartOfSpeech}{a part of speech (see COCA documentation for codes)}
#'   \item{Frequency}{number of occurrences in COCA}
#'   \item{Dispersion}{dispersion score for the word (see COCA documentation)}
#' }
#' @source \url{https://www.wordfrequency.info/}
"dataset_word_freq"



#' full text of the unabomber manifesto, written by ted kaczynski ca. 1995
#'
#' full text of the unabomber manifesto, broken down by "sections." originally published by the washington post. useful for practicing text analysis techniques. total word count is ~35k. lol. 
#'
#' @format a length-314 character vector, where each element is a section/paragraph of the unabomber manifesto.
#' 
#' @source \url{http://www.washingtonpost.com/wp-srv/national/longterm/unabomber/manifesto.text.htm}
"text_una_manifesto"



#' the "google memo," written by james damore in 2017. 
#'
#' the full text of james damore's "google memo," useful for demonstration of text analysis techniques. 
#'
#' @format a single string containing the entire google memo
#' 
#' @source \url{http://gizmodo.com/exclusive-heres-the-full-10-page-anti-diversity-screed-1797564320}
"text_googmem"

