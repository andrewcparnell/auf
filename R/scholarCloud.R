#' Word clouds from Google Scholar IDs
#'
#' Take the Google Scholar IDs of a person and create a word cloud of their paper titles
#'
#' @param scholarId The ID number of person's Google scholar page. This is usually an alphanumeric code in the URL of their Google scholar page
#' @param minFreq The minimum frequency of words used in the word cloud
#' @param maxWords The maximum number of words to show on the page
#' @param colours The colour palette
#'
#' @return Returns a word cloud plot
#' @export
#'
#' @examples
#' \dontrun{
#' id = 'mXSv_1UAAAAJ' # Leo Brieman
#' scholarCloud(id)
#' }
scholarCloud = function(scholarId,
                        minFreq = 1,
                        maxWords = 100,
                        colours = viridis::viridis(8))
{

  # Get the link using the scholar package
  pubs = paste(as.character(scholar::get_publications(scholarId)$title),
               collapse= ' ')

  # Remove stop words and de-capitalise etc
  myCorpus = tm::Corpus(tm::VectorSource(pubs))
  myCorpus = tm::tm_map(myCorpus, tm::content_transformer(tolower))
  myCorpus = tm::tm_map(myCorpus, tm::removePunctuation)
  myCorpus = tm::tm_map(myCorpus, tm::removeNumbers)
  myCorpus = tm::tm_map(myCorpus, tm::removeWords,
                        tm::stopwords("SMART"))

  myDTM = tm::TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  m2 = sort(rowSums(m), decreasing = TRUE)

  # wordcloud::wordcloud(names(m2), m2,
  #               min.freq = minFreq,
  #               max.words = maxWords,
  #               colors = colours)
  wordcloud::wordcloud(words = names(m2), freq = m2,
                       min.freq = minFreq,
                       max.words=maxWords,
                       random.order=FALSE,
                       rot.per=0.1,
            colors=colours)


}
