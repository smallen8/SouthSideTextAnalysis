library(dplyr)
library(janeaustenr)
library(tidytext)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
data("stop_words")
library(igraph)
library(ggraph)
set.seed(2022)
library(widyr)

# Put QR code on doors of residences
# Get age, stage of life, number of kids, number of people in residents, block

# Load data.
loadData <- function() {
  raw_text <- read.csv('text.csv')
  raw_text$neighbor_not_anonymous <- NULL
  neighbor_metadata <- read.csv('neighbor_metadata.csv')
  neighbor_metadata$neighbor_not_anonymous <- NULL
  
  text_df <- raw_text %>%
    left_join(neighbor_metadata, by='neighbor') %>%
    filter(!is.na(text), trimws(text)!='') %>%
    mutate(text = gsub('[^A-Za-z0-9. ]', '', text),
           line_num = row_number()) %>%
    as_tibble()
  
  return(text_df)
}
text_df <- loadData()
# View(text_df)

# Count words. Group by neighbor, age group, stage of life, etc.
countWords <- function(text_df, groupByVar = "Don't Summarize") {
  # Tokenize by sentence to get sentence number for later word association.
  tidy_text <- text_df %>%
    unnest_tokens(sentence, text, token='sentences') %>%
    mutate(sentence_num = row_number())
  
  if(groupByVar=="Don't Summarize") {
    wordCount <- tidy_text %>%
      unnest_tokens(word, sentence, token='words') %>%
      anti_join(stop_words) %>%
      count(word, sort=T)
  } else {
    wordCount <- tidy_text %>%
      unnest_tokens(word, sentence, token='words') %>%
      anti_join(stop_words) %>%
      count(word, !!sym(groupByVar), sort=T)
  }
  
  return(wordCount)
}

countBigrams <- function(text_df, groupByVar = "Don't Summarize") {
  tidy_text <- text_df %>%
    unnest_tokens(sentence, text, token='sentences') %>%
    mutate(sentence_num = row_number()) %>%
    unnest_tokens(bigram, sentence, token='ngrams', n=2) %>%
    mutate(bigram_tmp = bigram) %>%
    separate(bigram_tmp, c('word1','word2'), sep=' ') %>%
    mutate(word1_stop = ifelse(word1 %in% stop_words$word, T, F),
           word2_stop = ifelse(word2 %in% stop_words$word, T, F)) %>%
    filter(!(word1_stop | word2_stop)) %>%
    filter(!is.na(bigram))
  
  if(groupByVar=="Don't Summarize") {
    bigramCount <- tidy_text %>%
      count(bigram, sort=T)
  } else {
    bigramCount <- tidy_text %>%
      count(bigram, !!sym(groupByVar), sort=T)
  }
  
  return(bigramCount)
}

countTrigrams <- function(text_df, groupByVar = "Don't Summarize") {
  tidy_text <- text_df %>%
    unnest_tokens(sentence, text, token='sentences') %>%
    mutate(sentence_num = row_number()) %>%
    unnest_tokens(trigram, sentence, token='ngrams', n=3) %>%
    mutate(trigram_tmp = trigram) %>%
    separate(trigram_tmp, c('word1','word2','word3'), sep=' ') %>%
    mutate(word1_stop = ifelse(word1 %in% stop_words$word, T, F),
           word2_stop = ifelse(word2 %in% stop_words$word, T, F),
           word3_stop = ifelse(word3 %in% stop_words$word, T, F)) %>%
    filter(word1_stop+word2_stop+word3_stop==0) %>%
    filter(!is.na(trigram))
  
  if(groupByVar=="Don't Summarize") {
    trigramCount <- tidy_text %>%
      count(trigram, sort=T)
  } else {
    trigramCount <- tidy_text %>%
      count(trigram, !!sym(groupByVar), sort=T)
  }
  
  return(trigramCount)
}

plotTopWords <- function(text_df, groupByVar = "Don't Summarize", tokenType = 'word', myN = 10) {
  if(tokenType=='word') {
    tokenCount <- countWords(text_df, groupByVar = groupByVar) %>%
      rename(token = word)
  }
  if(tokenType=='bigram') {
    tokenCount <- countBigrams(text_df, groupByVar = groupByVar) %>%
      rename(token = bigram)
  }
  if(tokenType=='trigram') {
    tokenCount <- countTrigrams(text_df, groupByVar = groupByVar) %>%
      rename(token = trigram)
  }
  
  title <- case_when(tokenType=='word' ~ glue::glue('Top {myN} Words'),
                     tokenType=='bigram' ~ glue::glue('Top {myN} 2-Word Phrases'),
                     tokenType=='trigram' ~ glue::glue('Top {myN} 3-Word Phrases'))
  xlabel <- case_when(tokenType=='word' ~ 'Word',
                      tokenType=='bigram' ~ 'Phrase',
                      tokenType=='trigram' ~ 'Phrase')
  
  if(groupByVar=="Don't Summarize") {
    tokenCount %>%
      arrange(desc(n)) %>%
      head(myN) %>%
      ggplot() +
      geom_col(aes(reorder(token, n), n, fill = token[2]), show.legend = F) +
      coord_flip() +
      ggtitle(title) +
      xlab(xlabel) +
      ylab('Count') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  } else if(groupByVar=='age_group') {
    tokenCount %>%
      group_by(!!sym(groupByVar)) %>%
      arrange(desc(n)) %>%
      filter(row_number()<=myN) %>%
      ungroup() %>%
      mutate(age_group = as.factor(age_group),
             token = reorder_within(token, n, age_group)) %>%
      ggplot() +
      geom_col(aes(token, n, fill = !!sym(groupByVar)), show.legend = F) +
      facet_wrap(~age_group, scales='free_y') +
      coord_flip() +
      scale_x_reordered() +
      ggtitle(title) +
      xlab(xlabel) +
      ylab('Count') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  } else if(groupByVar=='stage_of_life') {
    tokenCount %>%
      group_by(!!sym(groupByVar)) %>%
      arrange(desc(n)) %>%
      filter(row_number()<=myN) %>%
      ungroup() %>%
      mutate(stage_of_life = as.factor(stage_of_life),
             token = reorder_within(token, n, stage_of_life)) %>%
      ggplot() +
      geom_col(aes(token, n, fill = !!sym(groupByVar)), show.legend = F) +
      facet_wrap(~stage_of_life, scales='free_y') +
      coord_flip() +
      scale_x_reordered() +
      ggtitle(title) +
      xlab(xlabel) +
      ylab('Count') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  } else if(groupByVar=='has_kids') {
    tokenCount %>%
      group_by(!!sym(groupByVar)) %>%
      arrange(desc(n)) %>%
      filter(row_number()<=myN) %>%
      ungroup() %>%
      mutate(has_kids = as.factor(has_kids),
             token = reorder_within(token, n, has_kids)) %>%
      ggplot() +
      geom_col(aes(token, n, fill = !!sym(groupByVar)), show.legend = F) +
      facet_wrap(~has_kids, scales='free_y') +
      coord_flip() +
      scale_x_reordered() +
      ggtitle(title) +
      xlab(xlabel) +
      ylab('Count') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  }  
}

getTfIdf <- function(text_df, groupByVar = 'neighbor', myN = 5) {
  group_words <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(!!sym(groupByVar), word, sort = TRUE)
  
  total_words <- group_words %>%
    group_by(!!sym(groupByVar)) %>%
    summarize(total = sum(n))
  
  group_words <- left_join(group_words, total_words)
  
  group_tf_idf <- group_words %>%
    bind_tf_idf(word, !!sym(groupByVar), n) %>%
    select(-total) %>%
    group_by(!!sym(groupByVar)) %>%
    arrange(desc(tf_idf)) %>%
    head(myN) %>%
    ungroup() %>%
    arrange(!!sym(groupByVar), desc(tf_idf))
  
  return(group_tf_idf)
}
# plotTfIdf <- function(text_df, groupByVar = 'neighbor', myN = 5) {
#   tf_idf_summ <- getTfIdf(text_df, groupByVar = 'age_group', myN = 5)
#   tf_idf_summ %>%
#     ggplot(aes(tf_idf, reorder(word, tf_idf), fill = !!sym(groupByVar))) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~(!!sym(groupByVar)), ncol = 2, scales = "free") +
#     labs(x = "tf-idf", y = NULL)
# }

graphBigram <- function(text_df, myN = 2) {
  bigram_graph <- countBigrams(text_df) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(n > myN) %>%
    graph_from_data_frame()
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void() +
    theme(text = element_text(size = textSize))
}

getWordCorrelations <- function(text_df, myN = 10, myCorr=.2) {
  word_cors <- text_df %>%
    unnest_tokens(sentence, text, token='sentences') %>%
    mutate(sentence_num = row_number()) %>%
    unnest_tokens(word, sentence) %>%
    filter(!word %in% stop_words$word) %>%
    group_by(word) %>%
    filter(n() >= myN) %>%
    ungroup() %>%
    pairwise_cor(word, sentence_num, sort = TRUE) %>%
    filter(correlation>=myCorr)
  
  return(word_cors)
}

getTopCorrelatedWords <- function(text_df, myN = 10, myCorr = .2,
                                  myWords = c('violence','police','liquor','bar')) {
  wordCorrs <- getWordCorrelations(text_df, myN = myN, myCorr = myCorr) %>%
    filter(item1 %in% myWords) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    rename(word1 = item1,
           word2 = item2) %>%
    mutate(correlation = round(correlation, 2))
  
  return(wordCorrs)
}

plotTopCorrelatedWords <- function(text_df, myN = 10, myCorr = .2,
                                   myWords = c('violence','police','liquor','bar')) {
  wordCorrs <- getWordCorrelations(text_df, myN = myN, myCorr = myCorr) %>%
    filter(item1 %in% myWords) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation))
  
  if(length(myWords)>1) {
    wordCorrs %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip() +
      xlab('word') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  } else {
    wordCorrs %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab('word') +
      theme(text = element_text(size = textSize)) +
      scale_fill_brewer(palette = colorPalette)
  }
}

plotWordCorrGraph <- function(text_df, myN = 10, myCorr = .2) {
  getWordCorrelations(text_df, myN = myN, myCorr = myCorr) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
}

# # Count words, bigrams, trigrams
# countWords(text_df)
# countBigrams(text_df)
# countTrigrams(text_df)
#
# countWords(text_df, groupByVar = 'neighbor')
# countBigrams(text_df, groupByVar = 'age_group')
# countTrigrams(text_df, groupByVar = 'stage_of_life')
#
# # Plot counts of words, bigrams, trigrams
# plotTopWords(text_df, tokenType = 'word', myN = textSize)
# plotTopWords(text_df, groupByVar = 'age_group', tokenType = 'word')
# plotTopWords(text_df, groupByVar = 'stage_of_life', tokenType = 'bigram')

# # Analyze tf-idf to see unique responses from each neighbor.
# getTfIdf(text_df, groupByVar = 'neighbor', myN = 5)
# getTfIdf(text_df, groupByVar = 'age_group', myN = 5)
#
# # Bigram graph
# graphBigram(text_df)
#
# # Word correlation - words that tend to appear within the same sentence
# getWordCorrelations(text_df) %>%
#   filter(row_number()%%2==0)
#
# getWordCorrelations(text_df) %>%
#   filter(item1 == 'violence')
# getWordCorrelations(text_df) %>%
#   filter(item1 == 'police')
# getWordCorrelations(text_df) %>%
#   filter(item1 == 'liquor')
# getWordCorrelations(text_df) %>%
#   filter(item1 == 'bar')
#
# plotTopCorrelatedWords(text_df, myN = 10, myCorr = .2,
#                        myWords = c('violence','police','liquor','bar'))
# plotTopCorrelatedWords(text_df, myN = 10, myCorr = .2,
#                        myWords = c('violence'))
#
# plotWordCorrGraph(text_df, myCorr = .3)
