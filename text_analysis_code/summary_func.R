library(qdap)
library(tm)
library(dplyr)
library(stringr)
library(hash)
library(wordcloud)
library(stm)
library(foreach)

## function to summarize word frequency and tfidf

# preprocess the responses
response_preprocess = function(docvec, metadata){
  # first set of transition replace contractions, replace abbreviations and remove punctuations
  # also make all the words lowercase
  # doesn't hurt the original meaning of the answers at all
  mydata = as.data.frame(cbind(docvec, metadata))
  colnames(mydata) = c("docvec", "Affiliation")
  mydata$docvec = as.character(mydata$docvec)
  mydata$processed = mydata$docvec %>% replace_contraction() %>% replace_abbreviation() %>%
    removePunctuation() %>% removeNumbers() %>% str_to_lower() %>% str_squish()
  
  # remove stopwords
  exception <- c("no","not")
  stops = setdiff(stopwords("en"),exception)
  mydata$processed = mydata$processed %>% removeWords(stops)
  
  #make term document matrix
  textsource = VectorSource(mydata$processed)
  textcorpus = VCorpus(textsource)
  text_dtm <- as.matrix(DocumentTermMatrix(textcorpus))
  text_tdm <- as.matrix(TermDocumentMatrix(textcorpus))
  
  #get the list of all the unique words
  words = names(text_tdm[,1])
  
  # get the frequency of all the unique words
  freq <- colSums(text_dtm) %>% unname()
  
  #make a new dataframe with original words, frequency, stemmed word and stemmed word frequency
  stemword = stemDocument(words)
  word_freq = cbind(words,freq,stemword) %>% as.data.frame()
  
  # count the number of words that share the same stemmed word
  stemfreq = table(stemword)
  word_freq$stem_freq = NA
  word_freq$stem_freq = stemfreq[word_freq$stemword]
  
  #for those vacabularies that only appear once and don't share its stemmed word
  # with any other word, they may as well be removed
  a = word_freq %>% filter(freq==1 & stem_freq==1) %>% select(words)
  newstops = as.character(a$words)
  
  # list of the following remove step is a bit meandering
  # because the list of stopwords is too long.
  # this for loop takes a while to run
  group <- 100
  n <- length(newstops)
  r <- rep(1:ceiling(n/group),each=group)[1:n]
  d <- split(newstops,r)
  for (j in 1:length(d)){
    mydata$processed = removeWords(mydata$processed, paste(d[[j]]))
  }
  
  # remove trimming and trailing white space
  mydata$processed = rm_white(mydata$processed,trim=TRUE, clean=TRUE)
  mydata$processed = str_squish(mydata$processed)
  # remove empty strings
  
  mydata = mydata %>% filter(processed!="")
  
  #hash table from original words to stemmed words
  stem_hash = hash(word_freq$words,word_freq$stemword)
  
  
  # function to map words to stemmed version
  map_stem = function(x){
    allwords = unlist(strsplit(x,'[ ]'))
    for (j in 1:length(allwords)){
      if (has.key(allwords[j],stem_hash)){
        allwords[j] = toString(stem_hash[[allwords[j]]])
      }
    }
    paste(allwords,collapse=' ')
  }
  
  mydata$processed = sapply(mydata$processed, map_stem) %>% unname()
  
  return(mydata)
}

# rank words based on appearances and tfidf
singleword = function(mydata, meta=FALSE){
  # based on the stemmed version, regenerate a new set of term document matrix
  freq_calc = function(docvec){
    newtextsource = VectorSource(docvec)
    newtextcorpus = VCorpus(newtextsource)
    new_dtm <- as.matrix(DocumentTermMatrix(newtextcorpus))
    new_tdm <- as.matrix(TermDocumentMatrix(newtextcorpus))
    
    # calculate tfidf scores
    
    raw_frequency <- rowSums(new_tdm)
    occurrence <- rowSums(new_tdm > 0)
    tfidf <- raw_frequency * log(ncol(new_tdm) / occurrence)
    
    # combine the final result
    result = cbind(raw_frequency, tfidf) %>% as.data.frame()
    result$word = rownames(result)
    
    finalcount = result %>% arrange(desc(tfidf))
    finalcount = finalcount %>% select(word, everything())
    
    colnames(finalcount) = c("Word", "Raw Frequency", "TF-IDF Score")
  
    list(finalcount, docvec)
  }
  if (!meta){
    output = freq_calc(mydata$processed)
  } else{
    alllevels = mydata$Affiliation %>% as.character() %>% unique() %>% na.omit()
    output = foreach(level = alllevels, .inorder=FALSE) %do% {
      subset_data = mydata %>% filter(Affiliation==level)
      countresult = subset_data$processed %>% freq_calc()
      list(class=level, ranking=countresult[[1]], doc=countresult[[2]])
    }
  }
  output
}

# stm model
topic_model = function(mydata, meta=FALSE, numtopic=10){
  mydata = na.omit(mydata)
  exception <- c("no","not")
  # customize stop words
  stops = setdiff(stopwords("en"),exception)
  processed <- textProcessor(mydata$processed, metadata = mydata[,c("docvec", "Affiliation")],
                             customstopwords = stops)
  out <- prepDocuments(processed$documents, processed$vocab,
                       processed$meta, lower.thresh = 1)
  if (meta){
    modelfit <- stm(documents = out$documents, vocab = out$vocab,
                             K = numtopic, prevalence =~Affiliation,data = out$meta)
  } else{
    modelfit <- stm(documents = out$documents, vocab = out$vocab,
                    K = numtopic, data = out$meta)
  }
  list(modelfit, out$meta)
}


stmsum <- function(smodel, rawreview){
  topicwords <- labelTopics(smodel)$prob
  representative <- findThoughts(smodel, text = rawreview$docvec, n=7)$docs %>% 
    as.data.frame(stringsAsFactors=FALSE)
  topicproportion_all <- apply(smodel$theta, 2, function(allprop){
    c(mean(allprop), sd(allprop))
  })
  topicproportion_undergrad <- apply(smodel$theta, 2, function(allprop){
    undergrad_prop = allprop[rawreview$Affiliation == "Undergraduate"]
    c(mean(undergrad_prop), sd(undergrad_prop))
  })
  topicproportion_grad <- apply(smodel$theta, 2, function(allprop){
    grad_prop = allprop[rawreview$Affiliation == "Master's Student"]
    c(mean(grad_prop), sd(grad_prop))
  })
  result = list(topicwords, representative, topicproportion_all, 
                topicproportion_undergrad, topicproportion_grad)
  names(result) = c("topicwords", "representative", "all", "undergrad", "graduate")
  result
}


# run stm silently
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}


full_analysis <- function(textresponse, affiliation, topicnum = 6){
  cleaned <- response_preprocess(textresponse, 
                                         affiliation)
  single_word <- singleword(cleaned)
  stm_result <- quiet(topic_model(cleaned, numtopic = topicnum))
  stm_model <- stm_result[[1]]
  raw_text <- stm_result[[2]]
  stm_summary <- stmsum(stm_model, raw_text)
  list(single_word,  stm_summary)
}



