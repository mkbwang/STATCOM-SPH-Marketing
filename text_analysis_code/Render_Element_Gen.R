
# function takes in the STM summary object, hand annotated topic names and hand picked examples
# return the two tables and two plots to be rendered
elements = function(analysis_result, topicnames, topicexamples){
  single_word = analysis_result[[1]]
  stm_summary = analysis_result[[2]]
  
  whole_score = stm_summary$all %>% t() %>% as.data.frame()
  colnames(whole_score) = c("Proportion", "SD")
  whole_score$Name = topicnames
  
  undergrad_score = stm_summary$undergrad %>% t() %>% as.data.frame()
  colnames(undergrad_score) = c("Proportion", "SD")
  undergrad_score$Name = topicnames
  undergrad_score$Affiliation = "Undergrad"
  
  grad_score = stm_summary$grad %>% t() %>% as.data.frame()
  colnames(grad_score) = c("Proportion", "SD")
  grad_score$Name = topicnames
  grad_score$Affiliation = "Graduate"
  
  stratify_score = rbind(grad_score, undergrad_score)
  
  # top 10 words
  top_10_words = head(single_word[[1]], 10)
  
  # example answers
  examples = cbind(Topic = topicnames, Example = topicexamples)
  
  # first plot
  whole_scoreview <- ggplot(whole_score,aes(x=reorder(Name,Proportion),y=Proportion*100))+
    geom_point(color='navy blue',size=3)+geom_segment(aes(xend=Name,yend=0),color='navy blue',size=2)+
    coord_flip()+xlab("Topic") + ylab("Topic Prevalence (%)")+theme_bw(base_size=12)
  
  # second plot
  compare_scoreview <- ggplot(stratify_score,aes(x=Affiliation,y=Proportion*100))+
    geom_point(color='navy blue',size=2)+geom_errorbar(aes(ymin=(Proportion-SD)*100, ymax=(Proportion+SD)*100), width=.2,position=position_dodge(.9)) + 
    facet_wrap(~ Name, nrow = 2) +
    coord_flip()+xlab("Topic") + ylab("Topic Prevalence (%)")+theme_bw(base_size=12)
  
  text_elements <- list(top_10_words, examples, whole_scoreview, compare_scoreview)
  names(text_elements) <- c("top_10_words", "example", "whole_plot", "stratified_plot")
  text_elements
}

# a simple version only records word frequency and typical responses
elements_simple = function(analysis_result, rawwords){
  single_word = analysis_result[[1]]
  top_10_words = head(single_word[[1]], 10)
  word_count = sapply(strsplit(rawwords, " "), length)
  selected_index = order(word_count, decreasing=TRUE)
  selected_doc = rawwords[selected_index]
  examples = data.frame(Example = selected_doc[1:5])
  list(top_10_words, examples)
}

