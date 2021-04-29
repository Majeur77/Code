# import the library 
library(tidyverse)
library(shiny)
library(DT)
library(writexl)
library(dplyr)
library(text2vec)
library(readr)
library(stringr)
library(stopwords)
library(textstem)
library(tm)
library(NLP)
library(quanteda)
library(corpus)
library(tibble)
library(gsubfn)

scoreResult = function(input1,input2){
  
  case <- read.csv(input1,stringsAsFactors = F) 
  resume_f <- read_file(input2)
  
  # make resume content a dataframe
  resume_fdf <- tibble(title = "User", description=resume_f)
  
  # combine resume and job description
  case_resume <- rbind(resume_fdf,case)
  
  # data cleaning function
  prep_fun = function(x) {
    # make text lower case
    x = str_to_lower(x)
    # remove non-alphanumeric symbols
    x = str_replace_all(x, "[^[:alnum:]]", " ")
    # remove numbers
    x = gsub(patter="\\d", replace=" ", x)
    # remove stopwords
    x = removeWords(x, stopwords())
    # remove single character
    x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
    # collapse multiple spaces
    x= str_replace_all(x, "\\s+", " ")
    # lemmatization 
    x = lemmatize_strings(x)}
  
  # clean the job description data and create a new column
  case_resume$description_clean = lemmatize_words(prep_fun(case_resume$description))
  
  # use vocabulary_based vectorization
  it_resume = itoken(case_resume$description_clean, progressbar = FALSE)
  v_resume = create_vocabulary(it_resume)
  
  # eliminate very frequent and very infrequent terms
  # v_resume = prune_vocabulary(v_resume, doc_proportion_max = 0.1, term_count_min = 5)
  v_resume = prune_vocabulary(v_resume)
  vectorizer_resume = vocab_vectorizer(v_resume)
  
  # apply TF-IDF transformation
  dtm_resume = create_dtm(it_resume, vectorizer_resume)
  tfidf = TfIdf$new()
  dtm_tfidf_resume = fit_transform(dtm_resume, tfidf)
  
  # compute similarity-score against each row
  resume_tfidf_cos_sim = sim2(x = dtm_tfidf_resume, method = "cosine", norm = "l2")
  resume_tfidf_cos_sim[1:15,1:15]
  
  # create a new column for similarity_score of dataframe
  case_resume["similarity_score"] = resume_tfidf_cos_sim[1:nrow(resume_tfidf_cos_sim)]
  # sort the dataframe by similarity score from the lowest to the highest
  case_resume_result<- case_resume[order(case_resume$similarity_score),c(1,4)]
  
  return(case_resume_result)
  
}

#########Shiny Server##########

shinyServer(function(input ,output, session) {
  restab <- eventReactive(input$go,{
    req(input$upload_resume)
    req(input$upload_syllabus)
    scoreResult(input$upload_syllabus$datapath,input$upload_resume$datapath)
  })
  output$case_resume_result <- renderTable({
    restab()
  })
  output$downloadData <- downloadHandler(
    filename = "Scoring_Result.xlsx",
    content = function(file1){
      write_xlsx(restab(),file1)
    })
})