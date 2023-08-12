
# Read in the packages
library(readr)
library(dplyr)
library(tm)
library(wordcloud)

# Read in the file

jeopardy <- read_csv("datasets/jeopardy.csv")

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("The correct packages were attached", {
        expect_true("readr" %in% .packages(), 
                    info = "Did you load the readr package with library()? Please check the hint.")
        })
    test_that("The correct packages were attached", {
        expect_true("dplyr" %in% .packages(), 
                    info = "Did you load the dplyr package with library()? Please check the hint.") 
        })
    test_that("The correct packages were attached", {
        expect_true("tm" %in% .packages(), 
                    info = "Did you load the tm package with library()? Please check the hint.")
        })
    test_that("The correct packages were attached", {
        expect_true("wordcloud" %in% .packages(), 
                    info = "Did you load the wordcloud package with library()? Please check the hint.") 
        })
    test_that("The dataset was loaded correctly", {
        expect_is(jeopardy, "tbl_df", 
                  info = "Did you read in the dataset with read_csv()? Please check the hint.")
        })
    })

# Take a glimpse at the data

glimpse(jeopardy)

head(jeopardy)

head_output <- .Last.value

#e1  <- rlang::current_env()
#rlang::env_print(e1)
#dir()

run_tests({
    test_that("glimpse", {
        expect_identical(head_output, head(jeopardy),
                     info = "Use head() to display for first six row of jeopardy after calling glimpse(). \n Please check the hint.")
    })
})

# Select categories

categories <- jeopardy %>% 
filter(round == "Jeopardy!") %>% 
select(category)

# Create a vector source

categories_source <- VectorSource(categories)

# Create a volatile corpus

categories_corp <- VCorpus(categories_source)

e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("categories was created", {
        expect_true("categories" %in% rlang::env_names(e1), 
            info = "categories was not created. Is there a typo? Please check the hint.")
    })
    test_that("categories_source was created", {
        expect_true("categories_source" %in% rlang::env_names(e1), 
            info = "categories_source was not created. Is there a typo? Please check the hint.")
    })
    test_that("categories_corp was created", {
        expect_true("categories_corp" %in% rlang::env_names(e1), 
            info = "categories_corp was not created. Is there a typo? Please check the hint.")
    })
})

# Clean the corpus for a custom function

clean_corpus <- function(corpus){
    
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,
                     content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, 
                     c(stopwords("en")))
    
    return(corpus)
    
}

# Clean

clean_corp <- clean_corpus(categories_corp)

# Term document matrix

categories_tdm <- TermDocumentMatrix(clean_corp)

e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("clean_corp was created", {
        expect_true("clean_corp" %in% rlang::env_names(e1), 
            info = "clean_corp was not created. Is there a typo? Please check the hint.")
    })
    test_that("categories_tdm was created", {
        expect_true("categories_tdm" %in% rlang::env_names(e1), 
            info = "categories_tdm was not created. Is there a typo? Please check the hint.")
        })
})

# TDM into a matrix format

categories_m <- as.matrix(categories_tdm)

# Calculate counts of words across documents

term_frequency <- sort(rowSums(categories_m), decreasing = TRUE)

head(term_frequency)

# Bar plot of the results

barplot(head(term_frequency, n = 12), las = 2, col = "orange")

p  <- .Last.value
e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("categories_m was created", {
        expect_true("categories_m" %in% rlang::env_names(e1), 
            info = "categories_m was not created. Is there a typo? Please check the hint.")
    })
    test_that("term_frequency was created", {
        expect_true("term_frequency" %in% rlang::env_names(e1), 
            info = "term_frequency was not created. Is there a typo? Please check the hint.")
        })
    test_that("a barplot was created", {
        expect_equal(p, barplot(term_frequency[1:12]),
                     info = "Did you create a barplot? Please check the hint.")
        })
})

rm(categories_m, categories_tdm, term_frequency)

# Modify the custom function to add some stop words

clean_corpus <- function(corpus){
    
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,
                     content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, 
                     c(stopwords("en"), 
                       "time", "new", "first"))
    
    return(corpus)
    
}

# Clean better

cleaner_corp <- clean_corpus(categories_corp)

# Cleaner TDM

cleaner_tdm <- TermDocumentMatrix(cleaner_corp)

# Cleaner as.matrix

categories_m <- as.matrix(cleaner_tdm)

# Frequencies again!

term_frequency <- sort(rowSums(categories_m), decreasing = TRUE)

# Barplot the newly cleaned stuff

barplot(head(term_frequency, n = 12), las = 2, col = "orange")

p  <- .Last.value
e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("cleaner_corp was created", {
        expect_true("cleaner_corp" %in% rlang::env_names(e1), 
            info = "cleaner_corp was not created. Is there a typo? Please check the hint.")
    })
    test_that("cleaner_tdm was created", {
        expect_true("cleaner_tdm" %in% rlang::env_names(e1), 
            info = "cleaner_tdm was not created. Is there a typo? Please check the hint.")
        })
    test_that("categories_m was created", {
        expect_true("categories_m" %in% rlang::env_names(e1), 
            info = "categories_m was not created. Is there a typo? Please check the hint.")
        })
    test_that("term_frequency was created", {
        expect_true("term_frequency" %in% rlang::env_names(e1), 
            info = "term_frequency was not created. Is there a typo? Please check the hint.")
        })
    #test_that("a barplot was created", {
    #    expect_output(print(p), "[,1]\n [1,]  0.7\n [2,]  1.9\n [3,]  3.1\n [4,]  4.3\n [5,]  5.5\n [6,]  6.7\n [7,]  7.9\n [8,]  9.1\n [9,] 10.3\n[10,] 11.5\n[11,] 12.7\n[12,] 13.9", fixed = TRUE,
    #                 info = "Did you create a barplot? Please check the hint.")
    #    })
})

speed_clean <- function(corpus){
    
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,
                     content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, 
                     c(stopwords("en"), 
                       "time", "new", "first"))
    
    return(corpus)
    
}

e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("speed_clean function was created", {
        expect_true("speed_clean" %in% rlang::env_names(e1), 
            info = "The speed cleaning function was not created. Is there a typo? Please check the hint.")
    })
})

freq_terms <- function(matrix) {
    
    matrix <- sort(rowSums(matrix), decreasing = TRUE)
    
    return(matrix)
    
}

e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("freq_terms function was created", {
        expect_true("freq_terms" %in% rlang::env_names(e1), 
            info = "The term frequency function was not created. Is there a typo? Please check the hint.")
    })
})

# Create answers

answers <- jeopardy %>% 
filter(round == "Final Jeopardy!") %>% 
select(answer)

# Create the vector soure and corpus, TDM, and make into matrix

answer_source <- VectorSource(answers)
answer_corpus <- VCorpus(answer_source)
answer_clean <- speed_clean(answer_corpus)
answer_tdm <- TermDocumentMatrix(answer_clean)
answer_m <- as.matrix(answer_tdm)

# Answer frequency

ans_frequency <- freq_terms(answer_m)

# Get names

ans_names <- names(ans_frequency)

wordcloud(ans_names, ans_frequency, max.words = 40)

e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("answers was created", {
        expect_true("answers" %in% rlang::env_names(e1), 
            info = "answers was not created. Is there a typo? Please check the hint.")
    })
    test_that("ans_frequency was created", {
        expect_true("ans_frequency" %in% rlang::env_names(e1), 
            info = "ans_frequency was not created. Is there a typo? Please check the hint.")
        })
    test_that("ans_names was created", {
        expect_true("ans_names" %in% rlang::env_names(e1), 
            info = "ans_names was not created. Is there a typo? Please check the hint.")
        })
    test_that("term_frequency was created", {
        expect_true("term_frequency" %in% rlang::env_names(e1), 
            info = "term_frequency was not created. Is there a typo? Please check the hint.")
        })
})

"c"

p  <- .Last.value
e1  <- rlang::current_env()
#rlang::env_print(e1)
#rlang::env_names(e1)

run_tests({
    test_that("the string is correct", {
        expect_identical(p, "c", 
            info = "Oops. What book would help you study American history? Please check the hint."  )
    })
    # You can have more than one test
})
