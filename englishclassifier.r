# English Detector
library(caret)
library(quanteda)
library(reshape)

# move to the user's home directory
setwd("~/")

# Download Data
file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
temp<-tempfile()
download.file(file_url,temp)
unzip(temp)

# move into the unzipped directory
setwd("~/final")

# The text samples downloaded contain lines from 3 different sources, news papers, blogs, and tweets. This script uses 
# only the news sources from each language because they are mostly likely to be spelled correctly and have fewer instances
# of foreign words. The script reads in all of the lines from English, German, Finnish and Russian news
# documents. 

# read in English news file

con <- file("en_US/en_US.news.txt", "r")
ENnews<-readLines(con, -1L)
unlink(con)

#read in file German news file

con <- file("de_DE/de_DE.news.txt", "r")
DEnews<-readLines(con, -1L)
unlink(con)

#read in in Finnish news file

con <- file("fi_FI/fi_FI.news.txt", "r")
FInews<-readLines(con, -1L)
unlink(con)

#read in Russian news file

con <- file("ru_RU/ru_RU.news.txt", "r")
RUnews<-readLines(con, -1L)
unlink(con)


#use quanteda to make corpii from news documents from all languages

ENnewsCORPUS<-corpus(ENnews)
DEnewsCORPUS<-corpus(DEnews)
FInewsCORPUS<-corpus(FInews)
RUnewsCORPUS<-corpus(RUnews)

# save corpus files for later loading

saveRDS(ENnewsCORPUS,"ENnewsCORPUS.rds")
saveRDS(DEnewsCORPUS,"DEnewsCORPUS.rds")
saveRDS(FInewsCORPUS,"FInewsCORPUS.rds")
saveRDS(RUnewsCORPUS,"RUnewsCORPUS.rds")

#make words DFM for all languages

ENwords <- tokens(tolower(ENnewsCORPUS),remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_separators = TRUE,
                remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

DEwords <- tokens(tolower(DEnewsCORPUS),remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_separators = TRUE,
                  remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

FIwords <- tokens(tolower(FInewsCORPUS),remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_separators = TRUE,
                  remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

RUwords <- tokens(tolower(RUnewsCORPUS),remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_separators = TRUE,
                  remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)


# save word lists 

saveRDS(ENwords,"ENwords.rds")
saveRDS(DEwords,"DEwords.rds")
saveRDS(FIwords,"FIwords.rds")
saveRDS(RUwords,"RUwords.rds")

# make document feature matrices from all news sources

ENwordsDFM<-dfm(ENwords)
DEwordsDFM<-dfm(DEwords)
FIwordsDFM<-dfm(FIwords)
RUwordsDFM<-dfm(RUwords)

# save dfms from all languages

saveRDS(ENwordsDFM,"ENwordsDFM.rds")
saveRDS(DEwordsDFM,"DEwordsDFM.rds")
saveRDS(FIwordsDFM,"FIwordsDFM.rds")
saveRDS(RUwordsDFM,"RUwordsDFM.rds")

# determine word frequencies from all languages

ENwordsFREQs<-textstat_frequency(ENwordsDFM)
DEwordsFREQs<-textstat_frequency(DEwordsDFM)
FIwordsFREQs<-textstat_frequency(FIwordsDFM)
RUwordsFREQs<-textstat_frequency(RUwordsDFM)

# Remove terms that occur fewer than 5000 times in English corpus
# If foreign words and misspellings exist in the English corpus they are likely used infrequently
# so removing infrequenttly used words should help remove words that are not truly English 
# from the corpus 

ENwordsFREQs<-ENwordsFREQs[which(ENwordsFREQs$frequency>=5000)]

# Use same number of words from foreign languages as from English (1/3 of the total from english as from each foreign)

DEwordsFREQs<-DEwordsFREQs[1:(.33*nrow(ENwordsFREQs))]
FIwordsFREQs<-FIwordsFREQs[1:(.33*nrow(ENwordsFREQs))]
RUwordsFREQs<-RUwordsFREQs[1:(.33*nrow(ENwordsFREQs))]

# Save frequency data frames for later use

saveRDS(ENwordsFREQs,"ENwordsFREQs.rds")
saveRDS(DEwordsFREQs,"DEwordsFREQs.rds")
saveRDS(FIwordsFREQs,"FIwordsFREQs.rds")
saveRDS(RUwordsFREQs,"RUwordsFREQs.rds")

# Make data frames of words from all langauges with word (ENwordsFREQs$feature) as a variable and language as a second variable
# Language is "english" for words from the english corpus and "foreign" if from one of the other languages

EN.wordscorpus<-as.data.frame(ENwordsFREQs$feature)
EN.wordscorpus$language<-"english"
EN.wordscorpus$number<-rownames(EN.wordscorpus)
rownames(EN.wordscorpus)<-paste(EN.wordscorpus$language,EN.wordscorpus$number, sep=".")
EN.wordscorpus$number<-NULL
colnames(EN.wordscorpus)<-c("word","language")

DE.wordscorpus<-as.data.frame(DEwordsFREQs$feature)
DE.wordscorpus$language<-"foreign"
DE.wordscorpus$number<-rownames(DE.wordscorpus)
rownames(DE.wordscorpus)<-paste(DE.wordscorpus$language,DE.wordscorpus$number, sep=".")
DE.wordscorpus$number<-NULL
colnames(DE.wordscorpus)<-c("word","language")

FI.wordscorpus<-as.data.frame(FIwordsFREQs$feature)
FI.wordscorpus$language<-"foreign"
FI.wordscorpus$number<-rownames(FI.wordscorpus)
rownames(FI.wordscorpus)<-paste(FI.wordscorpus$language,FI.wordscorpus$number, sep=".")
FI.wordscorpus$number<-NULL
colnames(FI.wordscorpus)<-c("word","language")

RU.wordscorpus<-as.data.frame(RUwordsFREQs$feature)
RU.wordscorpus$language<-"foreign"
RU.wordscorpus$number<-rownames(RU.wordscorpus)
rownames(RU.wordscorpus)<-paste(RU.wordscorpus$language,RU.wordscorpus$number, sep=".")
RU.wordscorpus$number<-NULL
colnames(RU.wordscorpus)<-c("word","language")

# merge words corpii from each language
ALLwords<-rbind(RU.wordscorpus,EN.wordscorpus,DE.wordscorpus,FI.wordscorpus)
ALLwords$word<-as.character(ALLwords$word)
ALLwords.corpus<-corpus(ALLwords, text_field = "word")
docvars(ALLwords.corpus,"language")<-ALLwords$language
docvars(ALLwords.corpus,"id_numeric") <- 1:ndoc(ALLwords.corpus)

saveRDS(ALLwords.corpus,"ALLwords.corpus.rds")


#split corpus into training and ALLwords sets
set.seed(300)
id_train <- sample(1:ndoc(ALLwords.corpus), .75*ndoc(ALLwords.corpus), replace = FALSE)

# get training set
training.corpus<- corpus_subset(ALLwords.corpus, id_numeric %in% id_train)

# get test set (documents not in id_train)
test.corpus<- corpus_subset(ALLwords.corpus, !id_numeric %in% id_train) 

#Make bigrams Tokens

trainingbigramS<-tokens(tolower(training.corpus),what="character", ngrams = 2L, remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,include_docvars=TRUE)


testbigramS<-tokens(tolower(test.corpus),what="character", ngrams = 2L, remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_separators = TRUE,
                        remove_twitter= TRUE, remove_hyphens = TRUE, remove_url = TRUE, include_docvars=TRUE)

#make document feature matrices for the training and test bigram sets

trainingbigram.DFM<-dfm(trainingbigramS)
testbigram.DFM<-dfm(testbigramS)

saveRDS(trainingbigram.DFM,"trainingbigram.DFM.rds")
saveRDS(testbigram.DFM,"testbigram.DFM.rds")

# split document column into document and number
trimmedTraining<-dfm_trim(trainingbigram.DFM, min_termfreq = 0.05, termfreq_type = "quantile")

# convert trimmedTraining into data frame for training the NB model
trimmedTraining.dataset<-as.data.frame(trimmedTraining)
trimmedTraining.dataset<-transform(trimmedTraining.dataset,document = colsplit(document, split = "\\.", names = c("language","number")))
trimmedTraining.dataset$document$number<-NULL
trimmedTraining.dataset$document<-trimmedTraining.dataset$document$language
trimmedTraining.dataset$document<-as.factor(trimmedTraining.dataset$document)
trimmedTraininglables<-trimmedTraining.dataset$document

# make the test set dataframe
trimmedTest<-dfm_trim(testbigram.DFM, min_termfreq = 0.05, termfreq_type = "quantile")
trimmedTest.dataset<-as.data.frame(trimmedTest)
trimmedTest.dataset<-transform(trimmedTest.dataset,document = colsplit(document, split = "\\.", names = c("language","number")))
trimmedTest.dataset$document$number<-NULL
trimmedTest.dataset$document<-trimmedTest.dataset$document$language
trimmedTest.dataset$document<-as.factor(trimmedTest.dataset$document)
trimmedTestlables<-trimmedTest.dataset$document

# train the Naive Bayes model
english.detector_nb <- textmodel_nb(trimmedTraining, trimmedTraininglables)

# assess accuracy with confusion matrix
dfmat_matched <- dfm_match(trimmedTest, features = featnames(trimmedTraining))
actual_class <- trimmedTestlables
predicted_class <- predict(english.detector_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
confusionMatrix(tab_class, mode = "everything")



