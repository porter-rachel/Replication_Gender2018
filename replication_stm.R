######################################### Final Cleaning  ######################################### 
library(readr)
library(stringr)
library(tm)
library(quanteda)
library(stm)
library(tidytext)
library(dplyr)
library(haven)

rm(list=ls())
cleaned <- read_csv("cleaned_fullframe.csv")
cleaned <- unique(cleaned[ , 2:56])

## Dropping observations with "NA" as text
bios <- data.frame(subset(cleaned, cleaned$About != "NA"))

## Changing abbreveations to words 
bios$About <- gsub( "\\bBA\\b", " Bachelors Degree ", bios$About)
bios$About <- gsub( "\\bBS\\b", " Bachelors in Science ", bios$About)
bios$About <- gsub( "\\bB.S.\\b", " Bachelors in Science ", bios$About)
bios$About <- gsub( "\\bB.A.\\b", " Bachelors Degree ", bios$About)
bios$About <- gsub( "\\bMA\\b", " Masters Degree ", bios$About)
bios$About <- gsub( "\\bM.A.\\b", " Masters Degree ", bios$About)
bios$About <- gsub( "\\bJD\\b", " Law Degree ", bios$About)
bios$About <- gsub( "\\bJ.D.\\b", " Law Degree ", bios$About)
bios$About <- gsub( "\\bPhD\\b", " Doctorate ", bios$About)
bios$About <- gsub( "\\bPh.D.\\b", " Doctorate ", bios$About)
bios$About <- gsub( "\\bMBA\\b", " Masters in Business ", bios$About)
bios$About <- gsub( "\\bMD\\b", " Medical Doctorate ", bios$About)
bios$About <- gsub( "\\bMD\\b", " Medical Doctorate ", bios$About)
bios$About <- gsub( "\\bM.D.\\b", " Medical Doctorate ", bios$About)
bios$About <- gsub( "\\bU.S.\\b", " United States of America", bios$About)
bios$About <- gsub( "\\bUS\\b", " United States of America", bios$About)
bios$About <- gsub( "\\bU.S.A.\\b", " United States of America ", bios$About)
bios$About <- gsub( "\\bUSA\\b", " United States of America", bios$About)
bios$About <- gsub( "\\b2nd\\b", " Second ", bios$About)
bios$About <- gsub("[^[:alpha:] ]", " ", bios$About)

## Import city names and prep text for cleaning
city_master_list <- read_csv("~/Dropbox/Primary_Elections/PRQ_paper/city_master_list.csv")
bios$About <- sapply(bios$About, tolower)

## Drop states from text
state <- as.vector(unique(sapply(city_master_list$`State full`, tolower)))
state <- paste0('\\b', state, '\\b', collapse='|')
bios$About <- gsub(state, "", bios$About)

## Removing candidate names 
for(i in 1:nrow(bios)){
  name <- tolower(unlist(strsplit(bios$Name[i], " "))) 
  name <- paste0('\\b', name, '\\b', collapse='|')
  bios$About <- gsub(name, "", bios$About)
  print(i)
}

## Removing some other words
bios$About <- gsub( "\\bcom\\b", " ", bios$About)
bios$About <- gsub( "\\bsite\\b", " ", bios$About)
bios$About <- gsub( "\\bfacebook\\b", " ", bios$About)
bios$About <- gsub( "\\btwitter\\b", " ", bios$About)
bios$About <- gsub( "\\bcontact\\b", " ", bios$About)
bios$About <- gsub( "\\bsan\\b", " ", bios$About)
bios$About <- gsub( "\\bdiego\\b", " ", bios$About)
bios$About <- gsub( "\\philadelphia\\b", " ", bios$About)
bios$About <- gsub( "\\blos\\b", " ", bios$About)
bios$About <- gsub( "\\bchicago\\b", " ", bios$About)
bios$About <- gsub( "\\bangeles\\b", " ", bios$About)
bios$About <- gsub( "\\bhouston\\b", " ", bios$About)
bios$About <- gsub( "\\bmiami\\b", " ", bios$About)
bios$About <- gsub( "\\borange\\b", " ", bios$About)
bios$About <- gsub( "\\btexan\\b", " ", bios$About)

## Create factor variables 
bios$factor_all <- as.factor(bios$factor_all)

## Remove one letter words
bios$About <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", bios$About)

## Creating corpus
bios1 <- corpus(bios, text_field = "About")

## Checking to see if there are any more missing bios
apply(bios1$documents, 2, function(x) sum(is.na(x)))

## Stemming and removing stop words
bios_dfm <- dfm(bios1, stem = T,
                remove = stopwords('english'), remove_punct = T)

## Examining top features and threshold level
topfeatures(bios_dfm, 100)
bios_stm <- convert(bios_dfm, to = 'stm')
plotRemoved(bios_stm$documents, lower.thresh=seq(1,200, by=100))

## Determining drop threshold 
bios_stm <- prepDocuments(bios_stm$documents, bios_stm$vocab,
                          bios_stm$meta, lower.thresh = 30)

######################################### MODEL FIT  ######################################### 

## Fitting initial model
## Using a gamma.prior of 'L1' due to a factor variable with hundreds of levels (raceid)
fit_stm <- stm(documents = bios_stm$documents, vocab = bios_stm$vocab,
               K = 0, prevalence = ~ factor_all,
               seed = 374075, data = bios_stm$meta, init.type = "Spectral", max.em.its = 150)

labelTopics(fit_stm)
plot(fit_stm, type="summary", main = "")
## Initial model finds 40 topics 
topicQuality(fit_stm,documents=bios_stm$documents)

storage <- searchK(bios_stm$documents, bios_stm$vocab, K = c(10, 20, 30, 40, 50, 60),
                   prevalence = ~ factor_all,
                   data = bios_stm$meta, seed = 374075)
plot(storage, main = "TEST")
## Somewhere between 15 and 25 topics
storage2 <- searchK(bios_stm$documents, bios_stm$vocab, K = c(20:40),
                   prevalence = ~ factor_all,
                   data = bios_stm$meta, seed = 374075)
plot(storage2)
storage <- searchK(bios_stm$documents, bios_stm$vocab, K = c(20:30),
                   prevalence = ~ factor_all,
                   data = bios_stm$meta, seed = 374075)
plot(storage)

fit_stm <- stm(documents = bios_stm$documents, vocab = bios_stm$vocab,
               K = 27, prevalence = ~ factor_all,
               seed = 374075, data = bios_stm$meta, init.type = "Spectral", max.em.its = 150)

labelTopics(fit_stm, n = 20)
plot(fit_stm, type="summary")
topicQuality(fit_stm,documents=bios_stm$documents, 
             labels= c('Coaching', 'Nature/Land', 'Campaign Phrases',  'Healthcare',
                       'Think-Do-Say', 'Agriculture', 'Awards & Prestige', 'Electoral Experience',
                       'Activism', 'Bipartisanship', 'Immigration', 'Family/Education',
                       'Support/Donate', 'Blue Collar', 'Terrorism', 'President/Party',
                       'Tax/Spending', 'Diversity', 'Religion', 'Legislative',
                       'Partisan Issues', 'Business/Industry', 'Representation', 'Medicine/Doctor', 
                       'Legal/Lawyer', 'American Dream', 'Military'))
frequency <- colMeans(fit_stm$theta)
frequency

## Check model usefulness
bios_stm$meta$factor_all <- as.factor(bios_stm$meta$factor_all)
prep <- estimateEffect(1:27 ~ factor_all, fit_stm, meta = bios_stm$meta, uncertainty = "Global")

summary(prep, topics=8)

par(mfrow = c(1,1), cex = 0.6)

### MODELS IN PAPER ###

plot(prep, covariate="factor_all", c(20, 10, 8, 18, 17, 24, 16, 4, 15, 2, 11, 21, 
                                     6, 1, 13, 7, 3, 25, 26, 19, 27, 22, 14, 9, 12, 5, 23), 
     model=fit_stm, method="difference", 
     cov.value1="6", cov.value2="7", labeltype ="custom", xlim=c(-.18,.18),
     custom.labels= c('Legislative', 'Bipartisanship', 'Electoral Experience', 'Diversity', 'Tax/Spending', 
                      'Legal/Lawyer', 'President/Party', 'Healthcare', 'Terrorism', 
                      'Nature/Land', 'Immigration', 'Partisan Issues', 'Agriculture', 'Coaching', 'Support/Donate', 
                      'Awards & Prestige', 'Campaign Phrases', 'Medicine/Doctor', 'American Dream', 
                      'Religion', 'Military', 'Business/Industry', 'Blue Collar',
                      'Activism', 'Family/Education', 'Think-Do-Say', 'Representation'),
     xlab = "Amateur Democratic Women...Experienced Democratic Women", ci.level = .90)


plot(prep, covariate="factor_all", topics=c(18, 23, 20, 25, 16, 22, 21, 
                                             11, 4, 8, 19, 17, 15, 26, 2, 3,
                                             6, 12, 24, 14, 7, 27, 13, 1, 5, 10, 9), 
     model=fit_stm, method="difference", 
     cov.value1="6", cov.value2="2", labeltype ="custom", xlim=c(-.18,.18),
     custom.labels= c('Diversity', 'Representation', 'Legislative', 'Medicine/Doctor', 'President/Party',
                      'Business/Industry', 'Partisan Issues', 'Immigration', 'Healthcare',
                      'Electoral Experience', 'Religion', 'Tax/Spending', 'Terrorism', 'American Dream',
                      'Nature/Land', 'Campaign Phrases', 'Agriculture', 'Family/Education',
                      'Legal/Lawyer', 'Blue Collar', 'Awards & Prestige', 'Military',
                      'Support/Donate', 'Coaching', 'Think-Do-Say', 'Bipartisanship', 'Activism'),
       xlab = "Experienced Democratic Men...Experienced Democratic Women", ci.level = .90)

plot(prep, covariate="factor_all", topics=c(18,20,4,10, 17), model=fit_stm, method="difference", 
     cov.value1="6", cov.value2="4", labeltype ="custom", xlim=c(-.25,.25),
     custom.labels= c('Diversity', 'Legislative', 'Healthcare', 'Bipartisanship','Taxes'),
     xlab = "Republican Female Experienced...Democrat Female Experienced", ci.level = .90)

plot(prep, covariate="factor_all", topics=c(4,18,16,17), model=fit_stm, method="difference", 
     cov.value1="7", cov.value2="5", labeltype ="custom", xlim=c(-.25,.25),
     custom.labels= c('Healthcare', 'Diversity', 'President/Party', 'Taxes'),
     xlab = "Republican Female Amateur...Democrat Female Amateur", ci.level = .90)

