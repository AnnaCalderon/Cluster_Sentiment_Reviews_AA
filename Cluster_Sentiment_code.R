#-------------------------------------------------------------------------------
# APAN 5205 PROJECT PROPOSAL CODE
# Group 2 
# Date: 04/15/21
#-------------------------------------------------------------------------------


# LOAD LIBRARIES ---------------------------------------------------------------
library(lattice) 
library(ggplot2) 
library(caret)
library(corrplot)
library(ggcorrplot)
library(psych)
library(GPArotation)
library(dplyr)
library(stringr)
library(tidyr)
library(mice)
library(tidyverse)
library(ggthemes)
library(magrittr)
library(tidytext)
library(lexicon)
library(cluster)
library(mclust)
library(RColorBrewer)
library(janeaustenr)
library(tm)


# IMPORT RAW DATASET & DATA EXPLORATION ----------------------------------------

# Set working directory
# Import raw dataset
americanData = read.csv(file = 'Raw Dataset_Airline Reviews_Group 2.csv', stringsAsFactors = F)

# Explore raw dataset
glimpse(americanData)
str(americanData)


# STEP 1: CLEAN DATA & CREATION OF NEW VARIABLES -------------------------------

# Add "tripVerification" variable 
# Indicates whether a review is verified, not verified, or not listed (3 levels)
americanData$tripVerification = 
  ifelse(grepl('Not Verified', americanData$customer_review, fixed = TRUE),'Not Verified',
         ifelse(grepl('Unverified', americanData$customer_review, fixed = TRUE),'Not Verified',
                ifelse(grepl('Verified', americanData$customer_review, fixed = TRUE),'Verified','Not Listed')))

str(americanData)
americanData$customer_review[1] #view review 1

# Add "review" variable
# Cleaned version of customer_review column
americanData$review = 
  ifelse(grepl(' | ', americanData$customer_review, fixed = TRUE),
         substring(gsub(".*erified","",americanData$customer_review),4),
         americanData$customer_review)

str(americanData)
americanData$review[1]

# Add "flightInterruption" variable 
# Flags any mention of flight delays or cancellations
americanData$flightInterruption = 
  ifelse(grepl('delay', tolower(americanData$customer_review), fixed = TRUE), 'Delayed',
         ifelse(grepl('late', tolower(americanData$customer_review), fixed = TRUE), 'Delayed',
                ifelse(grepl('cancel', tolower(americanData$customer_review), fixed = TRUE), 'Cancelled',
                       ifelse(grepl('miss', americanData$customer_review, fixed = TRUE), 'Missed', 
                              ifelse(grepl('change', americanData$customer_review, fixed = TRUE), 'Changed', 
                                     "No Delay/Cancellation")))))

str(americanData)
unique(americanData$flightInterruption)

# Add "reviewLength" variable 
# Count of characters in each passenger review 
americanData$reviewLength = nchar(americanData$customer_review)
str(americanData)

# Clean date_flown column: (1) Fix "-" in any dates  (2) Remove the day 
americanData$dateFlight = str_replace(str_replace(americanData$date_flown, "May-", "May "), "20", "") 

americanData$dateReview = str_replace(sub(".*? ", "", americanData$review_date), "20", "") 

# Add "reviewSameMonth" variable 
# Indicates whether review was completed in the same month as the date of the trip
americanData$reviewSameMonth = ifelse(americanData$dateFlight == americanData$dateReview, 'Yes', 'No')
americanData$dateFlight

str(americanData)

# Add "flightType" variable 
# Binary flag for Direct Flight vs. With Layover
americanData$flightType = ifelse(grepl(' via ', tolower(americanData$route), fixed = TRUE), 'With Layover','Direct')

str(americanData)

# Add AircraftType variable 
# Indicates size of aircraft (small, medium, big, other)
americanData$AircraftType = 
  ifelse(grepl('CR|700|170|175|190|145|Em|MD|ER', americanData$aircraft, fixed = FALSE),'Small',
         ifelse(grepl('320|757|737|738|319|321', americanData$aircraft, fixed = FALSE),'Medium',
                ifelse(grepl('787|789|777|772|767|747|330|300|340|343', americanData$aircraft, fixed = FALSE),'Big','Other')))

glimpse(americanData)
table(americanData$AircraftType)


# STEP 2: CHANGE VARIABLE TYPES ------------------------------------------------
AAdata <- americanData %>%
  select(overall,traveller_type,cabin,seat_comfort,cabin_service,food_bev,entertainment,ground_service,value_for_money,recommended,
         tripVerification,review,flightInterruption,reviewLength,reviewSameMonth,flightType,AircraftType)

str(AAdata)

# Change variables to factors
AAdata$traveller_type <- factor(AAdata$traveller_type)
AAdata$cabin <- factor(AAdata$cabin)
AAdata$recommended <- factor(AAdata$recommended)
AAdata$tripVerification <- factor(AAdata$tripVerification)
AAdata$flightInterruption <- factor(AAdata$flightInterruption)
AAdata$reviewSameMonth <- factor(AAdata$reviewSameMonth)
AAdata$flightType <- factor(AAdata$flightType)
AAdata$AircraftType <- factor(AAdata$AircraftType)

# Add recommender column: binary flag 
AAdata$recommender <- NA

AAdata$recommender[AAdata$recommended == "no"] <- "no"
AAdata$recommender[AAdata$recommended == "yes"] <- "yes"

AAdata$recommender <- factor(AAdata$recommender)

levels(AAdata$recommender)

sum(is.na(AAdata$recommender)) #91 NAs

str(AAdata)

AAdata <- AAdata %>%
  select(overall,traveller_type,cabin,seat_comfort,cabin_service,food_bev,entertainment,ground_service,value_for_money,
         tripVerification,review,flightInterruption,reviewLength,reviewSameMonth,flightType,AircraftType,recommender)


# STEP 3: IMPUTATION -----------------------------------------------------------
# Checking NAs
table(is.na(americanData$overall))
table(is.na(americanData$seat_comfort))
table(is.na(americanData$cabin_service))
table(is.na(americanData$food_bev))
table(is.na(americanData$entertainment))
table(is.na(americanData$ground_service))
table(is.na(americanData$value_for_money))
table(is.na(americanData$reviewLength))

AAdata_factors <- AAdata %>%
  select(traveller_type,cabin,tripVerification,flightInterruption,reviewSameMonth,flightType,AircraftType,recommender)

AAdata_factors <- complete(mice(AAdata_factors, seed = 617))

sum(is.na(AAdata_factors))

AAdata_rest <- AAdata %>%
  select(overall,seat_comfort,cabin_service,food_bev,entertainment,ground_service,value_for_money,reviewLength)

# impute numerical variables through prediction 
set.seed(617)
AAdata_rest <- predict(preProcess(AAdata_rest, method = 'bagImpute'), newdata = AAdata_rest)

# verify that there are no NAs 
sum(is.na(AAdata_rest))

# Join all variable and review into one dataset after imputation 
AAdata_review <- AAdata %>%
  select(review)

AAdata2 <- bind_cols(AAdata_rest,AAdata_factors,AAdata_review)

# Verify the join and proper imputation 
str(AAdata2)
sum(is.na(AAdata2))


# STEP 4: FACTOR ANALYSIS ------------------------------------------------------

# Correlation matrix of between all passenger rating categories
cor(AAdata2[,2:7])
AAdatafa <- AAdata2[,2:7]

# Heatmap Correlation Plot
ggcorrplot(cor(AAdatafa),colors = c('red','white','green'),type = 'lower')

# Bartlett’s Test of Sphericity
# To see if there are at least some non-zero correlations 
cortest.bartlett(cor(AAdatafa), n=100)

# KMO Measure of Sampling Adequacy (MSA)
# If MSA > 0.5, data is suitable for factor analysis
KMO(r = cor(AAdatafa))

# RESULT
# They all are, so we move to forward with with factor analysis


# NUMBER OF FACTORS

# Scree Plot
scree(cor(AAdatafa),factors = T, pc=T)  
# RESULT: Suggests 1

# Eigen Value 
# All factors with eigen value > 1 are selected
data.frame(factor = 1:ncol(AAdatafa), eigen = eigen(cor(AAdatafa))$values)
# RESULT: Suggests 1

# Parallel Analysis
# Select factors with eigen values in the original data > simulated data
fa.parallel(AAdatafa,fa='fa',fm = 'pa')
# RESULT: Suggests 2

# Total Variance Explained
# Using 2 factors 
# the total variance explained by factors should be greater than 70%
faresult <- fa(r = AAdatafa,nfactors = 2,fm = 'pa',rotate = 'none')
faresult$Vaccounted

# Extracted Commonalities
# Ideally, commonality of each variable must be greater than 0.7, 
# but a commonality greater than 0.5 may be seen as acceptable.
data.frame(communality = faresult$communality)

# Mapping Variables to Factors
print(faresult$loadings, cut=0)
# RESULT: pattern of loading does not show a clear preference of a variable for a factor

# THEREFORE: Let's rotate the axes
# Orthogonal rotation using varimax
faresult2 <- fa(r = AAdatafa,nfactors = 2,fm = 'pa',rotate = 'varimax')
faresult2$Vaccounted
data.frame(communality = faresult2$communality)
print(faresult2$loadings, cut=0.5)

# Oblique rotation using oblimin
faresult3 <- fa(r = AAdatafa,nfactors = 2,fm = 'pa',rotate = 'oblimin')
faresult3$Vaccounted
data.frame(communality = faresult3$communality)
print(faresult3$loadings, cut=0.5, sort = T)

# INTERPRETATION 
fa.diagram(faresult,sort = T)
fa.diagram(faresult2,sort = T) #varimax
fa.diagram(faresult3,sort = T)

# Representing The Factor 
factor1scores = faresult2$scores[,'PA1']
factor2scores = faresult2$scores[,'PA2']

# FA Dataframe for further analysis 
# Weighted average of variables reflecting the factor using varimax
fadataframe <- as.data.frame(faresult2$scores)

AAdatafinal <- bind_cols(AAdata2,fadataframe)

str(AAdatafinal)

# RESULT
# PA1: food_bev, cabin_service, seat_comfort, entertainment (In-flight Service)
# PA2: value_for_money, ground_service (Not In-flight Service)


# STEP 5: NATURAL LANGUGAGE PROCESSING -----------------------------------------
# options(scipen = 999)
# DATA 
AAdatafinal_reviews <- AAdatafinal %>%
  select(overall,review)

AAdatafinal_reviews <- tibble::rowid_to_column(AAdatafinal_reviews, "ID")

str(AAdatafinal_reviews)

AAdatafinal_reviews$overall = as.integer(AAdatafinal_reviews$overall)

str(AAdatafinal_reviews)

# EXPLORING
# Average and median of overall ratings
median(AAdatafinal_reviews$overall) # median overall rating
mean(AAdatafinal_reviews$overall) # mean overall rating

# Distribution of Reviews Graph
ggplot(data=AAdatafinal_reviews,aes(x=overall))+
  geom_bar(fill='steelblue', alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_x_continuous(name = "Overall Rating", n.breaks = 10)+
  scale_y_continuous(name = "Number of Reviews",limits = c(0,1600))+
  ggtitle("Distribution of Reviews")+
  coord_flip()

#proportions of reviews by rating
AAdatafinal_reviews%>%
  group_by(overall)%>% #changed
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))
# In the graph (above) we can see that most of the overall reviews were 1

# Stop words
tidytext::stop_words

# Common words
word <- c("airline","flight","american","airlines", "aa", "get","one","2")
df_common_words = data.frame(word)
df_common_words

# Common words in all reviews without stop-words 
AAdatafinal_reviews%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  anti_join(df_common_words)%>%
  group_by(word)%>%
  dplyr::summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+ #graph 
  geom_col(fill = "steelblue",alpha=8/10)+
  theme_light()+
  xlab('Word')+
  ylab('Word Count')+
  ggtitle("Common Words in all Reviews")+
  coord_flip()

# Reviews
# capital letters 
percentUpper = 100*str_count(AAdatafinal_reviews$review,pattern='[A-Z]')/nchar(AAdatafinal_reviews$review)
summary(percentUpper)

# Exclamation marks
percentExclamation = 100*str_count(AAdatafinal_reviews$review,pattern='!')/nchar(AAdatafinal_reviews$review)
summary(percentExclamation)

# Relationship with exclamation marks and uppercase letters
r_upper = cor.test(percentUpper,AAdatafinal_reviews$overall)
r_exclamation = cor.test(percentExclamation,AAdatafinal_reviews$overall)
correlations2 = data.frame(r = c(r_upper$estimate, r_exclamation$estimate),p_value=c(r_upper$p.value, r_exclamation$p.value))
rownames(correlations2) = c('Upper Case','Exclamation Marks')
correlations2 #Upper case shows moderate positive correlation, according to p-value

# Categorize
# Binary Sentiment Lexicons
as.data.frame(get_sentiments('bing'))

# After removing stopwords & Common Terms 
AAdatafinal_reviews%>%
  group_by(ID)%>%
  unnest_tokens(output = word, input = review)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  anti_join(df_common_words)%>%
  group_by(word)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>% #get count by running up to here
  ggplot(aes(x=sentiment,y=n,fill=sentiment,legend()))+ #graph 
  ylab("Number of Words")+
  xlab(element_blank())+
  scale_x_discrete(labels = c("Negative","Positive"))+
  geom_col(alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_fill_manual(values = c("red2","steelblue"),name = "Sentiment", labels = c("Negative", "Positive"))+
  theme(legend.position = "bottom")+
  ggtitle("Sentiment of Reviews", subtitle = "After removing stopwords & common terms" )+
  coord_flip()
# Count 
# neg.:13162
# pos.:6884


# After Stemming
library(SnowballC)
AAdatafinal_reviews%>%
  group_by(ID)%>%
  unnest_tokens(output = word, input = review)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  anti_join(df_common_words)%>%
  group_by(word)%>%
  select(word)%>%
  inner_join(get_sentiments("bing")) %>%
  mutate(word = wordStem(word)) %>% #stemming
  count(word, sentiment) %>%
  ungroup()%>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(alpha=8/10, show.legend = FALSE) +
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  scale_fill_manual(values = c("red2","steelblue"))+
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Proportion of positive (and negative words) for each rating **
#All words
AAdatafinal_reviews%>%
  group_by(overall)%>% #changed
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=overall,y=proportion,fill=sentiment))+
  labs(y="Proportion")+
  scale_x_continuous(name = "Rating", n.breaks = 10)+
  geom_col(alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.x = element_line(color = "grey"))+
  theme(legend.position = "top")+
  guides(fill=F)+
  scale_fill_manual(values = c("red2","steelblue"),labels = c("Negative", "Positive"))+
  ggtitle("Proportion of Sentiment by Rating ", subtitle = "Before removing stopwords")+
  coord_flip()

#Proportion of positive (and negative words) for each rating
#With no stopwords  nor common terms 
AAdatafinal_reviews%>%
  group_by(overall)%>% #changed
  unnest_tokens(output=word,input=review)%>%
  select(word)%>% #stopwords removed
  anti_join(stop_words)%>%#stopwords removed 
  group_by(word)%>%
  anti_join(df_common_words)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=overall,y=proportion,fill=sentiment))+
  labs(y="Proportion")+
  scale_x_continuous(name = "Rating", n.breaks = 10)+
  geom_col(alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.x = element_line(color = "grey"))+
  theme(legend.position = "top")+
  guides(fill=F)+
  scale_fill_manual(values = c("red2","steelblue"),labels = c("Negative", "Positive"))+
  ggtitle("Proportion of Sentiment by Rating ", subtitle = "After removing stopwords & common terms" )+
  coord_flip()

#Positive Reviews
#Let us compute the proportion of negative words for each review
AAdatafinal_reviews%>%
  group_by(ID, overall)%>% 
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(ID, overall)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_negative = negative_words/(negative_words+positive_words))%>%
  ungroup()

# Correlation
#Let us see if reviews with a lot of positive words are rated favorably.
AAdatafinal_reviews%>%
  group_by(ID, overall)%>% 
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(ID, overall)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()%>%
  summarize(correlation = cor(proportion_positive,overall))

# Correlation
#Let us see if reviews with a lot of negative words are rated less favorably.
AAdatafinal_reviews%>%
  group_by(ID, overall)%>% 
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(ID, overall)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_negative = negative_words/(negative_words+positive_words))%>%
  ungroup()%>%
  summarize(correlation = cor(proportion_negative,overall))


# Emotions in Reviews
# Remove stop words (by leveraging nrc lexicon)

#Method 2: Get Lexicon posted on github
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
                 header = F,
                 col.names = c('word','sentiment','num'),
                 sep = '\t',
                 stringsAsFactors = F)
nrc = nrc[nrc$num!=0,]
nrc$num = NULL

#Emotions is each review
AAdatafinal_reviews%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review, token = "ngrams", n = 1)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n), y=n, fill=sentiment))+
  labs(x ="Emotion", y = "Number of Words")+
  geom_col(fill = "steelblue", alpha=8/10)+
  guides(fill=F)+
  coord_flip()+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.x = element_line(color = "grey"))
# This graphs shows more positive, contradictory to other lexicons
# So let's compare emotions with ratings

#Emotions and Rating
library(tidyr)
AAdatafinal_reviews%>%
  group_by(ID,overall)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(ID,sentiment,overall)%>%
  count()%>%
  group_by(ID,sentiment, overall)%>%
  pivot_wider(names_from = sentiment,values_from = n)%>%
  mutate_at(.vars = 3:12, .funs = function(x) replace_na(x,0))%>%
  ungroup()%>%
  pivot_longer(cols = anticipation: sadness, names_to = 'sentiment',values_to = 'n')%>%
  group_by(sentiment, overall)%>%
  summarize(n = mean(n))%>%
  ggplot(aes(x=overall,y=n,fill=overall))+
  geom_col(fill = "steelblue", alpha=8/10)+
  scale_x_continuous(name = "Average Rating", n.breaks = 10)+
  ylab("Average Word Count")+
  facet_wrap(~sentiment)+
  guides(fill=F)+
  coord_flip()+
  ggtitle("Emotions and Rating")+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.x = element_line(color = "grey"))

# Correlartion
# Quantifying the relationship by examining the correlation 
# between frequency of emotions expressed and rating
AAdatafinal_reviews%>%
  group_by(ID,overall)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(ID,overall, sentiment)%>%
  count()%>%
  pivot_wider(names_from = sentiment,values_from=n)%>%
  select(ID, overall, positive, negative, trust, anticipation, joy, fear, anger, sadness, surprise, disgust)%>%
  mutate_at(.vars = 3:12, .funs = function(x) replace_na(x,0))%>%
  ungroup()%>%
  pivot_longer(cols = 3:12, names_to = 'sentiment',values_to = 'n')%>%
  group_by(sentiment)%>%
  summarize(r = cor(n,overall))
# Conclusion: Correlation between detected emotions and ratings not significant
# except for joy

# AFINN
# Let's use one more lexicon to confirm 
# Method 2: Get Lexicon from github
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)

#Visual
AAdatafinal_reviews %>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review, token = "ngrams", n = 1)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('red','blue'))+
  guides(fill=F)+
  theme_bw()

#count table
AAdatafinal_reviews %>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review, token = "ngrams", n = 1)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  group_by(reviewSentiment)%>%
  count()

# Quantified
# Conclusion: shows that the average is slightly more negative 
AAdatafinal_reviews %>%
  select(ID,review)%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))


# Step 6: Cluster Analysis -----------------------------------------------------

# PA1: food_bev, cabin_service, seat_comfort, entertainment (In-flight Services)
# PA2: value_for_money, ground_service (Ground Services)

str(AAdatafinal)
str(AAdata_review)
table(is.na(AAdatafinal))

# Purpose: Profiling clusters by needs-based variables will help us understand 
# how the four market segments differ in their needs which in turn 
# will aid the firm in offering products and services to match the unique needs of the segment.
# Profiling clusters on demographics or observable variables will help identify customers and target them with the right offer.


# Hierarchical Cluster Analysis

# Selecting previously standardized variables
# Factor loadings from the factor analysis solution.
AAClusterAnalysis = AAdatafinal %>%
  select(PA1, PA2)
AAClusterAnalysis

# Define Similarity Measure
# Euclidean distance to assess similarity
d_h = dist(x=AAClusterAnalysis, method='euclidean')
#Clustering Method
clusters1 = hclust(d=d_h, method='ward.D2')

# Intepret Results
# Examine Dendrogram
plot(clusters1)

# Goodness of fit
cor(cophenetic(clusters1),d_h)

# Number of clusters
plot(cut(as.dendrogram(clusters1),h=12)$upper, main= "Cluster Dendrogram")
rect.hclust(tree=clusters1,k = 2,border='tomato')
rect.hclust(tree=clusters1,k = 3,border='tomato')
rect.hclust(tree=clusters1,k = 4,border='tomato')


h_segments = cutree(tree=clusters1, k=3)
h_segments_alt = cutree(tree=clusters1, k=4)
table(h_segments)
table(h_segments_alt)

# AAdatafa
# Visualize hierarchical clustering in 2 dimensions using factor analysis
# h_segments
temp = data.frame(Cluster = factor(h_segments),
                  Factor_1 = fa(AAClusterAnalysis,nfactors = 2,rotate = 'varimax')$scores[,1],
                  Factor_2 = fa(AAClusterAnalysis,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=Factor_1,y=Factor_2,col=Cluster))+
  labs(y="Factor 1")+
  scale_x_continuous(name = "Factor 2")+
  geom_point(alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  theme(legend.position = "bottom")+
  ggtitle("Hierarchical Cluster Plot", subtitle = "Factor analysis with varimax rotation" )

# K-means Clustering
set.seed(617)
km = kmeans(x = AAClusterAnalysis,centers = 3,iter.max=10000,nstart=25)

table(km$cluster)

## Number of Clusters
# Total within sum of squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = AAClusterAnalysis,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  ylab("Within S.S.")+
  xlab("Number of Clusters")+
  geom_line(col='steelblue',size=1.2)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  geom_point()+
  ggtitle("Total Within Sum of Squares Plot")+
  scale_x_continuous(breaks=seq(1,10,1))

# Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = AAClusterAnalysis,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  ylab("Ratio S.S.")+
  xlab("Number of Clusters")+
  geom_line(col='steelblue',size=1.2)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  geom_point()+
  ggtitle("The Ratio Between The Sum of Squares")+
  scale_x_continuous(breaks=seq(1,10,1))

# Silhouette Plot
library(cluster)
silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = AAClusterAnalysis,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  ylab("Silhouette Width")+
  xlab("Number of Clusters")+
  geom_line(col='steelblue',size=1.2)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  geom_point()+
  ggtitle("Silhouette Analysis")+
  scale_x_continuous(breaks=seq(2,10,1))

# Visualize clustering in 2 dimensions using factor analysis
# k_segments 
k_segments = km$cluster
table(k_segments)

temp2 = data.frame(cluster = factor(k_segments),
                   factor_1 = fa(AAClusterAnalysis,nfactors = 2,rotate = 'varimax')$scores[,1],
                   factor_2 = fa(AAClusterAnalysis,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp2,aes(x=factor_1,y=factor_2,col=cluster))+
  labs(y="Factor 2",x="Factor 1")+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  ggtitle("Clusters on a Scatterplot")+
  geom_point()


#Model-based Clustering
clusters_mclust = Mclust(AAClusterAnalysis)
summary(clusters_mclust)

clusters_mclust2 = Mclust(AAClusterAnalysis,G=3)
summary(clusters_mclust2)

clusters_mclust3 = Mclust(AAClusterAnalysis,G=4)
summary(clusters_mclust3)

# Plot of bic (Bayesian Information Criterion)
# We are looking for the lowest bic in the line graph
mclust_bic = sapply(1:10,FUN = function(x) -Mclust(AAClusterAnalysis,G=x)$bic)
mclust_bic

ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))



# Let's go with either hierarchical clustering or k-means clustering 
# as it's unrealistic to have 9 different clusters although it's associated with the best BIC
# Merging clusters back to the main dataset

AAdatafinalclusters = cbind(AAdatafinal, h_segments, k_segments)

str(AAdatafinalclusters)


# Cluster Profiling and Key Characteristics
# Profile Segments by Needs
# h_segments 
AAdatafinalclusters %>%
  select(overall:value_for_money,recommender,PA1,PA2,h_segments)%>%
  group_by(h_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,overall:value_for_money,recommender,PA1,PA2)%>%
  ggplot(aes(x=var,y=value,fill=factor(h_segments)))+
  geom_col(position='dodge')+
  labs(y="Value",x="Variables")+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  ggtitle("Cluster Profiling and Key Characteristics")+
  coord_flip()

# k_segments ****
AAdatafinalclusters %>%
  select(overall:value_for_money,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,overall:value_for_money)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge',alpha=8/10)+
  labs(y="Rating Average",x="Variables")+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_fill_manual(values = c("orange2","olivedrab","plum3"),name = "Segments")+
  theme(legend.position = "bottom")+
  ggtitle("Profile by Needs", subtitle = "K-Means segments" )+
  coord_flip()

# Profile Segments by characteristics
# Proportions by clusters
# Reason for travel 
round(prop.table(table(AAdatafinalclusters$h_segments,AAdatafinalclusters[,9]),1), digits = 2)

# h_segments
lapply(9:15,function(x) {
  dat = round(prop.table(table(AAdatafinalclusters$h_segments,AAdatafinalclusters[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=6)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Blues'))
})

# k_segments **
lapply(9:15,function(x) {
  dat = round(prop.table(table(AAdatafinalclusters$k_segments,AAdatafinalclusters[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=6)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Blues'))
})


# k_segments does a better job than h_segments in distinguishing market segments, 
# we'll move forward with clusters from k-means clustering


# Step 7: Text Mining and Sentiment Analysis by Clusters -----------------------

# Sentiment Analysis by Clusters - k_segments **
AAdatafinalclusters%>%
  unnest_tokens(output = word, input = review)%>%
  select(k_segments, word)%>%
  anti_join(stop_words)%>%
  group_by(k_segments,word)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(k_segments, sentiment)%>%
  count()%>%
  arrange(desc(n)) %>%
  ggplot(aes(x=reorder(sentiment,X = n), y=n, fill=sentiment,legend()))+ #graph 
  ylab("Number of Reviews")+
  xlab(element_blank())+
  scale_x_discrete(labels = c("Positive","Negative"))+
  geom_col(alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_fill_manual(values = c("red2","steelblue"),name = "Sentiment", labels = c("Positive","Negative"))+
  theme(legend.position = "bottom")+
  ggtitle("Sentiment of Reviews by Clusters", subtitle = "Valence" )+
  facet_wrap(~k_segments)

nrc = read.csv(file = 'nrc.csv', stringsAsFactors = F)
str(nrc)

# Emotions by Cluster - k_segments
AAdatafinalclusters%>%
  unnest_tokens(output = word, input = review)%>%
  select(k_segments,word)%>%
  anti_join(stop_words)%>%
  group_by(k_segments,word)%>%
  inner_join(nrc)%>%
  ungroup() %>%
  group_by(k_segments,sentiment)%>%
  count() %>%
  ungroup() %>%
  group_by(k_segments) %>%
  mutate(proportion=n/sum(n))%>%
  arrange(desc(proportion)) %>%
  ggplot(aes(x=reorder(sentiment,X = proportion), y=proportion))+
  labs(y="Proportion", x="Emotions")+
  geom_col(fill = "steelblue",alpha=8/10)+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  ggtitle("Sentiment of Reviews by Clusters", subtitle = "Emotions" )+
  facet_wrap(~k_segments)+
  coord_flip()


# Cluster 1
# Represents the most dissatisfied customers while 
# cluster 3 represents the most satisfied customers

# Text Mining by Clusters
# Segment 1 Common Words - Most Dissatisfied 
# The least likely to travel in first or business class
# which may be one of the drivers to their poor in-flight experience
AA3_1 = AAdatafinalclusters %>%
  filter(k_segments==1) %>%
  unnest_tokens(input = review, output = word) %>%
  select(k_segments,word) %>%
  anti_join(stop_words) %>%
  anti_join(df_common_words) %>%
  inner_join(get_sentiments('bing')) %>%
  group_by(word,sentiment) %>%
  count()

AA3_1 = AA3_1 %>%
  group_by(word) %>%
  mutate(proportion=n/sum(AA3_1$n))
view(AA3_1)

AA3_1 %>%
  group_by(sentiment) %>%
  arrange(desc(proportion)) %>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,proportion), y=proportion, fill=sentiment))+
  geom_col(alpha=8/10)+
  theme_light()+
  xlab('Word')+
  ylab('Word Proportion')+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_fill_manual(values = c("red2","steelblue"),name = "Sentiment", labels = c("Positive","Negative"))+
  theme(legend.position = "bottom")+
  ggtitle("Common Words in Segment 1 by Sentiment")+
  coord_flip()

# Segment 2 Common Words 
AA3_2 = AAdatafinalclusters %>%
  filter(k_segments==2) %>%
  unnest_tokens(input = review, output = word) %>%
  select(k_segments,word) %>%
  anti_join(stop_words) %>%
  anti_join(df_common_words) %>%
  inner_join(get_sentiments('bing')) %>%
  group_by(word,sentiment) %>%
  count()

AA3_2 = AA3_2 %>%
  group_by(word) %>%
  mutate(proportion=n/sum(AA3_2$n))
view(AA3_2)

AA3_2 %>%
  group_by(sentiment) %>%
  arrange(desc(proportion)) %>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,proportion), y=proportion, fill=sentiment))+
  geom_col()+
  theme_light()+
  xlab('Word')+
  ylab('Word Proportion')+
  ggtitle("Common Words in Segment 2 by Sentiment")+
  coord_flip()

# Segment 3 Common Words - Most Satisfied 
# higher percentage of business or first class passengers
# also least likely to have experienced a flight delay/cancellation or layover.  
AA3_3 = AAdatafinalclusters %>%
  filter(k_segments==3) %>%
  unnest_tokens(input = review, output = word) %>%
  select(k_segments,word) %>%
  anti_join(stop_words) %>%
  anti_join(df_common_words) %>%
  inner_join(get_sentiments('bing')) %>%
  group_by(word,sentiment) %>%
  count()

AA3_3 = AA3_3 %>%
  group_by(word) %>%
  mutate(proportion=n/sum(AA3_3$n))
view(AA3_3)

AA3_3 %>%
  group_by(sentiment) %>%
  arrange(desc(proportion)) %>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,proportion), y=proportion, fill=sentiment))+
  geom_col(alpha=8/10)+
  theme_light()+
  xlab('Word')+
  ylab('Word Proportion')+
  theme(text = element_text(family = "Helvetica Neue",color = "black"))+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  scale_fill_manual(values = c("red2","steelblue"),name = "Sentiment", labels = c("Positive","Negative"))+
  theme(legend.position = "bottom")+
  ggtitle("Common Words in Segment 3 by Sentiment")+
  coord_flip()
