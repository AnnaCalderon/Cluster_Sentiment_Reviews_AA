# Cluster and Sentiment Analyses on Customer Reviews
- Extracted customer sentiment from reviews using text mining
- Applied cluster analysis to identify segments with differentiating needs and characteristics
- Provided insights regarding which services need the most improvement

## Dataset and Resources Used
**R Version:** 1.4.1717\
**Packages:**\
**Data:** [Raw Data](https://www.kaggle.com/efehandanisman/skytrax-airline-reviews)

## Data Preparation 
1. The raw dataset includes 17 variables 
2. A subset of 8 relevant variables was selected
    - Review (free text)
    - Overall Rating (numerical)
    - Food and Beverage Rating (numerical)
    - Cabin Service Rating (numerical)
    - Seat Comfort Rating (numerical)
    - Entretainment Rating (numerical)
    - Value for Money Rating (numerical)
    - Ground Service Rating (numerical)
3. Imputation of Missing Values 

## Initial Sentiment Analysis Using the Overall Rating
- Exploration of the proportion of positive and negative words for each rating (1-10)
- Three general-purpose lexicons were evaluated to against ratings: bing, ncr, and AFINN 
- The bing lexicon, as it presented a significant correlation with the overall rating
- Exploration also indicated a prevalence of passenger dissatisfaction

<img width="600" alt="Proportion of Sentiment by Rating" src="https://user-images.githubusercontent.com/52983514/132456477-fcd24d39-6cc1-4d0c-b916-8949a494b583.png">

## Dimension Reduction with Factor Analysis
- Through factor analysis, we found that using two components would explain 74% of the six rated categories 
<img width="700" alt="Factor Analysis" src="https://user-images.githubusercontent.com/52983514/132464590-9d9e575f-9201-4fd8-a875-10238c6f966b.png">

## Cluster Analyses
- To determine the optimal number of clusters multiple methods
- Results from Analyses suggest a three-cluster solution 

### Hierarchical Clustering
<img width="600" alt="Clusters 1" src="https://user-images.githubusercontent.com/52983514/132458970-3acd531e-5127-480c-9479-c9c1e231e385.png">

### K-Means Clustering
<img width="500" alt="Clusters 2" src="https://user-images.githubusercontent.com/52983514/132461808-a35f2b3c-3ac3-45d4-be42-7683e5d5b894.png">

## Sentiment by Clusters
- The highest prevalence of positive reviews was in Cluster 3
- The highest prevalence of negative reviews was in Cluster 1 
<img width="500" alt="Sentiment by Clusters" src="https://user-images.githubusercontent.com/52983514/132461003-851bea23-b0d8-4332-b820-eb8ddc1d4c03.png">

## Clusters Profiling
### Profile Segments by Needs
Cluster 1: 
- Represent the most unsatisfied passengers overall
- Reviews suggest that negative ratings are largely driven by:
    - Poor customer service
    - Flight delays
    - Overall poor experience across pre-flight(PA2), in-flight(PA1), and post-flight categories(PA2)

Cluster 2: 
- More likely to have negative pre-flight experiences, such as flight delays or cancellations
- In comparison to Cluster 2, they are slightly more satisfied with the in-flight experience

Cluster 3: 
- Represent the most satisfied passengers overall
- Provided relatively positive text reviews and ratings across all measures. 
- Reviews are largely positive and primarily attributed to the in-flight experience (PA1)
    
**Profiling Across Factors**\
<img width="500" alt="Profile by needs - factors" src="https://user-images.githubusercontent.com/52983514/132473792-0e6c50ac-2201-4b33-ad98-8e31a29da487.png">\
**Profiling Across Individual Variables**\
<img width="500" alt="Profile - K-means" src="https://user-images.githubusercontent.com/52983514/132473732-decb783a-0df9-4a24-8585-e979c2d605c8.png">\
**Common Words on Reviews**\
<img width="700" alt="Seg 1 and 3" src="https://user-images.githubusercontent.com/52983514/132484343-5e453000-777b-4429-b51f-e72eebd70822.png">

### Profile Segments by Characteristics
Cluster 1: 
- Least likely to travel in first or business class
- The least likely to have experienced a flight delay/cancellation or layover

Cluster 2: 
- The most likely to have experienced a flight delay, consistent with common words
- The most likely to have a connecting flight

Cluster 3:
- Higher percentage of business or first class passengers
- They appear less likely to travel for leisure purposes, compared to Cluster 1 and 2

<img width="700" alt="Profiling on Observable Variables" src="https://user-images.githubusercontent.com/52983514/132465349-eafe3f58-dd79-43e7-a43f-015ff70731c9.png">
