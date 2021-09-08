# Cluster and Sentiment Analyses on Customer Reviews
- Extracted customer sentiment from reviews using text mining
- Applied cluster analysis to categorize every service rated by customers
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
![Proportion of Sentiment by Rating](https://user-images.githubusercontent.com/52983514/132456477-fcd24d39-6cc1-4d0c-b916-8949a494b583.png)

## Factor Analysis
![Factor Analysis](https://user-images.githubusercontent.com/52983514/132464590-9d9e575f-9201-4fd8-a875-10238c6f966b.png)


## Cluster Analysis
![image](https://user-images.githubusercontent.com/52983514/132458970-3acd531e-5127-480c-9479-c9c1e231e385.png)

![image](https://user-images.githubusercontent.com/52983514/132461808-a35f2b3c-3ac3-45d4-be42-7683e5d5b894.png)

## Sentiment by Clusters
![image](https://user-images.githubusercontent.com/52983514/132461003-851bea23-b0d8-4332-b820-eb8ddc1d4c03.png)

## Clusters Profiling
### Profile Segments by Needs
![image](https://user-images.githubusercontent.com/52983514/132471342-92020e74-4d1f-4103-807c-ec5c11dbcb2c.png)
![image](https://user-images.githubusercontent.com/52983514/132471525-ba8f57cc-57fd-484e-8287-a5f89093947e.png)

### Profile Segments by Characteristics
<img width="200" alt="Profiling on Observable Variables" src="https://user-images.githubusercontent.com/52983514/132465349-eafe3f58-dd79-43e7-a43f-015ff70731c9.png">

