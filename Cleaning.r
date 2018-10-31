#packages


#Cleaning Script
imdb.data = read.csv('data_final.csv')
str(imdb.data)
names(imdb.data)
imdb.data = imdb.data[,-c(60:71)] #removing X columns

## Correcting wrong categorical variables into numeric variables
imdb.data$actor_1_star_meter = as.numeric(imdb.data$actor_1_star_meter)
imdb.data$actor_2_star_meter = as.numeric(imdb.data$actor_2_star_meter)
imdb.data$actor_3_star_meter = as.numeric(imdb.data$actor_3_star_meter)
imdb.data$critic_reviews_number = as.numeric(imdb.data$critic_reviews_number)
imdb.data$user_reviews_number = as.numeric(imdb.data$user_reviews_number)
imdb.data$user_votes_number = as.numeric(imdb.data$user_votes_number)
imdb.data$actor_3_facebook_likes = as.numeric(imdb.data$actor_3_facebook_likes)
imdb.data$aspect_ratio = as.factor(imdb.data$aspect_ratio)
# imdb.data$release_year = as.factor(imdb.data$release_year)
# imdb.data$release_month = as.factor(imdb.data$release_month)
# imdb.data$release_day = as.factor(imdb.data$release_day)




unique(imdb.data$aspect_ratio)

## Identifying NAs in rows

getNA = function(row){ any(is.na(row))}
rows.NA = which(apply(imdb.data, 1, getNA)==TRUE)  # get which rows have at least one NA
length(rows.NA)/nrow(imdb.data) #Calculate % of rows with NA values

## Identifying NAs in columns
col.NA = which(apply(imdb.data, 2, getNA)==TRUE)  # get which cols have at least one NA

sort(apply(imdb.data[,col.NA], 2, function(cols) {sum(is.na(cols))/length(cols)}),decreasing = TRUE)#Calculate % of NAs per predictor

#budget and movie budget have the largest amount of NA values, let's explore those two:
summary(imdb.data$budget)
summary(imdb.data$movie_budget)
boxplot(imdb.data$budget,imdb.data$movie_budget)
plot(imdb.data$budget,imdb.data$movie_budget)
cor(imdb.data$budget,imdb.data$movie_budget, use = "complete.obs")

#We can eliminate one of the columns, we will go with budget and also plot_keywords (both greater than 1% of NAs)

imdb.data.noNA = imdb.data[,-c(8,36)]
rows.NA2 = which(apply(imdb.data.noNA, 1, getNA)==TRUE)   
length(rows.NA2)/nrow(imdb.data.noNA) 

medianImput <- preProcess(imdb.data.noNA, method='medianImpute') #predicting missing numerical variables using medianImpute

imdb.data.noNA2 = predict(medianImput,imdb.data.noNA)
rows.NA3 = which(apply(imdb.data.noNA2, 1, getNA)==TRUE)   
length(rows.NA3)/nrow(imdb.data.noNA2)
boxplot(imdb.data.noNA2$movie_budget)
col.NA2 = which(apply(imdb.data.noNA2, 2, getNA)==TRUE)  # get which cols have at least one NA
sort(apply(imdb.data.noNA2[,col.NA2], 2, function(cols) {sum(is.na(cols))/length(cols)}),decreasing = TRUE)#Calculate % of NAs per predictor



imdb.data.noNA3 = imdb.data.noNA2[-rows.NA3,] #This is the database without NAs

names(imdb.data.noNA3)

