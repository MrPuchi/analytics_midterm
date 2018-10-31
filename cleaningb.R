names(imdb.data.noNA.genres)



table(imdb.data.noNA.genres$director)

hist(imdb.data.noNA.genres$imdb_score)
summary(imdb.data.noNA.genres$imdb_score)

imdb.data.noNA.genres[grepl('Meyers', imdb.data.noNA.genres$director),]

unique(imdb.data.noNA.genres$director)

predictors = c(3,4,7,8,10,11,13,15,17,19,21,23,25,27,29,30,31,32,33,34,35,36,37,38,39,40,44:66)

side = c(5,6,14,42,9,12)

not = c(1,2,16,18,20,22,24,26,28,41,43)

reg0 = lm(data= imdb.data.noNA.genres[,predictors], formula = imdb_score~.)
summary(reg0)


names(imdb.data.noNA.genres[,predictors])

table(imdb.data.noNA.genres$color)

boxplot

table(imdb.data.noNA.genres$actor_1_facebook_likes)

