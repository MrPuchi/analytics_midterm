
plot()


str(imdb.data.noNA.genres.transform[,predictors.kfold])
predictors.kfold = c(3,4,7,10,13,15,17,19,21,23,25,27,29,30,31,32,33,34,35,36,37,38,39,40,44:72)

plot(imdb.data.noNA.genres.transform$release_month) #k

plot(imdb.data.noNA.genres.transform$content_rating) #goes away

plot(imdb.data.noNA.genres.transform$country1) #goes away

plot(imdb.data.noNA.genres.transform$director1) #director

plot(imdb.data.noNA.genres.transform$distributor_1) 

nearZeroVar(x = imdb.data.noNA.genres.transform,  )

nzv <- nearZeroVar(imdb.data.noNA.genres.transform, saveMetrics= TRUE) 
nzvIndices <- which(nzv$nzv == TRUE)
nzv[nzvIndices, ]

plot(imdb.data.noNA.genres.transform$Animation)


names(imdb.data.noNA.genres.transform[44:66])
str(imdb.data.noNA.genres.transform)

attach(imdb.data.noNA.genres.transform)

detach(imdb.data.noNA.genres.transform)

as.numeric(Music) + as.numeric(Musical)

as.numeric(imdb.data.noNA.genres.transform[44:66])

adventure.war.western = as.numeric(War) + as.numeric(Western) + as.numeric(Adventure)
biography.documentary.history = as.numeric(Biography) + as.numeric(History) + as.numeric(Documentary)
drama.filmnoir = as.numeric(`Film-Noir`) + as.numeric(Drama)
animation.family.musical = as.numeric(Animation) + as.numeric(Family) + as.numeric(Music) + as.numeric(Musical)
action.sport = as.numeric(Sport) + as.numeric(Action)


# Replacing values greater than 1 for 1
adventure.war.western = as.character(ifelse(adventure.war.western >= 1, 1,0))
biography.documentary.history = as.character(ifelse(biography.documentary.history >= 1, 1,0))
drama.filmnoir = as.character(ifelse(drama.filmnoir >= 1, 1,0))
animation.family.musical = as.character(ifelse(animation.family.musical >= 1, 1,0))
action.sport = as.character(ifelse(action.sport >= 1, 1,0))





# Horror + Mistery
short  # we are gonna get rid of the shorts





  
  ggcorr(as.numeric(imdb.data.noNA.genres.transform[,c(44:66)]))



sort(table(imdb.data$genres), decreasing = TRUE)
