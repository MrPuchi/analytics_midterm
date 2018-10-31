#Modeling

fit = lm(data=imdb.data.cont, imdb_score~.)
summary(fit)


# WE found movies with the same movie_id, here we are changing the id since it doesn't affect reggression
imdb.data.noNA3[imdb.data.noNA3$title=='Despicable Me 2',]$movie_id='9999999'
imdb.data.noNA3[imdb.data.noNA3$title=='The Rocker',]$movie_id='9999998'
imdb.data.noNA3 = imdb.data.noNA3[-which(imdb.data.noNA3$movie_id=='865'),] #trash data row, Lost Souls 2000 
  


split.df = imdb.data.noNA3[,c(1,31)]

split.df2 = split.df %>%
  separate(genres, into= c('g1','g2','g3','g4','g5','g6','g7','g8'),sep = '\\|')

split.df3=melt(split.df2,id.vars = 'movie_id',na.rm = TRUE)


split.df4 = split.df3[,-2]

split.df5 = dcast(split.df4,movie_id~value)
split.df6 =  split.df5 
split.df6[!is.na(split.df6)]=1
split.df6[is.na(split.df6)]=0
split.df7=cbind(movie_id=split.df5$movie_id,split.df6)
split.df7=split.df7[,-2]



imdb.data.noNA4 = imdb.data.noNA3[,-c(31,38:50)] # We remove old genre columns and the column "genres"

imdb.data.noNA.genres = merge(imdb.data.noNA4,split.df7,by = 'movie_id')
names(imdb.data.noNA.genres)

for(i in 44:67){
  imdb.data.noNA.genres[,i]=as.factor(imdb.data.noNA.genres[,i])}
str(imdb.data.noNA.genres)

#getting numeric data
imdb.data.cont = select_if(imdb.data.noNA.genres, is.numeric)

str(imdb.data.cont)
cor.matrix = cor(imdb.data.cont)

ggcorr(imdb.data.noNA.genres)





imdb.data.cont.linear = imdb.data.cont[,c(1,2,5,8,9,11,13,14,15,18,23)]
reg1.linear = lm(data = imdb.data.cont.linear, formula = imdb_score~.)
residualPlots(reg1.linear)                                       
summary(reg1.linear)
names(imdb.data.cont.linear)

imdb.data.cont.linear2=imdb.data.cont.linear[,-c(4,11,10)]
reg1.linear2=lm(imdb.data.cont.linear2, formula = imdb_score~.)
residualPlots(reg1.linear2) 
names(imdb.data.cont.linear2)

imdb.data.cont.linear3=imdb.data.cont.linear2[,-7]
reg1.linear3=lm(imdb.data.cont.linear3, formula = imdb_score~.)
residualPlots(reg1.linear3) 

summary(reg1.linear3)


reg1.linear3=lm(imdb.data.cont.linear3, formula = imdb_score~.)
residualPlots(reg1.linear3) 

summary(reg1.linear3)