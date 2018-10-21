#Modeling

fit = lm(data=imdb.data.cont, imdb_score~.)
summary(fit)



names(imdb.data.noNA3)

unique(imdb.data.noNA3$genres)



split.df = imdb.data.noNA3[,c(1,31)]

char = as.character(imdb.data.noNA3$genres,split)

split.list = strsplit(char,'|',fixed = TRUE)

unique(unlist(split.list))

separate(split.df,col = genres, sep = '|',into = )

split.df2 = split.df %>%
  separate(genres, into= c('g1','g2','g3','g4','g5','g6','g7','g8'),sep = '\\|')

split.df3=melt(split.df2,id.vars = 'movie_id',na.rm = TRUE)
split.df3[sort(split.df3$movie_id),]

split.df4 = split.df3[,-2]

split.df5 = dcast(split.df4,movie_id~value)

imdb.data.noNA4 = imdb.data.noNA3[]
names()


  