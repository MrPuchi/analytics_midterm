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

split.df %>%
  separate(genres, into= c('g1','g2','g3','g4'),sep = '||')


head(split.df)


colsplit(split.df$genres, "|", c('g1','g2','g3','g4'))

gsub(split.df$genres,pattern = '|',replacement = ",")

replace()
  