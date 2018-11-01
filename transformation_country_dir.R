#Country transformation
plot(imdb.data.noNA.genres$country)
sort(table(imdb.data.noNA.genres$country), decreasing = TRUE) # 5 countries dominate
#select the names of top 5 countries
top_countries = names(head(sort(table(imdb.data.noNA.genres$country), decreasing = TRUE), 5))
top_countries

# perform transformation
country1 = ifelse(imdb.data.noNA.genres$country %in% top_countries, levels(imdb.data.noNA.genres$country)[imdb.data.noNA.genres$country], "Other")

sort(table(country1), decreasing = TRUE)


#Directors

sort(table(imdb.data.noNA.genres$director), decreasing = TRUE) # 5 directors more than 15 movies each
#select the names of top 5 countries
top_dir = names(head(sort(table(imdb.data.noNA.genres$director), decreasing = TRUE), 5))
top_dir

# perform transformation
director1 = ifelse(imdb.data.noNA.genres$director %in% top_dir, levels(imdb.data.noNA.genres$director)[imdb.data.noNA.genres$director], "Other")

sort(table(director1), decreasing = TRUE)


#Languajes
languaje1 = ifelse(imdb.data.noNA.genres$language == 'English', 'English',"Other")
table(languaje1)
