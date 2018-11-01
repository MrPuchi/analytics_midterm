names(imdb.data.noNA.genres)

release.dates = imdb.data.noNA.genres[,c(4:6)]

release.dates$month_num = match(release.dates$release_month,month.abb)


release.dates$full.date = paste(release.dates$release_year, release.dates$month_num, 
                                release.dates$release_day, sep="-")%>% ymd() %>% as.Date()

release.dates$weekday = wday(release.dates$full.date)


for (i in 1:nrow(release.dates)){
  if(release.dates[i,3] < 1980){
    release.dates$decade[i] = 'before 80s'
  }else if (release.dates[i,3] < 1990){
    release.dates$decade[i] = '80s'
  }else if (release.dates[i,3] < 2000){
    release.dates$decade[i] = '90s'
  }else if (release.dates[i,3] < 2010){
    release.dates$decade[i] = '2000s'
  }else if (release.dates[i,3] < 2020){
    release.dates$decade[i] = '2010s'
  }
}

release.dates$weekday = as.factor(release.dates$weekday)
release.dates$decade = as.factor(release.dates$decade)

#########################################################

