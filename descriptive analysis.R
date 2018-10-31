
install.packages("ggpubr")
library("dplyr")
library("ggpubr")
install.packages("moments")
library(moments)

hist(imdb.data$imdb_score)
mean(imdb.data$imdb_score)
median(imdb.data$imdb_score)
shapiro.test(imdb.data$imdb_score)

qqPlot(imdb.data$imdb_score)
ggdensity(imdb.data$imdb_score)
skewness(imdb.data$imdb_score)
kurtosis(imdb.data$imdb_score)

qplot(imdb.data.noNA.genres$imdb_score, geom = 'histogram', binwidth = 0.1) + xlab('Rating') + ylab('Frequency')

ggplot(, aes(x = n.sample), binwidth = 0.1) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + 
  ylab(expression(bold('Density')))