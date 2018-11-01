# Combining transformations

write.csv(imdb.data.noNA.genres.transform,'data_transformed_final.csv')


cbind(imdb.data.noNA.genres,country1,director1,distributor_1,production_company_1,release.dates[,c(6:7)])


imdb.data.noNA.genres.transform = cbind(imdb.data.noNA.genres[,-c(44,45,46,47,50,51,52,54,55,57,58,62,63,65,66)],
                                        adventure.war.western,
                                        biography.documentary.history,
                                        drama.filmnoir,
                                        animation.family.musical,
                                        action.sport,
                                        country1,director1,distributor_1,production_company_1,release.dates[,c(6:7)])

names(imdb.data.noNA.genres.transform)
for(i in 44:56){
  imdb.data.noNA.genres.transform[,i]=as.factor(imdb.data.noNA.genres.transform[,i])}


####################### Pure Model #############################################################
10
predictors = c(3,4,7,13,15,17,19,21,23,25,27,29,30,31,32,33,34,35,36,37,38,39,40,44:62)


names(imdb.data.noNA.genres.transform[,predictors])

side = c(5,6,14,42,9,12)

reg0 = glm(data= imdb.data.noNA.genres.transform[,predictors], formula = imdb_score~.)
summary(reg0)


cv.glm(imdb.data.noNA.genres.transform[,predictors],reg0,K = 10)$delta[1]
  
#Checking for linearity High p-value means linear
residualPlots(reg0)




######################As Languaje is mainly english, it doesn't add to explain the var of the dependant variable) ###########

###################### Cutting non significant linear variables - Model 2 ######################

names(imdb.data.noNA.genres.transform)

predictors2 = c(3,4,7,13,15,27,29,30,33,35,36,38,40,44:62)
names(imdb.data.noNA.genres.transform[,predictors2])


reg1 = glm(data= imdb.data.noNA.genres.transform[,predictors2], formula = imdb_score~.)
summary(reg1)

cv.glm(imdb.data.noNA.genres.transform[,predictors2],reg1,K = 30)$delta[1]

str(imdb.data.noNA.genres.transform[,predictors2])

###################### Big Empanada ######################

(11^3)/(6^5)
1.5*20

predictors2 = c(3,4,7,13,15,27,29,30,33,35,36,38,40,44:62)
categorical2 = names(imdb.data.noNA.genres.transform[c(4,29,44:62)])


cv.error.sk=data.frame(matrix(ncol=11,nrow=10^4))
names(cv.error.sk)=c('a','b','c','d','e','f','g','h','i','j','MSE')
z=1
names(imdb.data.noNA.genres.transform[,predictors2])

for(a in 1:4){
  for(b in 1:4){
    for(c in 1:4){
      for(d in 1:4){
        for(e in 1:4){
          for(f in 1:4){
            for(g in 1:4){
              for(h in 1:4){
                for(i in 1:4){
                  for(j in 1:4){
                      
          reg_sk = glm(data = imdb.data.noNA.genres.transform[,predictors2],
                       imdb_score~release_month+color+Comedy+Crime+Fantasy+Horror+Mystery+Romance+`Sci-Fi`+Thriller+
                         adventure.war.western+biography.documentary.history+drama.filmnoir+animation.family.musical+
                         biography.documentary.history+drama.filmnoir+animation.family.musical+action.sport+country1+
                         director1+distributor_1+production_company_1+weekday+decade+
                         bs(duration_mins,degree = a)+
                         bs(number_news_articles,degree = b)+
                         bs(director_facebook_likes,degree = c)+
                         bs(actor_3_star_meter,degree = d)+
                         bs(critic_reviews_number,degree = e)+
                         bs(number_of_faces_in_movie_poster,degree = f)+
                         bs(movie_budget,degree = g)+
                         bs(movie_facebook_likes,degree = h)+
                         bs(ratio_movie_cast_likes,degree = i)+
                         bs(number_of_votes,degree = j))
          
          cv.error.sk[z,] = c(a,b,c,d,e,f,g,h,i,j,cv.glm(imdb.data.noNA.genres.transform[,predictors2],reg_sk,K = 30)$delta[1])
          Sys.sleep(0.1)
          print(paste(as.character(round(z/nrow(cv.error.sk),digits = 5)*100),'%'))# % progress 
          flush.console()  
          z=z+1
                    
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

4^11
