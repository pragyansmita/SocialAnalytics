# Load required libraries
if (!require(shiny))
  install.packages("shiny")
if (!require(tm))
  install.packages("tm")
if (!require(wordcloud))
  install.packages("wordcloud")
if (!require(twitteR))
  install.packages("twitteR")

# Global declared outside the shinyServer function. 
# This makes it available anywhere inside shinyServer. 
# This code in server.R outside shinyServer is only run once when the app starts up, 
# so it can't contain user input.
consumer_key <- 'c2tNsuCoTOKEYODzs8eSEaUgh'
consumer_secret <- 'XuFZtszml3rLixJ7Hke8u6kcAt5Jw060It4Paw0NbCrkOzSUnk'
access_token <- '528020973-zC9vkOaUMfIkFolHkw1qwOhnJEPmwA7vkksToBjB'
access_secret <- 'sjkwX2lEH961FNFnc11lWxWWSTJotJd8DyX33FGqUz9K6'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
token <- get("oauth_token", twitteR:::oauth_cache) #Save the credentials info
token$cache()

negTerms <- c("sad","unacceptable", "fail", "crumbling" , "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c("Congrats", "inspiring", "progress", "success", "compassion", "achieve", "benchmark", "amazing", "wonderful", "awesome", "valuable", "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious", "uproarious", "riveting", "fascinating", "dazzling", "legendary")

#geo1 <- as.numeric(geocode("Fairfax,VA"))
#geo1 <- as.numeric(geocode("VA"))
#distVal <- "100mi" # 100 miles
#geoSearchTerm <- paste(geo1[2],",",geo1[1],",",distVal, sep="")


shinyServer(function(input, output, session) {
  output$currentTime <- renderText({invalidateLater(1000, session) #Here I will show the current time
                                    paste("Current time is: ",Sys.time())})
  observe({
    invalidateLater(60000,session)
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    vector_users <- vector()
    vector_text <- vector()
    vector_sentiments <- vector()
    tweets_result = ""
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()

    #tweets_result = searchTwitter("VeteranAffairs") #Here I use the searchTwitter function to extract the tweets, e.g.,"VeteranAffairs"
    tweets_result = searchTwitter(input$searchTag, n=input$numTweet) 
    
    for (tweet in tweets_result){
      # Display twitter id and (re)tweeted text
      print(paste(tweet$screenName, ":", tweet$text))
      vector_users <- c(vector_users, as.character(tweet$screenName)); #save the user name 
      vector_text <- c(vector_text, as.character(tweet$text)); #save the tweeted text
      
      # Display location
      #if ( is.null(tweet$latitude) & is.null(tweet$longitude) ) {
      if ( is.null(tweet$latitude)  == FALSE ) {
        print("Location: Not Available")
      }
      else {
        print(paste("Latitude: ", tweet$latitude, " Longitude: ", tweet$longitude))
      }
      
      # if positive words match...
#       if (  grepl("Amazing", tweet$text, ignore.case = TRUE) == TRUE | 
#               grepl("Wonderful", tweet$text, ignore.case = TRUE) | 
#               grepl("Awesome", tweet$text, ignore.case = TRUE) | 
#               grepl("Drumbeat", tweet$text, ignore.case = TRUE) | 
#               grepl("welcometothebrotherhood", tweet$text, ignore.case = TRUE) |
#               grepl("honor", tweet$text, ignore.case = TRUE) |
#               grepl("thank", tweet$text, ignore.case = TRUE) |
#               grepl("valuable", tweet$text, ignore.case = TRUE) |
#               grepl("Congratulations", tweet$text, ignore.case = TRUE) ){ 
      if ( sapply(posTerms, function(x) grepl(x, tweet$text, ignore.case = TRUE, fixed=FALSE)) ) {
        count_positive = count_positive + 1 # Add the positive counts
        vector_sentiments <- c(vector_sentiments, "Positive") #Add the positive sentiment
        positive_text <- c(positive_text, as.character(tweet$text)) # Add the positive text
        # Do the same for negatives   
#       } else if (  grepl("Vacuum", tweet$text, ignore.case = TRUE) | 
#                      grepl("EMPTY RHETORIC", tweet$text, ignore.case = TRUE)  | 
#                      grepl("condescending", tweet$text, ignore.case = TRUE)  | 
#                      grepl("racist", tweet$text, ignore.case = TRUE)  | 
#                      grepl("CongressStillFails", tweet$text, ignore.case = TRUE) ) { 
      } else if ( sapply(negTerms, function(x) grepl(x, tweet$text, ignore.case = TRUE, fixed=FALSE)) ) { 
        count_negative = count_negative + 1
        vector_sentiments <- c(vector_sentiments, "Negative")
        negative_text <- c(negative_text, as.character(tweet$text))
        # Do the same for neutrals
      } else { 
        count_neutral = count_neutral + 1
        print("neutral")
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(tweet$text))
      }
    }
    df_users_sentiment <- data.frame(vector_users, vector_text, vector_sentiments) 
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot <- renderPlot({
      #results = data.frame(tweets = c(paste("Positive","(",count_positive,")"), "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      results = data.frame(tweets = c(paste("Positive","(",count_positive,")"), paste("Negative","(",count_negative,")"), paste("Neutral", "(",count_neutral,")" )), numbers = c(count_positive,count_negative,count_neutral))
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
      # Display positive sentiment word cloud
      if (length(positive_text) > 0){
        output$positive_wordcloud <- renderPlot({ wordcloud(paste(positive_text, collapse=" "), min.freq = 0, random.color=TRUE, max.words=100 ,colors=brewer.pal(8, "Dark2"))  })
      }
      else {
        output$positive_wordcloud <- renderPlot({plot(1, type="n", axes=F, xlab="", ylab="")})
      }
      
      # Display negative sentiment word cloud
      if (length(negative_text) > 0) {
        output$negative_wordcloud <- renderPlot({ wordcloud(paste(negative_text, collapse=" "), random.color=TRUE,  min.freq = 0, max.words=100 ,colors=brewer.pal(8,"Set3"))  })
      }
      else {
        output$negative_wordcloud <- renderPlot({plot(1, type="n", axes=F, xlab="", ylab="")})
      }
      
      # Display neutral sentiment word cloud
      if (length(neutral_text) > 0){
        output$neutral_wordcloud <- renderPlot({ wordcloud(paste(neutral_text, collapse=" "), min.freq = 0, random.color=TRUE , max.words=100 ,colors=brewer.pal(8, "Dark2"))  })
      }
      else {
        output$neutral_wordcloud <- renderPlot({plot(1, type="n", axes=F, xlab="", ylab="")})
      }
    })
  })
})

# Reference:
# http://datascienceplus.com/how-to-create-a-twitter-sentiment-analysis-using-r-and-shiny/
# http://www.inside-r.org/packages/cran/twitter/docs/Rtweets


#
# DISTANCE CALCULATION
#
# VA2 <- as.numeric(geocode("22962 Lois Ln, Ashburn, VA 20148"))
# Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=22962%20Lois%20Ln,%20Ashburn,%20VA%2020148&sensor=false
# VA1 <- as.numeric(geocode("12601 Fairlakes Circle Fairax VA"))
# Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=12601%20Fairlakes%20Circle%20Fairax%20VA&sensor=false
# mapdist(VA1,VA2)
# by using this function you are agreeing to the terms at :
#   http://code.google.com/apis/maps/documentation/distancematrix/
#   
#   Information from URL : http://maps.googleapis.com/maps/api/geocode/json?latlng=38.8571671,-77.3824677&sensor=false
# Information from URL : http://maps.googleapis.com/maps/api/geocode/json?latlng=38.981544,-77.531771&sensor=false
# Information from URL : http://maps.googleapis.com/maps/api/distancematrix/json?origins=12601+Fair+Lakes+Cir+Fairfax+VA+22033+USA&destinations=22962+Lois+Ln+Brambleton+VA+20148+USA&mode=driving&sensor=false
# from                                       to     m     km    miles seconds  minutes     hours
# 1 12601 Fair Lakes Cir, Fairfax, VA 22033, USA 22962 Lois Ln, Brambleton, VA 20148, USA 23496 23.496 14.60041    1673 27.88333 0.4647222
#
# Refer: https://baseballwithr.wordpress.com/2013/11/11/calculating-distaces-in-r/
# Use geosphere, a package which implements spherical trigonometry, useful for calculating great distances on the globe.