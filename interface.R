#packages
library(shinydashboard)
library(shiny)
library(tm)
library(corrplot)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(plyr)
library(lubridate)
library(ggExtra)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(devtools)
library(igraph)
library(anytime)
library(SocialMediaMineR)
library(SocialMediaLab)
library(RColorBrewer)
library(base)
library(tidyr)
library(scales)
library(reshape2)

#import datasets
CAvideos <- read.csv("~/CAvideos.csv")
FRvideos <- read.csv("~/FRvideos.csv")
GBvideos <- read.csv("~/GBvideos.csv")
USvideos <- read.csv("~/USvideos.csv")
DEvideos <- read.csv("~/DEvideos.csv")
#merge datasets
Allvideos <- as.data.table(rbind(GBvideos,CAvideos,USvideos,DEvideos,FRvideos))

#ui
ui <- dashboardPage(skin="red",
  dashboardHeader(title = "Analysis"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  
  dashboardBody(
    tabItems(
      #tabItem home page
      tabItem("home",
              
              fluidRow(
              box(
              title = "YouTube Trending Analysis System",
              width = 8, solidHeader = TRUE,status="danger",
              br(),
              tags$img(src="youtube-logo.png",height=200,width=400,align="center"),
              p("YouTube is a popular video sharing website where content owners can share 
                videos and these videos can viewed by anyone who accesses youtube."),
              p("Viewers can react to these videos through likes,cooments and dislikes.Our proposed system
                system helps show concerned parties some of the key analysis concerning youtube videos"),
              p("These include:"),
              p(">Sentiment Analysis"),
              p(">Factors for popularity of youtube videos-why certain youtube videos are liked more than others"),
              p(">Time analysis - what is the usual time between trending date and publish times 
                per region,what times of the day are most trending videos?"),
              p(">General relationships-This shows the correlations between certain attributes of youtube videos
                e.g views and likes,views and dislikes etc"),
              p(">Regional Statistics- most liked categories,most disliked categories,top  
                liked categories,title wordclouds,tags wordclouds"),
              p(">General Statistics - most liked categories,most disliked categories,top  
                liked categories,title wordclouds,tags wordclouds")
              )
              )
      ),
      #tabItem sentiment
      tabItem("sentiment",
          h3("Choosing Category of Music as a sample study"),
          h4("visualizations"),
      tabBox(
        
        id = "tabset1", height = "500px",width="12",
        tabPanel("Liked videos",
                 "Top 5 liked videos",
                 plotOutput("")
              ),
        tabPanel("Disliked videos", "Top 5 disliked videos"),
        
        tabPanel("Liked wordcloud", "Most said words"),
        tabPanel("Disliked wordcloud", "Most said words")
      )
  )
  ,
 #tabItem factors for popularity
  tabItem("factors",
          
          br(),
          h3("Factor 1:Are videos with more consistent communicators popular?"),
            fluidRow(
              box(
                width=8,height="500px",
                title="Sample Study - SMTOWN",status="success",
                plotlyOutput("factor1"),verbatimTextOutput("event")

              ),
              box(width=4,
                  status="success",p(
                    class = "text-muted",
                    paste("From analyzing videos posted by SMTOWN across several months,we were able to deduce
                          if the channel posted  multiple videos in a short time frame,these videos had a higher
                          chance of being liked compared to when few videos were posted."
                    ))
            )
          ),
          br(),
          h3("Factor 2:Does category of a video affect it's popularity?"),
          fluidRow(
          box(width=8,
              h5("Using USA as a sampled region;"),
              p("Percentage of videos above 5k likes vs categories"),
         plotOutput("factor2")
          ),
         box(width=4,
             status="success",p(
               class = "text-muted",
               paste("Based on the data analysed,we concluded that in the USA,42% of videos in category
                     24 were above 5k likes which was significantly higher than other categories
                     e.g. category 10 where 18% of videos were above 5k likes."
               ))
               )
         
         )
  ),
 tabItem("time",
         h3("How long does it take a  video to trend from the time it was published?(days)"),
         br(),
         sidebarPanel(
           width="3",
           radioButtons("Region", "Select Region:",
                        c("Canada",
                          "USA",
                          "Great Britain",
                          "France",
                          "Germany",
                          "All")),
           br()
           
         ),
         box(
           width=9,height="500px",status="primary",
           title="Time between published and trending",
           plotOutput("plot_time1")
         )
         ,
         h3("What times are videos that take a day
            or less to trend published across all regions?"),
         fluidRow(
           box(
             width=9,height="500px",status="primary",
             plotOutput("plot_time2")
           )
         ),
         fluidRow(
           box(
             width = 6, background = "light-blue",
             p("From the first visualization potrayed above,
               we have come with the following conclusions,"
             ),
             p(">In Germany and France,it takes little time to reach the trending date,1 or 2days")
             ,p(">It is highly unlikely that videos trend the same day they were published"),
             p(">In Great Britain,most videos take between 4 - 10 days to trend"),
             p(">In USA,most videos take between 4 - 6 days to trend"),
             p(">Across all regions,videos usually take one day to trend")),
           box(
             width=6,background="light-blue",
             p("From the second visualization,we see that:"),
             p("Videos published in the late afternoon and
               early evening hours(16:00hrs -18:00hrs) have a higher chance of trending in a day or less.")
           )
           )
 ),
 #tabItem general relationships
 tabItem("relation",
         
         br(),
         
         
           fluidRow(
             box(width=6,title = "Views vs likes",
                 status="success",plotOutput("plot_views_likes")),
             box(width=6,title = "Views vs Dislikes",
                 status="danger",plotOutput("plot_viewsdislikes"))
           ),br(),
         fluidRow(
           box(width=6,title = "Views vs Comments",
               status="primary",plotOutput("plot_views_comments")),
           box(width=6,title = "Correlation between views
               ,likes,dislikes and comment count",
               status="primary",plotOutput("plot_correlation"))
           ),
         br(),
         fluidRow
         (
           box(
             width=6,"From the graphs above,we see views and likes have a high correlation
             ,unlike views and comment count and views and dislikes that have low correlations"
           )
         )
 ),
 #tab item regional statistics
 tabItem("regional",
         
         
           selectInput("Region2", "Choose a Region:",
                       list('Canada',
                            'France',
                            'Germany',
                            'Great Britain',
                            'USA'
                            )
           ),
           selectInput("visual", "Choose Visualization you would like:",
                       list(
                            'Title wordcloud',
                            'Tags wordcloud',
                            'Most liked categories',
                            'Most disliked categories',
                            'Most commented on categories',
                            'Most liked per category'
                       )
           ),
         br(),
         box(
           width=8,plotOutput("plotregional")
         )
           
        
         
         
  ),
 tabItem("general",
         selectInput("visual2", "Choose Visualization you would like:",
                     list(
                       'Title wordcloud',
                       'Tags wordcloud',
                       'Most liked categories',
                       'Most disliked categories',
                       'Most commented on categories',
                       'Most liked per category'
                     )
         ),
         br(),
         box(
           width=8,plotOutput("plotgeneral")
         )
         ))
))

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("Sentiment Analysis",tabName="sentiment"),
      menuItem("Factors for popularity",tabName="factors"),
      menuItem("Time analysis",tabName="time"),
      menuItem(" General Relationships",tabName="relation"),
      menuItem(" Regional Statistics",tabName="regional"),
      menuItem(" General Statistics",tabName="general")
    )
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$factor1 <- renderPlotly({
    average_likes<-trunc(mean(GBvideos$likes))
    average_likes
    
    nrow(GBvideos[GBvideos$channel_title == "SMTOWN",])
    
    #create dataframe from GBvideos where channel title = Soccer AM

    final<-GBvideos[GBvideos$channel_title == "SMTOWN",]
    
    #add column to final dataframe
    final$averagelikes = average_likes
    
    final$publish_time <- as.Date(final$publish_time)
    
    p <- plot_ly(final, x = ~publish_time, y = ~likes, type = 'scatter', mode = 'lines')
    p
  })
  output$factor2 <- renderPlot({
    plotj <- ggplot(USvideos, aes(as.factor(category_id))) + 
      geom_bar(aes(y =((likes > 5000)/likes), fill = factor(category_id)), stat="identity") + 
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage of videos above 5k likes")+ xlab("Categories") 
    
    plotj
  })
  output$plot_views_likes <- renderPlot({
       
    ggplot(Allvideos, aes(views, likes, colour = factor(category_id)))+ geom_point()+
      xlab("Views")+ ylab("Likes")+
      ggtitle("Relation between views and likes for trending\n videos ")+
      geom_smooth(method=lm,color="darkred")
  })
  
  output$plot_viewsdislikes <- renderPlot({
    
      ggplot(Allvideos, aes(views, dislikes,colour = factor(category_id)))+ geom_point() +
        xlab("Views per region")+ ylab("Dislikes per region")+
        ggtitle("Relation between views and dislikes")+
      geom_smooth(method=lm,color="darkred")
  })
  
  output$plot_views_comments <- renderPlot({
    
      ggplot(Allvideos, aes(views, comment_count,colour = factor(category_id)))+ geom_point() +
        xlab("Views per region")+ ylab("Comment count per region")+
        ggtitle("Relation between views and comment count")+
      geom_smooth(method=lm,color="darkred")
  
  })
  output$plot_correlation <- renderPlot({
    
    corrplot.mixed(corr = cor(Allvideos[,c("views","likes","dislikes","comment_count"),with=F]))
    
    
  })
  output$plot_time1  <- renderPlot({
    if(input$Region=="Canada")
    {
    CAvideos <- as.data.table(CAvideos)
    CAvideos$trending_date <-ydm(CAvideos$trending_date)
    CAvideos$publish_time <- as.Date(CAvideos$publish_time)

    CAvideos$difference = CAvideos$trending_date - CAvideos$publish_time
    
    ggplot(CAvideos[difference<10],
           aes(as.factor(difference),fill=as.factor(difference)))+
      geom_bar()+guides(fill="none")+
      labs(title=" Time between published and trending in days"
      )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="USA")
    {
      USvideos <- as.data.table(USvideos)
      USvideos$trending_date <-ydm(USvideos$trending_date)
      USvideos$publish_time <- as.Date(USvideos$publish_time)
      
      USvideos$difference = USvideos$trending_date - USvideos$publish_time
      
      ggplot(USvideos[difference<25],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    else if(input$Region=="Great Britain")
    {
      GBvideos <- as.data.table(GBvideos)
      GBvideos$trending_date <-ydm(GBvideos$trending_date)
      GBvideos$publish_time <- as.Date(GBvideos$publish_time)
      
      GBvideos$difference = GBvideos$trending_date - GBvideos$publish_time
      
      ggplot(GBvideos[difference<25],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="France")
    {
      FRvideos <- as.data.table(FRvideos)
      FRvideos$trending_date <-ydm(FRvideos$trending_date)
      FRvideos$publish_time <- as.Date(FRvideos$publish_time)
      
     FRvideos$difference = FRvideos$trending_date - FRvideos$publish_time
      
      ggplot(FRvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos")
    }
    else if(input$Region=="Germany")
    {
      DEvideos <- as.data.table(DEvideos)
      DEvideos$trending_date <-ydm(DEvideos$trending_date)
      DEvideos$publish_time <- as.Date(DEvideos$publish_time)
      
      DEvideos$difference = DEvideos$trending_date - DEvideos$publish_time
      
      ggplot(DEvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    else
    {
      Allvideos <- as.data.table(Allvideos)
      Allvideos$trending_date <-ydm(Allvideos$trending_date)
      Allvideos$publish_time <- as.Date(Allvideos$publish_time)
      
      Allvideos$difference = Allvideos$trending_date - Allvideos$publish_time
      
      ggplot(Allvideos[difference<10],
             aes(as.factor(difference),fill=as.factor(difference)))+
        geom_bar()+guides(fill="none")+
        labs(title=" Time between published and trending in days"
        )+xlab("Number of days")+ylab("Number of videos") 
    }
    
  }
  
  )
  output$plot_time2<-renderPlot(
    {
      Allvideos %>% select(category_id,likes,trending_date,publish_time)%>%
        mutate(trending_date = ydm(Allvideos$trending_date))%>%
        mutate(category_id = as.factor(Allvideos$category_id))%>%
        mutate(publish_time = anytime(as.factor(Allvideos$publish_time)))%>%
        mutate(difference=difftime(trending_date,publish_time,units=c("days")))%>%
        filter(difference < 2)%>%
        mutate(difference = as.integer(difference))%>%
        mutate(difference = as.factor(difference))%>%
        mutate(publish_time= hour(publish_time))%>%
        ggplot(aes(publish_time,fill=as.factor(publish_time)))+geom_bar()+
        guides(fill="none")+
        labs(title="Publish times of videos that have trending times of a day or less"
        )+xlab("Hours(24 hour clock)")+ylab("Number of videos")
      
      
        
    })
  output$plotregional<- renderPlot(
    {
     if(input$Region2=="Canada")
     {
       if(input$visual=="Title wordcloud"){
         
         CAvideos$title<-gsub('[^a-zA-Z0-9.]'," ", CAvideos$title)
         
         Encoding(CAvideos$title) <- "latin1"
         trial1 <- Corpus(VectorSource(iconv(CAvideos$title, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial1 <- tm_map(trial1, toSpace, "/")
         trial1 <- tm_map(trial1, toSpace, "@")
         trial1 <- tm_map(trial1, toSpace, "\\|")
         trial1 <- tm_map(trial1, removePunctuation)
         trial1 = tm_map(trial1, content_transformer(tolower))
         trial1 <- tm_map(trial1, removeNumbers)
         trial1 <- tm_map(trial1, stripWhitespace)
         trial1 <- tm_map(trial1, removeWords, stopwords('english'))
         trial1 <- tm_map(trial1, stemDocument)
         
         wordcloud(trial1, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
         
       }
      else if(input$visual=="Tags wordcloud")
      {
        
        CAvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", CAvideos$tags)
        
        Encoding(CAvideos$tags) <- "latin1"
    trial <- Corpus(VectorSource(iconv(CAvideos$tags, "latin1", "ASCII", sub="")))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        trial <- tm_map(trial, toSpace, "/")
        trial <- tm_map(trial, toSpace, "@")
        trial <- tm_map(trial, toSpace, "\\|")
        trial <- tm_map(trial, removePunctuation)
        trial = tm_map(trial, content_transformer(tolower))
        trial <- tm_map(trial, removeNumbers)
        trial <- tm_map(trial, stripWhitespace)
        trial <- tm_map(trial, removeWords, stopwords('english'))
        trial <- tm_map(trial, stemDocument)
        
        wordcloud(trial, scale=c(5,0.5),
                  max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
      }
       
     }
     else if(input$Region2=="France") 
     {
       if(input$visual=="Title wordcloud"){
         
         FRvideos$title<-gsub('[^a-zA-Z0-9.]'," ", FRvideos$title)
         
         Encoding(FRvideos$title) <- "latin1"
         trial <- Corpus(VectorSource(iconv(FRvideos$tags, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial <- tm_map(trial, toSpace, "/")
         trial <- tm_map(trial, toSpace, "@")
         trial <- tm_map(trial, toSpace, "\\|")
         trial <- tm_map(trial, removePunctuation)
         trial = tm_map(trial, content_transformer(tolower))
         trial <- tm_map(trial, removeNumbers)
         trial <- tm_map(trial, stripWhitespace)
         trial <- tm_map(trial, removeWords, stopwords('english'))
         trial <- tm_map(trial, removeWords, stopwords('french'))
         trial <- tm_map(trial, stemDocument)
         
         wordcloud(trial, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
         
       }
       else if(input$visual=="Tags wordcloud")
       {
         
         FRvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", FRvideos$tags)
         
         Encoding(FRvideos$tags) <- "latin1"
         tria1l <- Corpus(VectorSource(iconv(FRvideos$tags, "latin1", "ASCII", sub="")))
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         trial <- tm_map(trial1, toSpace, "/")
         trial1 <- tm_map(trial1, toSpace, "@")
         trial1 <- tm_map(trial1, toSpace, "\\|")
         trial1 <- tm_map(trial1, removePunctuation)
         trial1 = tm_map(trial1, content_transformer(tolower))
         trial1 <- tm_map(trial1, removeNumbers)
         trial1 <- tm_map(trial1, stripWhitespace)
         trial1 <- tm_map(trial1, removeWords, stopwords('english'))
         trial1 <- tm_map(trial1, removeWords, stopwords('french'))
         trial1 <- tm_map(trial1, stemDocument)
         
         wordcloud(trial1, scale=c(5,0.5),
                   max.words=100, random.order=FALSE,
                   rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
       } 
     }
      else if(input$Region2=="Germany")
      {
        if(input$visual=="Title wordcloud")
        {
          DEvideos$title<-gsub('[^a-zA-Z0-9.]'," ", DEvideos$title)
          
          Encoding(DEvideos$title) <- "latin1"
          
          trial1 <- Corpus(VectorSource(iconv(DEvideos$title, "latin1","ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, toSpace, "blm")
          trial1 <- tm_map(trial1, removePunctuation, preserve_intra_word_dashes=TRUE)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('german'))
          trial1<- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, toSpace, "blm")
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))  
        }
        else if(input$visual=="Tags wordcloud"){
          DEvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", DEvideos$tags)
          
        Encoding(DEvideos$tags) <- "latin1"
      
        trial <- Corpus(VectorSource(iconv(DEvideos$tags, "latin1","ASCII", sub="")))
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        trial <- tm_map(trial, toSpace, "/")
        trial <- tm_map(trial, toSpace, "@")
        trial <- tm_map(trial, toSpace, "\\|")
        trial <- tm_map(trial, toSpace, "blm")
        trial <- tm_map(trial, removePunctuation, preserve_intra_word_dashes=TRUE)
        trial = tm_map(trial, content_transformer(tolower))
        trial <- tm_map(trial, removeNumbers)
        trial <- tm_map(trial, stripWhitespace)
        trial <- tm_map(trial, removeWords, stopwords('german'))
        trial <- tm_map(trial, removeWords, stopwords('english'))
        trial <- tm_map(trial, toSpace, "blm")
        trial <- tm_map(trial, stemDocument)
        
        wordcloud(trial, scale=c(5,0.5),
                  max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        }
       
        
      }
      else if(input$Region2=="Great Britain")
      {
        if(input$visual=="Title wordcloud"){
          
          GBvideos$title<-gsub('[^a-zA-Z0-9.]'," ", GBvideos$title)
          
          Encoding(GBvideos$title) <- "latin1"
          trial1 <- Corpus(VectorSource(iconv(GBvideos$title, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, removePunctuation)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          
        }
        else if(input$visual=="Tags wordcloud")
        {
          
          GBvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", GBvideos$tags)
          
          Encoding(GBvideos$tags) <- "latin1"
          trial <- Corpus(VectorSource(iconv(GBvideos$tags, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial <- tm_map(trial, toSpace, "/")
          trial <- tm_map(trial, toSpace, "@")
          trial <- tm_map(trial, toSpace, "\\|")
          trial <- tm_map(trial, removePunctuation)
          trial = tm_map(trial, content_transformer(tolower))
          trial <- tm_map(trial, removeNumbers)
          trial <- tm_map(trial, stripWhitespace)
          trial <- tm_map(trial, removeWords, stopwords('english'))
          trial <- tm_map(trial, stemDocument)
          
          wordcloud(trial, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        } 
      }
      else
      {
        if(input$visual=="Title wordcloud"){
          
          USvideos$title<-gsub('[^a-zA-Z0-9.]'," ", USvideos$title)
          
          Encoding(USvideos$title) <- "latin1"
          trial1 <- Corpus(VectorSource(iconv(USvideos$title, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial1 <- tm_map(trial1, toSpace, "/")
          trial1 <- tm_map(trial1, toSpace, "@")
          trial1 <- tm_map(trial1, toSpace, "\\|")
          trial1 <- tm_map(trial1, removePunctuation)
          trial1 = tm_map(trial1, content_transformer(tolower))
          trial1 <- tm_map(trial1, removeNumbers)
          trial1 <- tm_map(trial1, stripWhitespace)
          trial1 <- tm_map(trial1, removeWords, stopwords('english'))
          trial1 <- tm_map(trial1, stemDocument)
          
          wordcloud(trial1, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          
        }
        else if(input$visual=="Tags wordcloud")
        {
          
          USvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", USvideos$tags)
          
          Encoding(USvideos$tags) <- "latin1"
          trial <- Corpus(VectorSource(iconv(USvideos$tags, "latin1", "ASCII", sub="")))
          toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
          trial <- tm_map(trial, toSpace, "/")
          trial <- tm_map(trial, toSpace, "@")
          trial <- tm_map(trial, toSpace, "\\|")
          trial <- tm_map(trial, removePunctuation)
          trial = tm_map(trial, content_transformer(tolower))
          trial <- tm_map(trial, removeNumbers)
          trial <- tm_map(trial, stripWhitespace)
          trial <- tm_map(trial, removeWords, stopwords('english'))
          trial <- tm_map(trial, stemDocument)
          
          wordcloud(trial, scale=c(5,0.5),
                    max.words=100, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        } 
      }
    })
  output$plotgeneral <- renderPlot({
    
    if(input$visual2=="Title wordcloud")
    {
      
      Allvideos$title<-gsub('[^a-zA-Z0-9.]'," ", Allvideos$title)
      
      Encoding(Allvideos$title) <- "latin1"
      trial_all <- Corpus(VectorSource(iconv(Allvideos$title, "latin1", "ASCII", sub="")))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      trial_all <- tm_map(trial_all, toSpace, "/")
      trial_all <- tm_map(trial_all, toSpace, "@")
      trial_all <- tm_map(trial_all, toSpace, "\\|")
      trial_all <- tm_map(trial_all, removePunctuation)
      trial_all = tm_map(trial_all, content_transformer(tolower))
      trial_all <- tm_map(trial_all, removeNumbers)
      trial_all <- tm_map(trial_all, stripWhitespace)
      trial_all <- tm_map(trial_all, removeWords, stopwords('german'))
      trial_all <- tm_map(trial_all, removeWords, stopwords('french'))
      trial_all <- tm_map(trial_all, removeWords, stopwords('english'))
      trial_all <- tm_map(trial_all, stemDocument)
      
      wordcloud(trial_all, scale=c(5,0.5),
                max.words=100, random.order=FALSE,
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
    } 
    else if(input$visual2=="Tags wordcloud")
    {
      
      Allvideos$tags<-gsub('[^a-zA-Z0-9.]'," ", Allvideos$tags)
      
      Encoding(Allvideos$tags) <- "latin1"
      trial_all1 <- Corpus(VectorSource(iconv(Allvideos$tags, "latin1", "ASCII", sub="")))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      trial_all1 <- tm_map(trial_all1, toSpace, "/")
      trial_all1<- tm_map(trial_all1, toSpace, "@")
      trial_all1 <- tm_map(trial_all1, toSpace, "\\|")
      trial_all1 <- tm_map(trial_all1, removePunctuation)
      trial_all1 = tm_map(trial_all1, content_transformer(tolower))
      trial_all1 <- tm_map(trial_all1, removeNumbers)
      trial_all1 <- tm_map(trial_all1, stripWhitespace)
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('german'))
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('french'))
      trial_all1 <- tm_map(trial_all1, removeWords, stopwords('english'))
      trial_all1 <- tm_map(trial_all1, stemDocument)
      
      wordcloud(trial_all1, scale=c(5,0.5),
                max.words=100, random.order=FALSE,
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
    } 
  })
}

shinyApp(ui, server)
