library(plot_ly)
library(ggplot2)
library(maptools)
library(gridExtra)
library(ggthemes)
#structure of the dataset
str(USvideos)
gpclibPermit()

#views:likes per region
ggplot(USvideos, aes(views, likes, colour = category_id))+ geom_point()+
  xlab("Views per region")+ ylab("Likes per region")+
  ggtitle("Relation between views and likes for US trending\n videos ")

#views:dislikes per region
ggplot(USvideos, aes(views, dislikes, colour = category_id))+ geom_point() +
  xlab("Views per region")+ ylab("Dislikes per region")+
  ggtitle("Relation between views and dislikes for\n US trending videos ")

#views:comment count
ggplot(USvideos, aes(views, comment_count, colour = category_id))+ geom_point() +
  xlab("Views per region")+ ylab("Comment count per region")+
  ggtitle("Relation between views and comment count\n for US trending videos ")

