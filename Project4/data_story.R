library(magick)
library(tidyverse)
my_colors <- c("#E59BE9","#FFCDEA", "#FB9AD1" ,"#86469C","#EEF7FF")
text_square <- image_blank(width = 600, height = 400, color = my_colors[5])
image_box <- image_blank(width = 600, height = 400, color = my_colors[5])

#Frame1-a title slide
text_1 <- "An exploration of the data of the two Youtube channels, @aliazaita and @LaniPliopa, 
 both are as life vloggers and have similar subscribers counts. We will focus on their difference on
 view counts, published times, video durations, and content topics of the videos." %>% str_wrap(50)
text_part_1 <- text_square %>%
  image_annotate(text = text_1,
                 size = 20,
                 gravity = "center" ) 

aliazaita_image_1 <- image_read(path ="aliazaita_1.png")%>%
  image_scale(300)
laniPliopa_image_1 <- image_read(path ="LaniPliopa_1.png")%>%
  image_scale(300)
aliazaita_image_2 <- image_read(path ="aliazaita_2.png")%>%
  image_scale(300)
laniPliopa_image_2 <- image_read(path ="LaniPliopa_2.png")%>%
  image_scale(300)

row1 = c(aliazaita_image_1,laniPliopa_image_1) %>% image_append()
row2 = c(aliazaita_image_2,laniPliopa_image_2) %>% image_append()

pictures_1 <- c(row1, row2)%>%
  image_append(stack = TRUE)%>%
  image_scale(600)

image_part_1 <- image_composite(image_box, pictures_1, offset = "+10+28")

frame1 <- c(image_part_1,text_part_1) %>%
  image_append()
frame1 

#slide 2
text_2 <- "Exploring the titles of each channel revealed some overlaps in 
the top 10 most frequent words. Some words appeared frequently in both two channels, 
such as week, vlog, life.This shared vocabulary implied there were some similar contents or themes
in the two channels. The other words can implied some other contents or elements appeared frequently." %>% 
  str_wrap(48)

text_part_2 <- text_square %>%
  image_annotate(text = text_2,
                 size = 20,
                 gravity = "center" ) 

plot_1 = image_read("plot1.png") %>%
  image_scale(500)
image_part_2 <- image_composite(image_box,plot_1 , offset = "+60+28")

frame2 <- c(image_part_2,text_part_2) %>%
  image_append()
frame2

#slide 3
text_3 <- "After exploring the relationship between published time in a day and views count,
we found the amount mean views are high when published time was from 1pm to 4pm for the two channels. 
However, @aliazaita also can get very high views when published time was around 8pm. In addition, most of
videos in two channels was published from 12am to 10pm." %>% str_wrap(50)
text_part_3 <- text_square %>%
  image_annotate(text = text_3,
                 size = 20,
                 gravity = "center" ) 

plot_2 = image_read("plot2.png") %>%
  image_scale(500)

image_part_3 <- image_composite(image_box,plot_2 , offset = "+80+28")

frame3 <- c(image_part_3,text_part_3) %>%
  image_append()
frame3

#silde 4
text_4 <- "The duration time of most of videos form @aliazaita is below 1 min or around 15 mins.
And the duration time of most of videos form @LaniPliopa is around 10 mins." %>% str_wrap(50)
text_part_4 <- text_square %>%
  image_annotate(text = text_4,
                 size = 20,
                 gravity = "center" ) 

plot_4 = image_read("plot4.png") %>%
  image_scale(500)

image_part_4 <- image_composite(image_box, plot_4, offset = "+80+28")

frame4 <- c(image_part_4,text_part_4) %>%
  image_append()
frame4

#slide 5
text_5 <- "There are more high views video from @aliazaita channel than @LaniPliopa.The
median of durations of videos which got high or very high views is around 16 minutes in the
@aliazaita channel.The median of durations of videos which got high or very high views is around 
10 minutes in the @LaniPliopa channel." %>% str_wrap(50)
text_part_5 <- text_square %>%
  image_annotate(text = text_5,
                 size = 20,
                 gravity = "center" ) 

plot_3 = image_read("plot3.png") %>%
  image_scale(480)

image_part_5 <- image_composite(image_box, plot_3, offset = "+80+28")

frame5 <- c(image_part_5,text_part_5) %>%
  image_append()
frame5

#final slides
text_6 <-"Overall, I learned that the some differences and similarities between 
the YouTube channels of @aliazaita and @LaniPliopa.There are some overlaps in the vocabularies used in titles, 
indicating shared content themes such as life, vlog, and week. Both channels experience peak views 
during certain hours of the day, with @aliazaita demonstrating the ability to attract high views even during non-peak hours.
The analysis reveals a disparity in high views videos between the two channels, with @aliazaita consistently 
outperforming @LaniPliopa in this regard. @Aliazaita tends to produce longer videos on average compared to @LaniPliopa,
with median durations of 16 minutes and 10 minutes, respectively, for high views videos."%>% str_wrap(50)
text_part_6 <- text_square %>%
  image_annotate(text = text_6,
                 size = 17,
                 gravity = "center" ) 

img_6 = image_read("img_6.png") %>%
  image_scale(400)

image_part_6 <- image_composite(image_box, img_6, offset = "+150+8")

frame6 <- c(image_part_6,text_part_6) %>%
  image_append()
frame6

#creating an animation
frames <- c(rep(frame1, 8), rep(frame2, 8),rep(frame3, 8),rep(frame4, 8),rep(frame5, 8),rep(frame6, 8))
data_story <-image_animate(frames, fps = 1)

image_write(data_story,"data_story.gif")





















