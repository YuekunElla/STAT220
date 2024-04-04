library(magick)

# Creating a meme
# The part of cats (Your cats wakes you up when 7:00am because they're hungry)
hungry_cat <- image_read("https://www.mypetsies.com/blog/app/uploads/2016/09/edfsaf.jpg") %>%
  image_scale(300) %>%
  image_annotate("Where's breakfast?",
                 font = "Impact", 
                 size = 30, 
                 color = "white", 
                 degree = -20, 
                 location = "+10+80")

# The part of time
time_text <- image_blank(width = 300, 
                         height = 200, 
                         color = "#000000") %>%
  image_annotate("7:00 am", 
                 color = "#FFFFFF", 
                 size = 40, 
                 font = "Impact", 
                 gravity = "center")

# Making the whole meme
meme <- c(time_text, hungry_cat) %>%
  image_append(stack = TRUE)

# Save the meme as the image file
meme %>% 
  image_write("my_meme.png")


# Creating my animation
# creating basic picture for frame1 to frame4
cat_image <- image_read("https://www.mypetsies.com/blog/app/uploads/2016/09/edfsaf.jpg") %>%
  image_scale(300)

# creating each frame
time_picture <- image_blank(width = 300, 
                         height = 225, 
                         color = "#000000") %>%
  image_annotate("7:00 am", 
                 color = "#FFFFFF", 
                 size = 40, 
                 font = "Impact", 
                 gravity = "center")

frame1 <- hungry_cat
  
frame2 <- cat_image %>%
  image_flop()  %>%
  image_annotate("Wake up!!!",
                 font = "Impact", 
                 size = 30, 
                 color = "white", 
                 degree = +10, 
                 location = "+150+40")

frame3 <- cat_image %>%
  image_rotate(180) %>%
  image_annotate("MORNING!",
                 font = "Impact", 
                 size = 30, 
                 color = "white", 
                 degree = -20, 
                 location = "+150+150")

frame4 <- cat_image %>%
  image_annotate("Hi,Mom",
                 font = "Impact", 
                 size = 30, 
                 color = "white", 
                 degree = -20, 
                 location = "+40+60")

# putting the frames in order using a vector
frames <- c(time_picture, frame1, frame2, frame3, frame4)

# creating an animation
my_animation <- image_animate(frames, fps = 1) 

# save as image file
image_write(my_animation, "my_animation.gif")


















