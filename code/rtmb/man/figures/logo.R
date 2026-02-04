library(hexSticker)
library(cowplot)
library(magick)

# setwd("/home/darcy/Projects/CCSBT/sbt/man/figures")

fish <- image_read("sbt_crop.png")

p <- ggdraw() + draw_image(image = fish, scale = 1.4)

sticker(subplot = p, 
        package = "sbt", 
        # h_color = "orange", h_fill = "#1881C2", 
        h_color = "#1881C2", h_fill = "white", 
        p_x = 0.6, p_y = 0.75, p_size = 40, p_color = "#1881C2",
        s_x = 1.05, s_y = 1.05, s_width = 1.2, s_height = 1.2, 
        white_around_sticker = FALSE, 
        dpi = 400,
        filename = "logo.png")

# ?sticker
# sticker(subplot = p, 
#         package = "sbt", 
#         # h_color = "orange", h_fill = "#1881C2", 
#         h_color = "#1881C2", h_fill = "white", 
#         p_x = 0.6, p_y = 0.75, p_size = 11, p_color = "#1881C2",
#         s_x = 1.05, s_y = 1.05, s_width = 1.2, s_height = 1.2, 
#         white_around_sticker = FALSE, 
#         dpi = 75,
#         filename = "logo.png")
