
library(hexSticker)



imgurl <- "inst/figures/logo.png"
library(showtext)
font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()

hexSticker::sticker(imgurl, 
                    package="hydrofab",
                    p_family = "gochi",
                    p_size=26, 
                    p_color = "gold",
                    s_x=1, 
                    s_y=.8, 
                    s_width=.4,
                    spotlight = TRUE,
                    u_color = 'black',
                    u_size = 6,
                    u_family = "gochi",
                    l_x = 0.95, l_y = 1.75, l_alpha = 0.3,
                    h_color = "#990303",
                    h_fill = "#9c9999",
                    white_around_sticker = FALSE,
        filename="inst/figures/imgfile.png")

# hexSticker::sticker(imgurl, 
#                     package="hydrofab",
#                     p_family = "gochi",
#                     p_size=26, 
#                     p_color = "#000000",
#                     s_x=1, 
#                     s_y=.8, 
#                     s_width=.4,
#                     spotlight = TRUE,
#                     u_color = '#009A4E',
#                     u_size = 6,
#                     u_family = "gochi",
#                     l_x = 0.95, l_y = 1.75, l_alpha = 0.3,
#                     h_color = "#ED1C24",
#                     h_fill = "#FFF200",
#                     white_around_sticker = TRUE,
#                     filename="inst/figures/imgfile2.png")


