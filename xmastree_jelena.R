
library(gtools)
library(ggplot2)
library(ggimage)

set.seed(24) # seed, so our tree is reproducible!

n_tree <- 7000
n_ornaments <- 50

# Sample for tree and ornaments
#   - sample n_tree/n_ornaments points from the tree spiral
#   - alpha = c(1, 1, 1) -> points are uniformly distributed
#   - map values from R^3 to R^2
dir_samples_tree <- rdirichlet(n_tree, c(1, 1, 1))
# Tree leafs 
df <- data.frame("x" = dir_samples_tree[, 2] - dir_samples_tree[, 3],
                 "y" = dir_samples_tree[, 1],
                 "size" = 1,
                 "type" = "tree")

dir_samples_ornaments <- rdirichlet(n_ornaments, c(1, 1, 1))
#ornament positons 
df <- rbind(df, data.frame("x" = dir_samples_ornaments[, 2] - dir_samples_ornaments[, 3],
                           "y" = dir_samples_ornaments[, 1], "size" = 2,
                           "type" = "ornaments"))
#xmas tree plot
xmastree<- ggplot(df, aes(x=x, y=y, color = type)) +
  geom_point(aes(size=size)) +
  #  geom_image(aes(image=image), size=.05)+
  scale_color_manual(values = c("#CF140D", "#1A8017")) + # set colors
  theme_void() + # remove axes, labels, ticks etc.
  theme(legend.position = "none") + # remove legend
  scale_size_continuous(range = c(1.5, 8)) + # set scale for points
  ylim(0, 1.1) + xlim(-1.2, 1.2) +
  annotate("text", x = 0, y = 1.1, label = "Merry Christmas!",
           color = "#CF140D", size = 7,
           family = "Palatino", fontface = "bold.italic") +
  geom_text(aes(x=0, y=1), label="★", size=15, colour="#FDBA1C",
            family = "HiraKakuPro-W3") 
#geom_image(data = d,aes(image=image), size=0.2)
ggsave(filename = "xmas.png",device = "png",plot = xmastree,width = 20,
       height = 20,units = "cm")

# font that supports the character
#view raw dirichlet_chr_tree.R hosted with ❤ by GitHub

# coccolith image random distribution frames
#first set
d1 <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample("Coccolithus_pelagicus.png",
                               size=10, replace = TRUE)
)
#second set
d2 <- data.frame(x = rnorm(10),
                 y = rnorm(10),
                 image = sample("Coccolithus_pelagicus.png",
                                size=10, replace = TRUE)
)
#Third set
d3 <- data.frame(x = rnorm(10),
                 y = rnorm(10),
                 image = sample("Coccolithus_pelagicus.png",
                                size=10, replace = TRUE)
)
#Fourth set
d4 <- data.frame(x = rnorm(10),
                 y = rnorm(10),
                 image = sample("Coccolithus_pelagicus.png",
                                size=10, replace = TRUE)
)
#plot random cocoliths
img1<- ggplot(d1, aes(x, y)) + geom_image(aes(image=image), size=0.2)+ theme_void() + theme(panel.background = element_blank())
img2<- ggplot(d2, aes(x, y)) + geom_image(aes(image=image), size=0.2)+ theme_void()+ theme(panel.background = element_blank())
img3<- ggplot(d3, aes(x, y)) + geom_image(aes(image=image), size=0.2)+ theme_void()+ theme(panel.background = element_blank())
img4<- ggplot(d4, aes(x, y)) + geom_image(aes(image=image), size=0.2)+ theme_void()+ theme(panel.background = element_blank())
#save images od coccoliths
ggsave(filename = "coco1.png",plot = img1,device = "png",width = 20,height = 20,units = "cm")
ggsave(filename = "coco2.png",plot = img2,device = "png",width = 20,height = 20,units = "cm")
ggsave(filename = "coco3.png",plot = img3,device = "png",width = 20,height = 20,units = "cm")
ggsave(filename = "coco4.png",plot = img4,device = "png",width = 20,height = 20,units = "cm")
#read images
library(magick)
#read xmas tree
xmas<- image_read("xmas.png")
#read coccoliths
coco1<- image_read("coco1.png")
coco2<- image_read("coco2.png")
coco3<- image_read("coco3.png")
coco4<- image_read("coco4.png")
#remove background
coco1<- image_transparent(coco1,color = "white")
coco2<- image_transparent(coco2,color = "white" )
coco3<- image_transparent(coco3,color = "white")
coco4<- image_transparent(coco4,color = "white")
#Make a gif with coccliths
coco_rain<- image_animate(image = c(coco1,coco2,coco3,coco4))
image_write(coco_rain, "coco.gif")
#Add background 
background <- image_background(xmas, "white", flatten = TRUE)
#read the cocco gif
cocorain<- image_read("coco.gif")
#make a composite
card<- image_composite(background,cocorain)
#Annotate with name
card_ano<- image_annotate(card, "Merry Christmas \n Dr. Jelena Godrijan", size = 100, 
                          color = "red", boxcolor = "pink",
               degrees = 0, gravity = "Center")
card_ano1<- image_annotate(card_ano, "This card is written in R language, want to reproduce ?follow the link below", size = 70, 
               gravity = "southwest", color = "green")

#creatw animation
cardxmas<- image_animate(card_ano,fps = 2)
#write to gif
image_write(cardxmas, "card.gif")

