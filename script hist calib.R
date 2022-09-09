#install.packages("Sim.DiffProc")

library("Sim.DiffProc")
library(tidyverse)
library(readr)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(gifski)
library(lubridate)
library(reshape2)
library(tibble)


#1.0 Exploracion de funcion y primeras pruebas ----

## Hull-White/Vasicek Models
## dX(t) = 4 * (2.5 - X(t)) * dt + 1 *dW(t), X0=10
set.seed(1234)

ncurvas <-  50 

cl <- rainbow(ncurvas)
X <- HWV(N=100,M=50,mu = 4, theta = 2.5,sigma = 0.06,x0=1)

plot(X,plot.type="single", col= cl)
lines(as.vector(time(X)),rowMeans(X),col="red")




# meltdf <- melt(lista$vol_9,id= NULL)
# 
# ggplot(meltdf,aes(x= Var1,y=value,colour=Var2,group=Var2)) + geom_line() + theme_bw() + theme(legend.position = "none") + 
#   labs(title ="Simulating HW 1F parameters", 
#        subtitle = 'Volatility: {frame_along}', caption = "Model: dX(t) = 4 * (2.5 - X(t)) * dt + sigma*dW(t), X0=10 \nInternal Presentation, MChile",
#        x = "Steps")



#2.0 Generacion objetos en lista ----


vector = c(seq(0,0.005*9,0.005))

listavol <- list() 

for (i in 1:10) {


  listavol[[i]] <- HWV(N=100,M=50,mu = 4, theta = 2.5,sigma = 0.03+vector[i] ,x0=1)


}

names(listavol) = paste0("Vol_",seq(1,10,1))



# listamu <- list()
# 
# for (i in 1:10) {
#   
#   
#   listamu[[i]] <- HWV(N=100,M=50,mu = 0.1+vector[i], theta = 0.02 ,sigma = 0.03 ,x0=1)
#   
#   
# }
# 
# names(listamu) = paste0("mu_",seq(1,10,1))




#alternativa, trabajar con libreria tibble
#lista <- lst(mtcars,iris, "asd") 



#3.0 Funciones para graficar y guardar imgs (gifs) ----

# i = 6
# 
# meltdf <- melt(listavol[i],id= NULL)
# 
# ggplot(meltdf,aes(x= Var1,y=value,colour=Var2,group=Var2)) + geom_line() + theme_bw() + theme(legend.position = "none") + 
#   labs(title ="Simulating HW 1F parameters", 
#        subtitle = paste0('Volatility= ',  0.03+vector[i]), caption = "Model: dX(t) = 4 * (2.5 - X(t)) * dt + sigma*dW(t), X0=10 \nInternal Presentation, MChile",
#        x = "Steps") +theme_ft_rc() +
#   theme(axis.text.x = element_text(hjust=1,size=12,
#                                    color="white"),
#         axis.text.y = element_text(size=12,
#                                    color="white",
#                                    face="bold"),
#         plot.subtitle=element_text(size=12,
#                                    color="white",
#                                    face="bold"),
#         legend.position="none")


for (i in 1:length(listavol)) {
  
  
  meltdf <- melt(listavol[i],id= NULL)
  
ggplot(meltdf,aes(x= Var1,y=value,colour=Var2,group=Var2)) + geom_line()  + 
  labs(title ="Simulating random process", 
       subtitle = paste0('Volatility= ',  0.03+vector[i]), caption = "Model: dX(t) = 4 * (2.5 - X(t)) * dt + sigma*dW(t), X0=10 \nrealizado para pablotv.medium.com",
       x = "Steps") + theme_ft_rc() + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1,size=12,
                                   color="white"),
        axis.text.y = element_text(size=12,
                                   color="white",
                                   face="bold"))


ggsave(filename = paste0('Volatility = ', 0.03+vector[i], ".png"), width = 13.33, height = 7.5, units = "in", dpi =320)

}

#install.packages("magick")
library(magick)

imgs_vol <- list.files(path = "./", full.names = TRUE, pattern = "Vol")
img_list <- lapply(imgs_vol, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 1 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
#img_animated

## save to disk
image_write(image = img_animated,
            path = "gif_vol.gif")

  
dir.create("mu", showWarnings = FALSE)


# for (i in 1:length(listamu)) {
#   
#   
#   meltdf <- melt(listamu[i],id= NULL)
#   
#   ggplot(meltdf,aes(x= Var1,y=value,colour=Var2,group=Var2)) + geom_line() + theme_bw() + theme(legend.position = "none") + 
#     labs(title ="Simulating HW 1F parameters", 
#          subtitle = paste0('mu (Mean Rev. Speed) =', 0.1+vector[i]), caption = "Model: dX(t) = mu * (0.02 - X(t)) * dt + 0.05*dW(t), X0=10\nInternal Presentation, MChile",
#          x = "Steps")
#   
#   
#   ggsave(filename = paste0('mu/mu = ', 0.1+vector[i], ".png"), width = 13.33, height = 7.5, units = "in", dpi =320)
#   
# }
# 
# 
# imgs_mu <- list.files(path = "mu/", full.names = TRUE, pattern = "*.png")
# img_list <- lapply(imgs_mu, image_read)
# 
# ## join the images together
# img_joined <- image_join(img_list)
# 
# ## animate at 1 frames per second
# img_animated <- image_animate(img_joined, fps = 1)
# 
# ## view animated image
# #img_animated
# 
# ## save to disk
# image_write(image = img_animated,
#             path = "mu/gif_mu.gif")
