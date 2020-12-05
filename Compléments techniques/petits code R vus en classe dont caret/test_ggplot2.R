# test utilisation ggplot2, recopié dans l'aide
str(mpg)   # contenu du fichier utilisé
help(mpg)  # aide du fichier utilisé

library(ggplot2)
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()


