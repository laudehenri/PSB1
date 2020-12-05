# Le minimum à savoir en Maths

# une courbe
curve(sin,
      from = -pi,
      to= pi,
      xname = "angle")

# une fonction et son intégrale (une droite)
g <- function(x) (2*x+1)
curve(g, 0,100)
pracma::integral(g,0,100)

# une autre
f <- function(x) (x^2+1)
curve(f, 0,100)
pracma::integral(f,0,100)

# teste de la fonction sur une valeur
f(3)

# application de la fonction sur un vector
v <-c(2,4,8)
w <- purrr::map(v,f)
w <- unlist(w)

# la même chose sur iris
str(iris)
a <- iris[,1]
w <- purrr::map(a,f)
w <- unlist(w) # on préfère les vectors au listes

ma_nouvelle_colonne      <- w
mon_nouveau_fichier_iris <- data.frame(iris,ma_nouvelle_colonne)

# une fonction de 2 variables
h <- function(z,t)(t+z)
h(1,10)



  