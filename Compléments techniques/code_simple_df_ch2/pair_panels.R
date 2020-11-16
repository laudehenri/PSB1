## panel pour la fonction pairs - pas present dans l'ouvrage
## pratique courante, code non ecrit par l'auteur (hors commentaires)
## vous trouverez de nombreux parametrages de panels sur le net
## en cherchant sur un moteur l'expression : "pairs panels"
## choisissez celui qui vous convient le mieux !

## par exemple ici : https://stat.ethz.ch/pipermail/r-help/2011-February/268962.html
## ou : dans l'ouvrage - R Graphics Cookbook de Winston Chang

## usage 1 (lignes courbes pour approximer les nuages de points)
#  pairs(Z,diag.panel  = panel.hist,
#          upper.panel = panel.cor,
#          lower.panel = panel.smooth)

## usage 2 (lignes droites pour approximer les nuages de points)
#  pairs(Z,diag.panel = panel.hist,
#        upper.panel  = panel.cor,
#        lower.panel  = panel.lm)


panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "magenta", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r, col = "blue")
}

## avec regression lineraire
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.smooth = "red", ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)    
  abline(stats::lm(y ~ x),col = col.smooth, ...) 
}

