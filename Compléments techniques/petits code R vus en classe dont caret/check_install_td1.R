p <- c(require(ggplot2),require(dplyr),require(caret))
t <- Sys.time()
n <- sum(p)
print(t)
cat("nombre de package installés parmi ggplot2 dplyr caret : ",
    n)

