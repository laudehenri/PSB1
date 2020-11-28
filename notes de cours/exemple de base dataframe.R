data()
df <- data.frame(longley)
colonne_GNP <- df$GNP
colonne_GNP
is.vector(colonne_GNP)
colonne_2 <- 2*colonne_GNP
df_bis <- data.frame(df, colonne_2)View(df_bis)
View(df_bis)
df[ 1, ]
df[ , 1]
df[2,3]
df[1:2, ]
df[ , 3:4]
df[1:2,3:4]
sum(df$Unemployed)  # somme
mean(df$Unemployed) # moyenne
sum(df[1:2,3:4])
d <- readxl::read_xlsx("lllkjlkjmlkj.xlsx")
df <- data.frame(d)
