install.packages("dabr")
library(dabr)
# connexion au moteur mariaDB sur la base chess
conn <- open_conn_mysql("bd1",
                        user = "root",
                        password = "",
                        host = "localhost",
                        port = 3306)
# interrogation de la table déjà crée dans mariaDB
querydep <- paste("SELECT * FROM dept")
outdep   <- select(conn,query = querydep)
str(outdep)

querysalarie <- paste("SELECT * FROM salarie")
outsalarie   <- select(conn,query = querysalarie)
str(outsalarie)

library(dplyr)
doubi <- left_join(outsalarie, outdep)
