library(RMySQL)

struct.simbol <- c("平","片","漢","A","N","B","S")
pos.simbol <- c("名","動","形","副","助","接","助動","連","感")
struct.pos.simbol <- c(struct.simbol, pos.simbol)
mysql.host <- 'localhost'
mysql.dbname <- 'book_difficulty'
mysql.user <- 'root'
mysql.password <- '********'
mysql.con <-dbConnect(MySQL(), host=mysql.host, dbname=mysql.dbname, user=mysql.user, password=mysql.password)
dbGetQuery(mysql.con, 'SET NAMES utf8')
books.df <- dbGetQuery(mysql.con, 'select filename, difficulty,title from books')

# 目次のみの解析
contents.vec <- sapply(books.df$filename,file_read,dirname="num")
loc.rate <- create.rate.mat(contents.vec)
loc.rate.df <- as.data.frame(t(loc.rate))
loc.reg <- lm(books.df$difficulty ~ ., data=loc.rate.df)
summary(loc.reg)
diff.loc.mat <- cbind(books.df$difficulty, t(loc.rate))
pairs(diff.loc.mat,panel=panel.smooth, diff.loc.mat)

# タイトルの解析
titles.vec <- books.df$title
title.rate <- create.rate.mat(titles.vec)
title.rate.df <- as.data.frame(t(title.rate))
title.reg <- lm(books.df$difficulty ~ . , data=title.rate.df)
summary(title.reg)
diff.title.mat <- cbind(books.df$difficulty, t(title.rate))
pairs(diff.title.mat,panel=panel.smooth,diff.title.mat)


mixed.vec <- add_str_vec(titles.vec, contents.vec)
mixed.rate <- create.rate.mat(mixed.vec)
mixed.rate.df <- as.data.frame(t(mixed.rate))
mixed.reg <- lm(books.df$difficulty ~ ., data=mixed.rate.df)
summary(mixed.reg)
diff.mixed.mat <- cbind(books.df$difficulty, t(mixed.rate))
pairs(diff.mixed.mat,panel=panel.smooth,diff.mixed.mat)
