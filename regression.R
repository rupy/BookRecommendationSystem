library(RMySQL)

struct.simbol <- c("平","片","漢","A","N","B","S")
pos.simbol <- c("名","動","形","副","助","接","助動","連","感")
struct.pos.simbol <- c(struct.simbol, pos.simbol)

# 推定された難易度をデータベースから取得する
get.estimated.book.difficulty <- function(){
  mysql.host <- 'localhost'
  mysql.dbname <- 'bookDifficulty_development'
  mysql.user <- 'root'
  mysql.password <- '27krkrps'
  mysql.con <-dbConnect(MySQL(), host=mysql.host, dbname=mysql.dbname, user=mysql.user, password=mysql.password)
  dbGetQuery(mysql.con, 'SET NAMES utf8')
  books.df <- dbGetQuery(mysql.con, 'select filename, difficulty,title from books')  
  books.df
}


# 2つの文字列ベクトルをそれぞれの要素に対して結合
add_str_vec <- function(vec1, vec2){
  if(length(vec1) == length(vec2)){
    res <- c()
    for( i in seq(length(vec1))){
      res <- c(res,paste(vec1[i], vec2[i], collapse="",sep=""))
    }
  }else{
    print("長さが揃っていません")
  }
  res
}

# 線形回帰分析
linear.regression <- function(contents.vec, difficulty.vec){
  loc.rate <- create.rate.mat(contents.vec)
  loc.rate.df <- as.data.frame(t(loc.rate))
  loc.reg <- lm(difficulty.vec ~ ., data=loc.rate.df)
  summary(loc.reg)
  diff.loc.mat <- cbind(difficulty.vec, t(loc.rate))
  pairs(diff.loc.mat, panel=panel.smooth, diff.loc.mat)  
}

books.df <- get.estimated.book.difficulty()
# 目次の解析
contents.vec <- sapply(data.df$filename,file_read,dirname="num")
linear.regression(contents.vec, books.df$difficulty)
# 書籍名の解析
titles.vec <- books.df$title
linear.regression(title.vec, books.df$difficulty)
# 目次と書籍名の解析
mixed.vec <- add_str_vec(titles.vec, contents.vec)
linear.regression(mixed.vec, books.df$difficulty)
