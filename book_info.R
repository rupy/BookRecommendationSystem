library(RSQLite)

get.book.info.from.db <- function(filename, rowsize=-1){

  sqlite.drv <- dbDriver("SQLite", max.con = 1)
  sqlite.conn <- dbConnect(sqlite.drv, dbname=filename)
  
  sql.stmt = "SELECT * FROM book_info;"
  
  resp <- dbSendQuery( sqlite.conn, sql.stmt)
  
  # -1の時にすべての結果を取り出す
  result <- fetch(resp, rowsize)

  dbClearResult(resp)
  dbDisconnect(sqlite.conn)
  
  result  
}

book.info <- get.book.info.from.db("book_info.db")
