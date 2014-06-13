library(RSQLite)

# データベースから書籍情報を取得する
get.book.info.from.db <- function(filename, rowsize=-1){

  # 準備
  sqlite.drv <- dbDriver("SQLite", max.con = 1)
  sqlite.conn <- dbConnect(sqlite.drv, dbname=filename)

  # クエリ実行
  sql.stmt = "SELECT * FROM book_info;"
  resp <- dbSendQuery( sqlite.conn, sql.stmt)
  result <- fetch(resp, rowsize) # rowsizeが-1の時にすべての結果を取り出す

  # 後処理
  dbClearResult(resp)
  dbDisconnect(sqlite.conn)
  
  result  
}

book.info <- get.book.info.from.db("book_info.db")

gen_forbidden_words <- function(){
  forbidden_words <- c(
    "chapter","chap","section","part","tip","lesson","step","ステップ",
    "case","phase","章","節","部","話","回","週","時間目","編",
    "はじめに","初めに","Introduction","まえがき","前書き","目次","イントロダクション",
    "おわりに","終わりに","最後に","さいごに","あとがき","謝辞","エピローグ","EPILOGUE",
    "まとめ","参考文献","索引","Index","インデックス",
    "付録","巻末資料","Appendix","特集","ほか",".曜日"
  )
  #数字を含める
  for(i in 0:100){
    forbidden_words <- c(forbidden_words,as.character(i))    
  }
  for(i in 0:9){
    forbidden_words <- c(forbidden_words,sprintf("%02d", i))    
  }
  for(i in 0:200){
    forbidden_words <- c(forbidden_words,sprintf("%03d", i))    
  }
  forbidden_words
}
