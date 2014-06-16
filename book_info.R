library(RSQLite)
library(RMeCab)


# データベースから書籍情報を取得する
get.book.info.from.db <- function(filename, rowsize=-1){

  # 準備
  sqlite.drv <- dbDriver("SQLite", max.con = 1)
  sqlite.conn <- dbConnect(sqlite.drv, dbname=filename)

  # クエリ実行
  sql.stmt = "SELECT * FROM book_info WHERE contents <> '';"
  resp <- dbSendQuery( sqlite.conn, sql.stmt)
  result <- fetch(resp, rowsize) # rowsizeが-1の時にすべての結果を取り出す

  # 後処理
  dbClearResult(resp)
  dbDisconnect(sqlite.conn)
  
  result  
}

# 禁止語を除去する正規表現を生成
gen.ban.regrep <- function(){
  forbidden.words <- c(
    "chapter","chap","section","part","tip","lesson","step","ステップ",
    "case","phase","章","節","部","話","回","週","時間目","編",
    "はじめに","初めに","Introduction","まえがき","前書き","目次","イントロダクション",
    "おわりに","終わりに","最後に","さいごに","あとがき","謝辞","エピローグ","EPILOGUE",
    "まとめ","参考文献","索引","Index","インデックス",
    "付録","巻末資料","Appendix","特集","ほか",".曜日"
  )
  #数字を含める
  for(i in 0:100){
    forbidden.words <- c(forbidden.words,as.character(i))    
  }
  for(i in 0:9){
    forbidden.words <- c(forbidden.words,sprintf("%02d", i))    
  }
  for(i in 0:200){
    forbidden.words <- c(forbidden.words,sprintf("%03d", i))    
  }
  forbidden.words <- c(forbidden.words, "[[:punct:]]+")

  ban.pattern <- paste(forbidden.words, sep="", collapse="|")
  ban.pattern <- paste("^(", ban.pattern, ")$", sep="", collapse="")
  ban.pattern
}

# namesに登録されている品詞で単語ベクトルを制限
limit.by.pos <- function(word.vec, pos){
  if(any(names(word.vec) %in% pos)){
    word.vec <- word.vec[names(word.vec) %in% pos]    
  }
  word.vec
}

# テキストをスペース区切り単語列に変換する
gen.words.line <- function(text, pos=NULL){
  
  # 形態素解析を行う。mypref=1を指定すると活用の原型となる
  words.vec <- unlist(RMeCabC(text, mypref=1))
  # 名詞のみに限定
  limited.words.vec <- NULL
  if(any(names(words.vec) %in% pos)){
    limited.words.vec <- words.vec[names(words.vec) %in% pos]    
  }

  # 禁止語の正規表現を生成
  ban.pattern <- gen.ban.regrep()
  
  # 禁止語を除去
  ban.vec <- grep(ban.pattern, limited.words.vec, ignore.case=TRUE)
  banned.words.vec <- limited.words.vec
  if(length(ban.vec)){
    banned.words.vec <- limited.words.vec[-ban.vec]
  }
  
  # スペースに区切った単語列にする
  words.line <- paste(banned.words.vec, sep="", collapse=" ")
  words.line  
}

# データベースから読みだした書籍情報からスペース区切り文字列のベクトルを生成する
gen.doc.line.vec <- function(db.file.name, use.title){
  
  # 書籍情報をDBから読み出し
  book.info.df <- get.book.info.from.db(db.file.name)
  boktext.vec <- NULL
  if(use.title){
    text.vec <- sapply(seq(length(book.info.df$title)), function(i){paste(book.info.df$title[i], "\n", book.info.df$contents[i], sep="", collapse=" ")})
  }else{
    text.vec <- book.info.df$contents
  }
  doc.line.vec <- sapply(text.vec, gen.words.line, pos=c("名詞")) 
  names(doc.line.vec) <- NULL
  doc.line.vec
}

doc.line.vec <- gen.doc.line.vec("book_info.db", TRUE)
