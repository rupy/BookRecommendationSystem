library(RSQLite)
library(RMeCab)
library(stringr)

# wordnetでの同義語を数える
get.word.def.count <-function(word){

  # 準備
  sqlite.drv <- dbDriver("SQLite", max.con = 1)
  sqlite.conn <- dbConnect(sqlite.drv, dbname="wnjpn.db")
  
  # クエリ実行  
  sql.stmt = paste("select count(*) as c from sense, word
where synset in (select synset from word,sense where lemma = '",word,"' and sense.wordid = word.wordid) 
and sense.wordid = word.wordid 
and sense.lang = 'jpn';", sep = "")
  resp <- dbSendQuery(sqlite.conn, sql.stmt)
  result <- fetch(resp, n = 1) # 行数は１行
  
  # 後処理
  dbClearResult(resp)
  dbDisconnect(sqlite.conn)
  
  result[1,"c"]
}

# 文字列に含まれる語義数の平均を求める
rate.def.count <- function(str){

  # 空の文字列が与えられた場合には終了
  if(nchar(str)==0){return(0)}
  
  # 形態素解析
  words <- RMeCabC(str,1)
  words <- unlist(words)
  
  # すべての語をループで回す
  sum <- 0 # 語義の合計
  c <- 0 # カウンタ
  for(i in seq(length(words))){
    # if(names(words)[i] %in% c("名詞","動詞","形容詞","副詞")){
    
    # 記号が含まれる場合はスキップ
    if(str_detect(words[i], "[[:punct:]]")) next
    
    # 語義数を得る
    def.count <- get.word.def.count(words[i])
    
    # 合計を得る（結果が0の場合にはwordnetに無いので数えない）
    if(def.count != 0){
      sum <- sum + def.count
      c <- c + 1
    }
    # else{
    # print(words[i])
    # }
    # }
  }
  rate <- sum/c
  return(rate)
}


# 結構時間かかる
# 文字列のベクトルから語義数のベクトルを作成
create.wordnet.vec <- function(str.vec){
  wnet.vec <- c()
  for( i in seq(length(str.vec))){
    # print(i)
    wnet.vec <- c(wnet.vec, rate.def.count(str.vec[i]))
  }
  wnet.vec
}
