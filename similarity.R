library(RSQLite)
library(RMeCab)
library(lda)

# 禁止語の正規表現を生成
global.ban.pattern <- gen.ban.regrep()

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
    "はじめに","初めに","introduction","まえがき","前書き","目次","イントロダクション",
    "おわりに","終わりに","最後に","さいごに","あとがき","謝辞","エピローグ","epilogue",
    "まとめ","参考文献","索引","index","インデックス",
    "付録","巻末資料","appendix","特集","ほか",".曜日"
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
  
  # 禁止語を除去
  ban.vec <- grep(global.ban.pattern, limited.words.vec, ignore.case=TRUE)
  banned.words.vec <- limited.words.vec
  if(length(ban.vec)){
    banned.words.vec <- limited.words.vec[-ban.vec]
  }
  
  # スペースに区切った単語列にする
  words.line <- paste(banned.words.vec, sep="", collapse=" ")
  words.line  
}

# データベースから読みだした書籍情報からスペース区切り文字列のベクトルを生成する
gen.doc.line.vec <- function(book.info.df, use.title){
  
  # 書籍情報をDBから読み出し
  boktext.vec <- NULL
  if(use.title){
    text.vec <- sapply(seq(length(book.info.df$title)), function(i){
      paste(book.info.df$title[i], "\n", book.info.df$contents[i], sep="", collapse=" ")
    })
  }else{
    text.vec <- book.info.df$contents
  }
  doc.line.vec <- sapply(text.vec, gen.words.line, pos=c("名詞")) 
  names(doc.line.vec) <- NULL
  doc.line.vec
}

# LDAによって文書におけるトピックの生起確率を求める
topic.proportions.by.lda <- function(corpus,topic_num=6, K=10,num_iter=25){
  
  # LDAの結果を格納
  # assignments: 長さDのリストで、各要素は整数のベクトルで単語ごとのトピック割り当てを示す
  # topics: K(トピック数) * V(単語数)の行列で単語（列）がトピック（行）に割り当てられた回数を示す
  # topic_sums: トピック数Kの長さを持つベクトルで単語が各トピックに割り当てられた総数を示す
  # document_sums: K（トピック数） * D(ドキュメント数)の行列でそれぞれのドキュメントにおいて単語が各トピックに割り当てられた総数を示す
  # log.likelihoods:
  result <- lda.collapsed.gibbs.sampler(
    corpus$documents,
    K,
    corpus$vocab,
    num_iter, # num iterations
    0.1,
    0.1,
    compute.log.likelihood=TRUE
  )
  # 各トピックのトップワードを抽出
  top.words <- top.topic.words(result$topics, topic_num, by.score=TRUE)
  # 分母：ドキュメントごとの単語数のベクトル（長さ：ドキュメント数）
  # document_sumsの正規化（ドキュメントのトピックに割り当てられる確率（割合））
  topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
  # NAになっている部分は1/Kにする（1/Kなのは均等になるから？、わり算でNAが生じる事がある？）
  topic.proportions[is.na(topic.proportions)] <- 1 / K
  # top.wordsを結合して列名にする
  colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
  # デーブルの構造変換を行う
  topic.proportions
}

# LDAによってトピックがどの文書に割り当てられるかの確率を求める
doc.proportions.by.lda <- function(corpus, topic_num=6, K=10,num_iter=25){
  result <- lda.collapsed.gibbs.sampler(
    corpus$documents,
    K,
    corpus$vocab,
    num_iter, # num iterations
    0.1,
    0.1,
    compute.log.likelihood=TRUE
  )
  top.words <- top.topic.words(result$topics, topic_num, by.score=TRUE)
  # トピックごとのドキュメントに割り当てられる割合
  # 行がトピック、列がドキュメントを表す
  doc.proportions <- result$document_sums/rowSums(result$document_sums)
  colnames(doc.proportions) <- names(corpus$documents)
  doc.proportions[is.na(doc.proportions)] <- 1 / length(corpus$documents)
  rownames(doc.proportions) <- apply(top.words, 2, paste, collapse=" ")
  doc.proportions  
}

# トピックの生起確率の棒グラフを生成
topic.bar <-function(topic.proportions, pickup = 0){
  
  if(pickup == 0){
    # 番号が決まっていなければランダムに決める
    pickup <- floor(runif(1,1,ncol(topic.proportions)))
  }
  # グラフに不要な０％の部分を除去
  bar.data <- topic.proportions[pickup, topic.proportions[pickup,]!=0]    
  # トピック列
  names(bar.data) <- colnames(topic.proportions)[topic.proportions[pickup,]!=0]
  # 昇順にソート
  bar.data <- sort(bar.data, de=FALSE)
  # 表題
  title <- rownames(topic.proportions)[pickup]
  # グラフの描画
  barplot(bar.data, col=rainbow(length(bar.data)), beside=TRUE, horiz=TRUE, las=1,
          xlab="proportion",legend.text = rownames(bar.data),main=title)
  cat("title:", title, " pickup:", pickup)
  
}

# トピックの円グラフを作成
topic.pie <-function(topic.proportions, pickup = 0){
  
  if(pickup == 0){
    # 番号が決まっていなければランダムに決める
    pickup <- floor(runif(1, 1, nrow(topic.proportions)))
  }
  # 表題
  title <- rownames(topic.proportions)[pickup]
  # グラフに不要な０％の部分を除去
  pie.data <- topic.proportions[pickup, topic.proportions[pickup,]!=0]    
  # ベクトルの長さが１だとラベルが消えてしまうので復元する
  if(length(pie.data) == 1){ 
    names(pie.data) <- colnames(topic.proportions)[topic.proportions[pickup,]!=0]
  }
  # 昇順にソート
  pie.data <- sort(pie.data, de=FALSE)
  # グラフの描画
  pie(pie.data, col=rainbow(length(pie.data)), labels = names(pie.data), main=title, radius=0.5)
  cat("title:", title, " pickup:", pickup)
}

# 文書の生起確率の棒グラフを生成
doc.bar <-function(doc.proportions,pickup=0){
  
  if(pickup==0){
    # 番号が決まっていなければランダムに決める
    pickup <- floor(runif(1,1,nrow(doc.proportions)))
  }
  # グラフに不要な０％の部分を除去
  bar.data <- doc.proportions[pickup,doc.proportions[pickup,]!=0]
  # 書籍名
  names(bar.data) <- colnames(doc.proportions)[doc.proportions[pickup,]!=0]
  # 昇順にソート
  bar.data <- sort(bar.data, de=FALSE)
  # 表題
  title <- rownames(doc.proportions)[pickup]
  # グラフの描画
  barplot(bar.data, col=rainbow(length(bar.data)), beside=TRUE, horiz=TRUE, las=1,
          xlab="proportion",main=title)
  
  cat("topic:", title, " pickup:", pickup)
  
}

# 類似度を求めるためのコサインの定義
my.cosine <- function( a, b=NULL ){
  if( is.matrix(a) && is.null(b) ){
    apply(a, 2, function(x){
      apply(a, 2, function(y){
        crossprod(x, y) / (sqrt(crossprod(x) * crossprod(y)))
      })
    })
  }
  else if( is.matrix(a) && is.vector(b) ){
    apply(a, 2, function(x){
      crossprod(x, b) / (sqrt(crossprod(x) * crossprod(b)))
    })
  }
  else if( is.vector(a) && is.vector(b) ){
    crossprod(a, b) / (sqrt(crossprod(a) * crossprod(b)))
  }
}

# 類似書籍を求める
get.similar.book <- function(num, topic.proportions){
  topic.proportions <- t(topic.proportions)
  cat(colnames(topic.proportions)[num],"と似ている書籍","\n")
  cos <- my.cosine(a=topic.proportions,
                  b=topic.proportions[,num])
  cos <- sort(cos,de=T)
  cos
}

# 類似度を測るため，行列から類似行列を生成する
cosine.mat <- function(doc.proportions){
  cos.mat <- my.cosine(a=doc.proportions)
  cos.mat
}

# 類似行列から類似ランク行列を生成
similar.rank.mat <- function(cos.mat){
  rank.mat <-apply(1-cos.mat, 1, rank, ties.method = c("first"))
  rank.mat
}

