library(RMeCab)
library(stringr)

#分類語彙表を読み込む
sakuin <- read.csv("goibunrui/sakuin.UTF", header=FALSE, stringsAsFactors=F)
colnames(sakuin) <- c("表記", "読み", "分類番号", "段落番号", "段落内行番号", "行内語番号")

#単語の同義語を探す関数
search.same.meaning.words <- function(word){
  line <- sakuin[sakuin$表記==word,]
  # 単語がヒットしなかった場合は読みで探す
  if(nrow(line) < 1){
    line <- sakuin[sakuin$読み==word,]
  }
  all.words <- c()
  # それでも単語が見つからなかった場合は未定義なので終了
  if(nrow(line) < 1){
    return(all.words)
  }
  # 同表記で複数の分類になっている場合があるので（例：優しい）その回数分ループする
  for(i in seq(nrow(line))){
    # 分類番号と段落番号が一致するものが同義語
    sim <- sakuin[(sakuin$分類番号==line[i,]$分類番号 & sakuin$段落番号==line[i,]$段落番号),]
    all.words <- c(all.words, sim$表記)      
  }
  all.words
}

# 語彙表から数える
get.goi.count <-function(word){
  words <- search.same.meaning.words(word)
  #print(words)
  return(length(words))
}


# 分類語彙表でのスコア
rate.goi.count <- function(str){
  # 文字ベクトルがからなら終了
  if(nchar(str)==0){return(0)}
  # 形態素解析を行って単語に分ける
  words <- RMeCabC(str,1)
  words <- unlist(words)
  sum <- 0
  c <- 0
  # それぞれの単語でループする
  for(i in seq(length(words))){
    # 助動詞，記号を含むものは飛ばす
    if( !(names(words)[i] %in% c("助詞","助動詞")))next
    if(str_detect(words[i], "[[:punct:]]")) next
    # スコアの計算
    def.count <- get.goi.count(words[i])
    # 語彙が見つからなかったら数えない（実は数えたほうが特徴語をカウントしやすいとかってある？）
    if(def.count != 0){
      sum <- sum + def.count
      c <- c + 1
    }else{
      # print(words[i])
    }
  }
  # ゼロで割らないようにする
  if(c==0){
    rate <- 0
  }else{
    rate <- sum/c    
  }
  return(rate)
  
}

# 結構時間かかる
# 語彙の数のベクトルを作成
create.goi.vec <- function(strvec){
  goi.vec <- c()
  for( i in seq(length(strvec))){
    print(i)
    goi.vec <- c(goi.vec, rate.goi.count(strvec[i]))
  }
  goi.vec
}
