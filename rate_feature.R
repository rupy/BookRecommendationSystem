library(RMeCab)
library(stringr)

#
# 構成要素に関する関数
#

# ひらがなの割合を数える関数
hiragana.rate <- function(str){
  return(str_count(str,"[ぁ-ん]")/nchar(str))
}
# カタカナを数える関数
katakana.rate <- function(str){
  return(str_count(str,"[ァ-ヴー]")/nchar(str))
}
# 漢字を数える関数
kanji.rate <- function(str){
  return(str_count(str,"[一-龠]")/nchar(str))
}
# アルファベットを数える関数
alphabet.rate <- function(str){
  return(str_count(str,"[a-zA-Z]")/nchar(str))
}
# 数字を数える関数
number.rate <- function(str){
  return(str_count(str,"[0-9]")/nchar(str))
}
# 記号を数える関数
simbol.rate <- function(str){
  punct <- str_extract_all(str, "[[:punct:]]")[[1]]
  return(length(punct)/nchar(str))
}
# 空白を数える関数
blank.rate <- function(str){
  
  blank<- str_extract_all(str, "[[:blank:]]")[[1]]
  return(length(blank)/nchar(str))
}
# 構成要素の含有率を基に特徴ベクトルを作成
structure.rate.vector <- function(str){
  rate.vec <- c(
    hiragana.rate(str),
    katakana.rate(str),
    kanji.rate(str),
    alphabet.rate(str),
    number.rate(str),
    blank.rate(str),
    simbol.rate(str)
  )
  rate.vec
}

#
# 品詞に関する関数
#

# 品詞を数える関数
rate.pos <- function(str,type){
  if(nchar(str)==0){return(0)}
  res <- RMeCabC(str)
  res <- unlist(res)
  res2 <- res[names(res) == type]
  rate <- length(res2)/length(res)
  return(rate)
}

# 品詞の含有率を基に特徴ベクトルを作成
pos.rate.vector <- function(str){
  rate.vec <- c(
    rate.pos(str,type="名詞"),
    rate.pos(str,type="動詞"),
    rate.pos(str,type="形容詞"),
    rate.pos(str,type="副詞"),
    rate.pos(str,type="助詞"),
    rate.pos(str,type="接続詞"),
    rate.pos(str,type="助動詞"),
    rate.pos(str,type="連体詞"),
    rate.pos(str,type="感動詞")
  )
  rate.vec  
}

#
# 構成要素と品詞に関する関数
#

# 品詞の含有率を基に特徴ベクトルを作成
rate.vector <- function(str){
  rate.vec <- c(
    structure.rate.vector(str),
    pos.rate.vector(str)
    #rate_def_count(str),
    #check_easy(str)
  )
  rate.vec
}

# 行列の作成
create.rate.mat <-function(strvec){
  rate.mat <- sapply(strvec,rate.vector)
  colnames(rate.mat) <- NULL
  rate.mat[is.na(rate.mat)] <- 0
  rownames(rate.mat)  <- struct.pos.simbol
  rate.mat
}

