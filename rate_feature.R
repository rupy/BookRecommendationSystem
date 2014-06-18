library(RMeCab)
library(stringr)

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

simbol.rate <- function(str){
  punct <- str_extract_all(str, "[[:punct:]]")[[1]]
  return(length(punct)/nchar(str))
}

blank.rate <- function(str){
  
  blank<- str_extract_all(str, "[[:blank:]]")[[1]]
  return(length(blank)/nchar(str))
}

create.rate.mat <-function(strvec){
  rate.mat <- sapply(strvec,rate.vector)
  colnames(rate.mat) <- NULL
  rate.mat[is.na(rate.mat)] <- 0
  rownames(rate.mat)  <- struct.pos.simbol
  rate.mat
}


rate.vector <- function(str){
  rate.vec <- c(
    structure.rate.vector(str),
    pos.rate.vector(str)
    #rate_def_count(str),
    #check_easy(str)
  )
  rate.vec
}

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

pos.rate.vector <- function(str){
  rate.vec <- c(
    rate.type(str,type="名詞"),
    rate.type(str,type="動詞"),
    rate.type(str,type="形容詞"),
    rate.type(str,type="副詞"),
    rate.type(str,type="助詞"),
    rate.type(str,type="接続詞"),
    rate.type(str,type="助動詞"),
    rate.type(str,type="連体詞"),
    rate.type(str,type="感動詞")
  )
  rate.vec  
}

# 品詞を数える関数
rate.type <- function(str,type){
  if(nchar(str)==0){return(0)}
  res <- RMeCabC(str)
  res <- unlist(res)
  res2 <- res[names(res) == type]
  rate <- length(res2)/length(res)
  return(rate)
}

