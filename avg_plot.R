

mean.each.diff <- function(d){
  cat("スペアマンの順位相関係数",cor(books.df$difficulty,mixed.rate.df[,d],method="s"))
  mean.each <- c()
  for(i in seq(4)){
    # cat(i,": ")
    m <- mean(mixed.rate.df[books.df$difficulty == i,d] )
    # cat( m, "\n"  )
    mean.each <- c(mean.each,m)
  }
  mean.each
}

struct.str <- c("平仮名","片仮名","漢字","英字","数字","空白","記号")
pos.str <- c("名詞","動詞","形容詞","副詞","助詞","接続詞","助動詞","連体詞","感動詞")
struct.pos.str <- c(struct.str, pos.str)

# 平均と難易度の関係をプロット
avg.plot.struct <- function(){
  par(mfrow=c(1,1),new=F) 
  for(i in seq(length(struct.str))){
    plot(1:4,mean.each.diff(i)*100, lty=i, ylim=c(0,0.4), type="b", main=pos.str[i])
    if(i != length(pos.str))par(new=T)    
  }
}
avg.plot.pos <- function(){
  par(mfrow=c(1,1),new=F) 
  for(i in seq(length(pos.str))){
    plot(1:4,mean.each.diff(i+length(struct.str))*100, lty=i, ylim=c(0,1), type="b", main=pos.str[i])
    if(i != length(pos.str))par(new=T)
  }
}

avg.plot.all <- function(){
  par(mfrow=c(4,4), new=F) 
  for( i in seq(length(struct.pos.str))){
    plot(1:4,mean.each.diff(i)*100, xlab="", ylab="", type="b", main=struct.pos.str[i])    
  }
}

#一つづつプロット
image.filename <- c("hiragana.png","katakana.png","kanji.png","alphavet.png",
                    "number.png","blank.png","simbol.png",
                    "meshi.png","doshi.png","keyoshi.png","fukushi.png","joshi.png",
                    "setsuzokushi.png","jodoshi.png","rentaishi.png","kandoshi.png"
)

plot.each.glaph <- function(){
  par(mfrow=c(4,4), new=F) 
  par(mar=c(1,1,1,1))
  for( i in seq(length(struct.pos.str))){
    png(filename=paste("score/",image.filename[i]),width=700,pointsize=20)
    plot(1:4,mean.each.diff(i)*100, xlab="難易度", ylab="スコアの平均", type="b", main=struct.pos.str[i])    
    dev.off()
  }
}

# 語彙数の平均
mean.each.diff.goi <- function(){
  mean.each <- c()
  for(i in seq(4)){
    # cat(i,": ")
    m <- mean(goi.vec[books.df$difficulty == i] )
    # cat( m, "\n"  )
    mean.each <- c(mean.each,m)
  }
  mean.each
  
}

# 語彙のプロット
avg.plot.goi <- function(){
  par(mfrow=c(1,1),new=F) 
  plot(1:4,mean.each.diff.goi(), type="b", xlab="難易度", ylab="同義語数", main="分類語彙表による同義語の数")
}

# 語彙数の平均
mean.each.diff.wordnet <- function(){
  mean.each <- c()
  for(i in seq(4)){
    # cat(i,": ")
    m <- mean(wordnet.vec[books.df$difficulty == i] )
    # cat( m, "\n"  )
    mean.each <- c(mean.each,m)
  }
  mean.each
  
}

# 語彙のプロット
avg.plot.wordnet <- function(){
  par(mfrow=c(1,1),new=F) 
  plot(1:4,mean.each.diff.wordnet(), type="b", xlab="難易度", ylab="同義語数", main="wordnetによる同義語の数")
}