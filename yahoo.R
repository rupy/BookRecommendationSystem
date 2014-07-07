library("YjdnJlp")

#ヤフーの形態素解析ツールを使うための準備
applycation_id <- "*************"
yahoo.con <- initYjdnJlp(applycation_id)

# Yahooの形態素解析APIをつかった解析
# なぜかよみがなが表示されない・・・
YahooDF <- function(str){
  yahoo.res <- MAService(yahoo.con,str)
  yahoo.df <- toDataFrame(yahoo.res)
  yahoo.df
}  
