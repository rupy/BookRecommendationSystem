source("similarity.R")

# lexicalize関数に与える引数はスペース区切りでなければならない．
# 日本語は通常スペース区切りではないので，スペース区切りにする．
doc.line.vec <- gen.doc.line.vec("book_info.db", TRUE)
# LDAの引数に与えるデータを作成
corpus <- lexicalize(doc.line.vec, lower=TRUE)
# 通常コーパスデータは大きいため，動作確認に適さないので
# 小規模なコーパスデータを生成
corpus_small <- lexicalize(doclines[1:100], lower=TRUE)

# トピック数
K <- 100
# サンプル数
N <- 5
# LDAによって文書におけるトピックの確率を得る
topic.proportions <- lda_topic_doc(corpus,topic_num=5,K=K)

