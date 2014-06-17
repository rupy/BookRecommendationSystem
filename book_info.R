source("similarity.R")

# データベースの読み込み
book.info.df <- get.book.info.from.db(db.file.name)

# lexicalize関数に与える引数はスペース区切りでなければならない．
# 日本語は通常スペース区切りではないので，スペース区切りにする．
doc.line.vec <- gen.doc.line.vec(book.info.df, TRUE)
# LDAの引数に与えるデータを作成
corpus <- lexicalize(doc.line.vec, lower=TRUE)
# 通常コーパスデータは大きいため，動作確認に適さないので
# 小規模なコーパスデータを生成
small.corpus <- lexicalize(doc.line.vec[1:100], lower=TRUE)

# トピック数
K <- 100
# LDAによって文書におけるトピックの確率を得る
topic.proportions <- lda_topic_doc(corpus,topic_num=5,K=K)
# トピックの棒グラフ
topic_bar(topic.proportions, book.info.df$title)


