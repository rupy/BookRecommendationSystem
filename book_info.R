source("similarity.R")

#
# 類似度の計算
#

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
topic.proportions <- topic.proportions.by.lda(corpus,topic_num=5,K=K)
rownames(topic.proportions) <- book.info.df$title
# トピックの棒グラフ
topic.bar(topic.proportions)
# トピックの円グラフ
topic.pie(topic.proportions)


# トピック数
K <- 100
# 文書の確率を得る
doc.proportions <- doc.proportions.by.lda(corpus,topic_num=5,K=K)
colnames(doc.proportions) <- book.info.df$title
# 文書の棒グラフ
doc.bar(doc.proportions)

# 類似書籍を求める
get.similar.book(1,topic.proportions)[1:20]

# コサイン
cos.mat <- cosine.mat(doc.proportions)
# 類似ランク行列
rank.mat <- similar.rank.mat(cos.mat)