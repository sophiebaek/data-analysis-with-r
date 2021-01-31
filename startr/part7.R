# 머신 러닝의 종류:
# Machine Learning (머신러닝): 컴퓨터가 데이터를 학습해서 분류/예측 등의 모델을 만들게 하는 통계 알고리즘
  # - Supervised Learning (지도학습): 정답 (label)이 있는 데이터로 학습하는 알고리즘
  # - Unsupervised Learning (비지도학습): 정답 (label)이 없는 데이터로 학습하는 알고리즘
    # - Anomaly detection (이상치 탐지): 평소의 패턴이랑 다른 것들을 잡아내는데 사용 ex) 분실카드 특이사용패턴
    # - Time-series modeling: 과거의 상태 (state)를 기반으로 미래의 state를 예측 ex) 날씨 예측
      # 지도학습 모델: State의 label이 있는 경우
      # 비지도학습 모델: State를 추론해서 예측하는 경우
    # - Dimension reduction (차원축소): 불필요한 정보들을 제거, 데이터를 축소하기 위해 사용 ex) 사진 화질 축소
    # - Latent (숨은, 잠재의) variable models: 데이터를 잘 설명해주는 latent variable (숨은 변수)을 찾는데 사용, Dimension reduction이랑 밀접한 개념 -> 데이터를 잘 설명 못하는 변수를 제거하면 dimension reduction이 되기 때문!

# 현업에서 사용되는 비지도학습:
# Anomaly detection:
  # 이상치 탐지를 통한 리스트 관리 - 게임 버그성 플레이 탐지 & 빠른 조치
  # 사람이 모니터링 하는 것 보다 빠르게 감지할 수 있는 시스템 구축
# Time series modeling:
  # 음성인식
  # 주가예측
# Latent variable models & Dimension reduction:
  # 차원축소
    # 중요한 변수를 가려내기 위해 사용
    # 지도학습의 전처리 과정으로 (성능 향상을 위해) 사용 - 이미지 데이터 크기 축소
  # 군집화
    # 고객 데이터 군집화 - 상품 판매 전략 수집
    # 상품 추천 알고리즘 개발

##############################################################################

# PCA (주성분 분석)의 개념 이해:
# Principal Component Analysis를 줄여서 PCA라 함
# 내가 가진 데이터에서 가장 중요한 성분을 순서대로 추출하는 기법
  # 내 데이터의 분산을 가장 잘 설명해주는 축이 주성분1 (PC1)
  # PC1에 직교하는 (직각의) 축이 주성분2 (PC2)

# 언제 PCA를 사용하나요?
# 1. 내 데이터에 쓸데 없는 정보들이 너무 많아 양을 줄이고 싶을 때
# 2. 내 데이터에 잠재하는 변수 (latent variable)을 확인하고 싶을 때
# 3. 분석하기 전 의미 없는 변수들을 가려내고 싶을 때
  # 각 변수들의 가중치를 확인 후 판단

# PCA process:
# 1. 공분산 행렬 계산
# 2. 공분산행렬의 eigenvalue와 eigenvector 계산
# 3. eigenvalue의 크기 순서대로 eigenvector 나열
# 4. 정렬된 eigenvector 중 필요한 만큼 일부 선택하여 차원 축소

# 공분산 행렬: 데이터 간 퍼져있는 정도를 나타내는 행렬 -> 데이터의 분산에 대한 정보 필요
# Eigenvalue (고유값) & Eigenvector (고유벡터): 공분산 행렬에서 나타나는 고유한 벡터와 벡터의 고유값을 의미함
  # 고유한 벡터: 분산의 방향, 주성분
  # 벡터의 고유값: 분산의 크기, 주성분의 분산

# 주성분의 개수 설정:
# 1. 시각화를 위해 2 or 3개로 주성분 개수 설정
# 2. Eigenvalue > 1을 기준으로 주성분 개수 설정
  # - 주성분은 observation이 아닌, latent variable이기 때문에, 해석을 해줘야함
  # - 그런데, 내 가 측정한 변수보다 분산이 작다면 설명력이 떨어지는데 해석까지 해줘야 한다는 의미
# 3. 주성분 개수가 늘어나도 분산이 더이상 추가되지 않는 지점에서 주성분 개수 설정
  # - scree plot에서 elbow point로 주성분 개수 설정
  # - 해석하는 수고를 할 만한 주성분까지 남겨준다

##############################################################################

# 1. 데이터 확인
head(iris)

# 2. 결측치 확인
colSums(is.na(iris))

# 3. 변수별 기술통걔 및 분포 확인
summary(iris)
boxplot(iris[, 1:4])

# 4. pca 함수 적용 및 요약 결과 확인
iris.pca <- prcomp(iris[1:4], center = T, scale. = T) # pca 함수
summary(iris.pca) # pca 요약정보. standard deviation 제곱 = 분산 = eigenvalue
iris.pca$rotation # 각 주성분의 eigenvector
head(iris.pca$x, 10) # 각 주성분의 값

# 5. scree plot 확인
plot(iris.pca, type = 'l', main = 'Scree Plot') # PC의 분산을 y축으로 scree plot 생성

# 6. 차원축소
head(iris.pca$x[, 1:2], 10) # 2개의 차원으로 축소

# 7. 2차원 시각화
install.packages("ggfortify")
library(ggfortify)
autoplot(iris.pca, data = iris, colour = 'Species') # 2차원으로 축소된 데이터 시각화

##############################################################################

# 1. 데이터 확인
install.packages("jpeg")
library(jpeg)
cat <- readJPEG('/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/cat.jpeg')
class(cat)
dim(cat)

# 2. rbg 데이터 분할 및 주성분 분석
r <- cat[, , 1] # array에서 r에 해당하는 데이터
g <- cat[, , 2] # array에서 g에 해당하는 데이터
b <- cat[, , 3] # array에서 b에 해당하는 데이터
cat.r.pca <- prcomp(r, center = F) # r 데이터 주성분분석 / center = F -> 표준화 하지 않음
cat.g.pca <- prcomp(g, center = F) # g 데이터 주성분분석
cat.b.pca <- prcomp(b, center = F) # b 데이터 주성분분석
rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca) # 분석 결과 rbg로 합침

# 3. 차원축소하여 jpg로 저장
pc <- c(2, 10, 50, 100, 300) # 축소할 차원 수
for (i in pc) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[, 1:i] %*% t(j$rotation[, 1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('cat_pca_', i, '.jpeg', sep = ''))
}

##############################################################################

# Clustering (군집화): 유사한 성질을 가지는 데이터끼리 cluster(군집)를 나누는 과정
  # 목표: 군집 내 데이터들의 거리는 가깝게, 군집 간 거리는 멀게

# K-means clustering: K개의 중심을 정하고, 그 중심을 기반으로 clustering하는 기법
# K-means process:
# 1. 랜덤하게 K개의 점을 찍고 각 점을 중심으로 데이터들을 할당
# 2. 할당된 군집에서 다시 중심점을 찾고 해당 중심점에서 가장 가까운 데이터로 재군집화
# 3. 군집에 할당된 데이터들이 바뀌지 않을 때 까지 2번의 과정을 반복

# K-means clustering 활용:
# - 군집화 및 군집별 특성 파악
    # ex1) 고객 유형을 분류하여 상품 판매 전략 도축
    # ex2) 제품의 성분 및 특성에 따라 분류하여 제품 추천 로직 (알고리즘) 개발
# - 이미지 데이터의 색상을 군집화 하여 사진 색상 축소

# K 개수를 선택하는 방법
# 1. 사전 정보 (domain knowledge)를 바탕으로 k 개수 설정
  # ex) 꽃의 데이터인데 3종류의 꽃이 있음
# 2. Elbow method
  # Within Sum of square (WSS) 그래프에서 Elbow point로 k 개수 설정
  # Sum of square (편차제곱합): 평균에서 각 데이터간의 거리 제곱을 모두 합한 값
    # sum of square / 데이터 수 = 분산
  # Elbow point: 군집 수를 늘려도 군집 내 데이터들의 거리가 더이상 크게 가까워지지 않는 지점
# 3. Silhouette method
  # 군집 내 거리 (a)와 최근접 군집 간의 거리 (b)를 비교하여 a는 최소, b는 최대가 되는 k로 개수 설정

##############################################################################

# K-medoids (중앙점) clustering: 군집의 평균점을 찾는 것이 아닌, 중앙점을 찾아 군집화
# 극단치의 영향을 덜 받는 clustering 기법

# K-medoids process:
# 1. 랜덤하게 K개의 데이터를 선택하고 각 데이터를 중심으로 군집 할당
# 2. 할당된 군집에서 다시 중앙점을 찾고 해당 중앙점에서 가장 가까운 데이터로 재군집화
# 3. 군집에 할당된 데이터들이 바뀌지 않을 때 까지 2번의 과정을 반복

##############################################################################

# 1. 데이터 다운로드
# https://archive.ics.uci.edu/ml/datasets/Wholesale_customers

# 2. 데이터 read
df <- read.csv("/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/Wholesale\ customers\ data.csv", stringsAsFactors = F, header = T)

# 3. 데이터 확인
library(dplyr)
head(df)
df$Channel <- df$Channel %>% as.factor() # 범주형 데이터 펙터로 변경
df$Region <- df$Region %>% as.factor() # 범주형 데이터 펙터로 변경

# 4. 결측치 확인
colSums(is.na(df))

# 5. 변수별 기술통계 및 분포 확인
summary(df)
boxplot(df[, 3:ncol(df)])

# 6. 지수표기법 변경
options(scipen = 100)
boxplot(df[, 3:ncol(df)])

# 7. 이상치 제거
temp <- NULL
for (i in 3:ncol(df)) {
  temp <- rbind(temp, df[order(df[, i], decreasing = T), ] %>% slice(1:5))
}
temp %>% arrange(Fresh) %>% head() # 중복 있음
temp <- distinct(temp) # 중복 제거
df.rm.outlier <- anti_join(df, temp) # df에서 temp 제거

# 8. 이상치 제거 후 박스플롯 확인
par(mfrow = c(1,2))
boxplot(df[, 3:ncol(df)])
boxplot(df.rm.outlier[, 3:ncol(df)])

##############################################################################

# 1. k 군집 개수 설정 (Elbow method)
install.packages("factoextra")
library(factoextra)
set.seed(1234)
fviz_nbclust(df.rm.outlier[, 3:ncol(df.rm.outlier)], kmeans, method = "wss", k.max = 15) +
  theme_minimal() +
  ggtitle("Elbow Method")

# 2. k 군집 개수 설정 (Silhouette method)
fviz_nbclust(df.rm.outlier[, 3:ncol(df.rm.outlier)], kmeans, method = "silhouette", k.max = 15) +
  theme_minimal() +
  ggtitle("Elbow Method")

# 3. k means 모델 생성
df.kmeans <- kmeans(df.rm.outlier[, 3:ncol(df.rm.outlier)], centers = 5, iter.max = 1000)
df.kmeans

# 4. 군집별 평균치 시각화
barplot(t(df.kmeans$centers), beside = TRUE, col = 1:6)
legend("topleft", colnames(df[, 3:8]), fill = 1:6, cex = 0.5)

# 5. raw data에 cluster 할당
df.rm.outlier$cluster <- df.kmeans$cluster
head(df.rm.outlier)

##############################################################################

# 1. 데이터 확인
library(jpeg)
img <- readJPEG('/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/cat.jpeg')
class(img)
dim(img)

# 2. 3차원 데이터를 2차원으로 펼침
imgdim <- as.vector(dim(img))
imgRGB <- data.frame(
  x = rep(1:imgdim[2], each = imgdim[1]),
  y = rep(imgdim[1]:1, imgdim[2]),
  R = as.vector(img[, , 1]),
  G = as.vector(img[, , 2]),
  B = as.vector(img[, , 3])
)
head(imgRGB)
tail(imgRGB)

# 3. 색상 개수 축소
kClusters <- c(3, 5, 10, 15, 30, 50) # 축소할 색상 클러스터 개수
set.seed(1234)
for (i in kClusters) {
  img.kmeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = i)
  img.result <- img.kmeans$centers[img.kmeans$cluster,]
  img.array <- array(img.result, dim = imgdim)
  writeJPEG(img.array, paste('kmeans_', i, 'clusters.jpeg', sep = ''))
}

##############################################################################

# Hierarchical clustering: 가장 가까운 데이터끼리 순차적 (계층적)으로 묶어 나가는 군집화 기법

# Hierarchical clustering process:
# 1. 모든 데이터들 간의 거리 행렬 (유사도 행렬) 생성
  # - 유클리드 거리 -> 가장 많이 사용
  # - 맨하탄 거리
  # - Correlation 등
# 2. 군집을 구성할 방법 선택
  # - 최단거리법 (single)
  # - 최장거리법 (complete)
  # - 평균기준법 (average)
  # - 중앙중심법 (median)
  # - Ward's method
    # 군집을 확장했을 때, 추가되는 분산이 적은 군집끼리 묶어주는 방법
    # Ward's distance = AB 군집의 분산 - (A 군집의 분산 + B 군집의 분산)
# 3. 군집화

# 군집 개수의 설정: Dendrogram을 살펴보고 적절한 군집 개수 설정

# Hierarchical clustering의 활용:
# - K-means와 동일한 방식으로 활용 가능
# - 데이터가 계층적으로 유사한 특징을 가질 때 적합
  # ex) A와 B가 가장 가깝고, 그 다음으로 C와 가깝고...
# - 활용 방법:
  # - 고객 유형을 분류하여 상품 판매 전략 도출
  # - 제품의 성분 및 특성에 따라 분류하여 제품 추천 로직 (알고리즘) 개발

##############################################################################

# 1. 데이터 확인
df <- USArrests
head(df)

# 2. 결측치 확인
colSums(is.na(df))

# 3. 변수별 기술통계 및 분포 확인
summary(df)
dev.off() # 위에서 par(mfrow = c(1,2)) 했던 것 리셋
boxplot(df)

# 4. 표준화
library(dplyr)
df <- scale(df) %>% as.data.frame()
boxplot(df)

# 5. 이상치 제거
library(tibble)
df.rm.outlier <- df %>% rownames_to_column('rname') %>%
  arrange(desc(`Rape`)) %>% 
  slice(-1:-2) %>% 
  column_to_rownames('rname')
boxplot(df.rm.outlier)

##############################################################################

# 1. 유사도 행렬 생성 (유클리드 거리)
df.dist <- dist(df.rm.outlier, method = "euclidean")

# 2. 군집 구성 방식 선택
df.hclust.sing <- hclust(df.dist, method = "single")
df.hclust.cplt <- hclust(df.dist, method = "complete")
df.hclust.avg <- hclust(df.dist, method = "average")
df.hclust.cent <- hclust(df.dist, method = "centroid")
df.hclust.ward <- hclust(df.dist, method = "ward.D2")

# 3. dendrogram 생성 & 군집 시각화
plot(df.hclust.sing, cex = 0.6, hang = -1)
rect.hclust(df.hclust.sing, k=4, border = 2:5)

plot(df.hclust.cplt, cex = 0.6, hang = -1)
rect.hclust(df.hclust.cplt, k=4, border = 2:5)

plot(df.hclust.avg, cex = 0.6, hang = -1)
rect.hclust(df.hclust.avg, k=4, border = 2:5)

plot(df.hclust.cent, cex = 0.6, hang = -1)
rect.hclust(df.hclust.cent, k=4, border = 2:5)

plot(df.hclust.ward, cex = 0.6, hang = -1)
rect.hclust(df.hclust.ward, k=4, border = 2:5)

# 4. raw data에 cluster 할당
df.clusters <- cutree(df.hclust.ward, k = 4)
table(df.clusters)
df.rm.outlier$cluster <- df.clusters
head(df.rm.outlier)

# 5. 2차원 시각화
library(factoextra)
fviz_cluster(list(data = df.rm.outlier[,1:ncol(df.rm.outlier)-1], cluster = df.clusters))

# 6. 군집별 평균치 확인 및 시각화
library(reshape2)
temp <- df.rm.outlier %>% melt(id = 'cluster')
head(temp)
df.means <- dcast(temp, cluster ~ variable, mean)
df.means
barplot(t(df.means[,-1]), beside=TRUE, col = 1:4, names.arg = c(1:4))
legend("topright", colnames(df.rm.outlier[1:4]), fill = 1:4, cex = 0.5)
