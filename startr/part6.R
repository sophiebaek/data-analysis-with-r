# 지도학습 (supervised learning): 정답이 있는 데이터 분석 / 라벨링이 되어있는 데이터를 분석
  # ex) classification
# 비지도학습 (unsupervised learning): 라벨링이 되어 있지 않음
  # ex) clustering

# 데이터 전처리:
# 단위가 다르면 비교하기가 힘들다 -> 그래서 스케일링이 필요
# 1. 표준화 (standardization) = (원 데이터 - 평균) / 표준편차
  # 평균 0, 표준편차 1
# 2. MinMax 스케일 = (원 데이터 - min(데이터)) / (max(데이터) - min(데이터))
  # 0과 1 사이로 데이터 위치 (음수값을 가질 수 없음)

# 모형 평가:
# - 범주형 종속변수: ROC 커브 (Receiver operating characteristics, 수신자 조작 특성)
  # ROC 커브: 민감도 vs False Positive Rate -> 좌측 상단으로 갈수록 좋은 모형
    # 곡선 아래의 면적 (AUC: Area Under Curve)이 넓을수록 좋은 모델 / 1에 가까울 수록 좋음
# - 연속형 종속변수: MSE (Mean Squared Error) 평균 제곱 오차

# True Positive: 양성으로 예측했는데 양성인 경우
# True Negative: 음성으로 예측했는데 음성인 경우
# False Positive (Type I Error): 양성으로 예측했는데 음성인 경우
# False Negative (Type II Error): 음성으로 예측했는데 양성인 경우

# 여러가지 평가 개념:
# - 정확도 (Accuracy) = (TP + TN) / (TP + FP + FN + TN) = 정답 데이터 / 전체 데이터
# - 에러율 (Error rate) = (FP + FN) / (TP + FP + FN + TN) = 오답 데이터  / 전체 데이터
# - 민감도 (Sensitivity, Recall) = TP / (TP + FN) = 예측양성, 실제양성 / 실제양성
# - 정밀도 (Precision) = TP / (TP + FP) = 예측양성, 실제양성 / 예측양성
# - False Positive Rate = FP / (FP + TN) = 예측양성, 실제음성 / 실제음성

# 오버피팅 (overfitting) <-> 언더피팅 (underfitting)

# 교차검증 (cross-validation):
# 모형 생성 시 사용했던 데이터는 정확도 측정에 사용하시면 안됩니다!
# Test - Validation - Train
# Validation에서 parameter 수정함
# 1. Leave-One-Out Cross-validation: training dataset 중에 하나씩 validation으로 사용 (n번 반복)
# 2. k-fold Cross-Validation: k개로 나눠서 k번 반복
# 3. stratified k-fold Cross-validation = 클래스 비율 맞춤

# 머신러닝 전체 과정 요약:
# 1. 전체 데이터를 트레이닝 데이터와 테스트 데이터로 나눈다.
# 2. 트레이닝 데이터를 트레이닝 데이터와 검증 데이터로 또 나눈다.
# 3. 트레이닝 데이터로 모형을 생성하고 검증 데이터로 parameter 설정을 한다.
# 4. 이 과정을 여러 번 반복하는 cross-validation을 실행함으로써 최종 모형을 만든다.
# 5. 최종 모형을 테스트 데이터를 사용하여 최종 모형 평가를 한다.

##############################################################################

# 지도학습 -> 분류 OR 예측
  # - 분류: ex) 고객, 상품, 특성, 게임 유저 분류
  # - 예측: ex) 판매량, 트래픽, 추천, 이탈예측모형

##############################################################################

# K-Nearest Neighbor: 가장 가까운 k개의 이웃 데이터를 보고 판단
# - 가장 가까이 있는 데이터 클래스에 속한다고 보는 방법
# - 유클리디안 거리를 사용하므로 피쳐는 연속형 변수이여야 함 (숫자이여야함).
# - 피쳐 정보 -> KNN -> 분류

##############################################################################

install.packages("caret", dependencies = TRUE)
library(caret)

# trainControl(): 데이터 훈련(train) 과정의 파라미터(parameter) 설정
# trainControl(
#  method = "repeatedcv", # cross-validation 반복
#  number = 10, # 훈련 데이터 fold 갯수
#  repeats = 5 # cv 반복 횟수
#)

# expand.grid(): 모든 백터 혹은 인자(factor) 조합인 데이터 프레임 생성
# expand.grid(k = 1:10)

# train(): 머신러닝 알고리즘 이용해 테이터 학습을 통한 모델 생성
# train(
#  Class~.,
#  data = train,
#  method = "knn", # 사용하고 싶은 머신러닝 방법
#  trControl = trainControl(), # 학습 방법
#  preProcess = c("center", "scale"), # 표준화(standardization): 평균을 빼고 표준편차로 나눔
#  tuneGrid = expand.grid(k = 1:10), # 튜닝 파라미터 값 목록
#  metric = "Accuracy" # 모형 평가 방식
#)

##############################################################################

# Accuracy VS Kappa 통계량:
# Kappa 통계량 = (p_0 - p_e) / (1 - p_e)
  # p_0: 관측된 정확도 = Accuracy
  # p_e: 기대 정확도
# Accuracy: 0 < 정확도 < 1 / 1에 가까울 수록 좋음
# Kappa 통계량: -1 < 정확도 < 1 / 1에 가까울 수록 좋음

##############################################################################

rawdata <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/wine.csv", header = TRUE)
rawdata$Class <- as.factor(rawdata$Class) # 숫자가 아닌 factor로 인식하게 만들기
str(rawdata)

# 트레이닝-테스트 데이터 분할
analdata <- rawdata

set.seed(2020) # 시드설정: 랜덤으로 뽑지만 일정하게 (숫자 상관 없음)
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*0.7)) # training data 뽑기
# sample(a, b): 1부터 a까지 숫자 중에 b개 추출
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]

train_x <- train[, 1:13]
train_y <- train[, 14]

test_x <- test[, 1:13]
test_y <- test[, 14]

# 모형학습
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
customGrid <- expand.grid(k = 1:10)
knnFit <- train(Class~.,
                data = train,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center", "scale"),
                tuneGrid = customGrid,
                metric = "Accuracy")
knnFit
plot(knnFit)

# 예측
pred_test <- predict(knnFit, newdata = test)
confusionMatrix(pred_test, test$Class)

# 변수중요도 (여러가지 피쳐 중에 어떤 것이 중요할까?)
importance_knn <- varImp(knnFit, scale = FALSE)
importance_knn
plot(importance_knn)

##############################################################################

# Logistic Regression
# 종속변수 값에 제한 있음 -> 가능한 범위 -> 가질 수 없는 값이 존재
# 종속변수 = 범주형, 연속형 (가능하긴 하지만 보통 주로 범주형)

# 로지스틱 회귀분석 탄생 배경
# y (시그모이드 함수) = 1 / (1 + e^-Z) = 1 / (1 + e^-(alpha + bata x))
# log(y / (1 - y)) = alpha + beta x -> log base: e (= ln)
# y -> pi(x) = p(Y = 1|X = x)
# logit(pi(x)) = log(pi(x) / (1 - pi(x))) = alpha + beta x -> log안에 있는 것: 오즈비 (odds ratio) = 양성 확률은 음성 확률의 몇배인가?
# 기울기가 가장 가파른 곳: x = -alpha / beta

##############################################################################

library(caret)

# LogitBoost
# ctrl <- trainControl(method = "repeatedcv", repeats = 5)
# logitFit <- train(target~.,
#                   data = train,
#                   method = "LogitBoost",
#                   trControl = ctrl,
#                   metric = "Accuracy")
# logitFit

# 로지스틱 회귀분석:
# 1. Boosted Logistic Regression: 약한 분류기 여러개를 계속 더하여 강하게 만듦
  # method = 'LogitBoost'
# 2. Logistic Model Trees: 로지스틱 + 나무
  # method = 'LMT'
# 3. Penalized Logistic Regression
  # method = 'plr'
# 4. Regularized Losgistic Regression
  # method = 'regLogistic'

rawdata <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/heart.csv", header = TRUE)
str(rawdata)

# 타켓 클래스 범주화
rawdata$target <- as.factor(rawdata$target)
unique(rawdata$target)

# 연속형 독립변수 표준화
rawdata$age <- scale(rawdata$age)
rawdata$trestbps <- scale(rawdata$trestbps)
rawdata$chol <- scale(rawdata$chol)
rawdata$thalach <- scale(rawdata$thalach)
rawdata$oldpeak <- scale(rawdata$oldpeak)
rawdata$slope <- scale(rawdata$slope)

# 범주형 독립변수
newdata <- rawdata
factorVar <- c("sex", "cp", "fbs", "restecg", "exang", "ca", "thal")
newdata[, factorVar] = lapply(newdata[, factorVar], factor)

# 트레이닝 테스트 나누기 (7:3)
set.seed(2020)
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*0.7))
train <- newdata[datatotal,]
test <- newdata[-datatotal,]

train_x <- train[, 1:12]
train_y <- train[, 13]

test_x <- test[, 1:12]
test_y <- test[, 13]

# LogitBoost
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
logitFit <- train(target~.,
                  data = train,
                  method = "LogitBoost",
                  trControl = ctrl,
                  metric = "Accuracy")
logitFit
plot(logitFit)

pred_test <- predict(logitFit, newdata = test)
confusionMatrix(pred_test, test$target)

importance_logit <- varImp(logitFit, scale = FALSE)
plot(importance_logit)

##############################################################################

# Naive Bayes Classification
# 베이즈 정리: P(X|Y) = P(X,Y) / P(Y)
  # P(X,Y) = P(X|Y)P(Y) = P(Y|X)P(X)
  # P(X|Y) = P(Y|X)P(X) / P(Y)

# P(X|Y) = P(Y|X)P(X) / P(Y):
  # P(X) -> 사전확률 (Prior Probability): 이벤트 발생 전 확률
  # P(X|Y) -> 사후확률 (Posterior Probability): 이벤트 발생 후 확률
  # P(X|Y)는 P(Y|X)P(X)에 비례
  # P(Y)는 확률변수가 아닌 상수로 고려! (조건이 주어졌다 (given))

# 피쳐들은 서로 조건부독립 (conditional independent)
# 조건부독립: P(X1,X2|Y) = P(X1|Y)P(X2|Y)

##############################################################################

# ctrl <- trainControl(method = "repeatedcv", repeats = 5)
# nbFit <- train(Class~.,
#                data = train,
#                method = "naive_bayes",
#                trControl = ctrl,
#                preProcess = c("center", "scale"),
#                metric = "Accuracy")
# nbFit

# 나이브 베이즈를 제공하는 패키지들:
# - naivebayes 패키지: method = 'naive_bayes' (추천)
# - bnclassify 패키지:
  # method = 'nbDiscrete'
  # method = 'manb' -> Model Averaged Naive Bayes: 모든 가능한 피쳐조합의 조건부확률의 평균
  # method = 'awnb' -> Attribute Weighting: 조건부 확률에 가중치 부여
# - klaR 패키지: method = 'nb'

# 나이브 베이즈 결과:
# - usekernel
  # 커널밀도추정 (Kernel Density Estimation): 데이터의 히스토그램을 보고 실제 분포를 추정
    # smoothing
# - adjust
  # Bandwidth: Bandwidth 값이 달라지면 추정 커널밀도함수 형태가 달라진다
  # adjust -> bandwidth를 조절한다는 뜻
# - laplace
  # 라플라스 스무딩 (Laplace Smoothing or Additive Smoothing): 데이터 수가 적을 경우, 0 또는 1과 같이 극단적인 값 (0 또는 1)으로 추정하는 것 방지

library(caret)

rawdata <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/wine.csv", header = TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

analdata <- rawdata

set.seed(2020)
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*0.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]

str(train)

train_x <- train[, 1:13]
train_y <- train[, 14]

test_x <- test[, 1:13]
test_y <- test[, 14]

ctrl <- trainControl(method = "repeatedcv", repeats = 5)
nbFit <- train(Class~.,
               data = train,
               method = "naive_bayes",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")
nbFit
plot(nbFit)

pred_test <- predict(nbFit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_nb <- varImp(nbFit, scale = FALSE)
plot(importance_nb) # ROC 커브의 면적이 넓을 수록 중요도 상승

##############################################################################

# Decision Tree
# Node
  # 루트 노드 (Root node) / 부모 노드
  # 리프 노드 (Leaf node) / 자식 노드
# 테스트의 결과는 또다른 테스트가 될 수도 있다

# 좋은 Decision Tree란?: 가능한 한 가장 작은 나무
