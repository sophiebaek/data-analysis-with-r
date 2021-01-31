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

# Decision Tree에서 각 트리의 스코어링 방법: 노드별 무질서 측정 후 퀄리티 테스트
# 무질서 (disorder) 측정 공식 (엔트로피):
# D(set) = -P/T log_2 P/T - N/T log_2 N/T
  # T: 노드 내 전체 데이터 개수
  # P: Positive 데이터 갯수
  # N: Negative 데이터 갯수
# 무질서는 낮을 수록 좋음!
# 양성비율 = P/T -> 양성비율이 0과 1에 가까울수록 무질서가 낮다. / 1/2일때 무질서가 가장 높음

# 테스트 퀄리티 (작을 수록 좋음)
# Q(Test) = sigma D(sets) * 해당 노트 데이터 수 / 테스트 전체 데이터 수

# 타겟이 연속형인 경우 -> 평균값을 사용하여 예측

# Decision Tree의 단점: 오버피팅 -> 일반화 하기 어려움

# Random Forest:
# 1. n개의 랜덤 데이터 샘플 선택 (중복 가능) -> 배깅 (bagging): bootstrap aggregating
# 2. d개의 피쳐 선택 (중복 불가능)
# 3. Decision Tree 학습
# 4. 각 Decision Tree 결과의 투표를 통해 클라스 할당

##############################################################################

# Decision Tree 순서:
# 1. 패키지 설치
install.packages("tree")
library(tree)

# 2. 기본 트리
treeRaw <- tree(Class~., data = train)
plot(treeRaw)
text(treeRaw)

# 3. cross-validation -> 적절한 사이즈 결정
cv_tree <- cv.tree(treeRaw, FUN = prune.misclass)
# FUN -> 가지치기 함수 선택
# prune.missclass -> 오분류 기준
plot(cv_tree)

# 4. 가지치기 (pruning)
prune_tree <- prune.misclass(treeRaw, best = 4) # best = 4: cross-validation을 통해 구한 사이즈
plot(prune_tree)
text(prune_tree, pretty = 0) # pretty = 0: 분할 피쳐 이름을 바꾸지 않음

# 5. 예측
pred <- predict(prune_tree, test, type = 'class')
confusionMatrix(pred, test$Class)

##############################################################################

# Random Forest
# install.packages("caret", dependencies = TRUE)
library(caret)

ctrl <- trainControl(method = "repeatedcv", repeats = 5)
rfFit <- train(Class~.,
               data = train,
               method = "rf", # randomForest 사용
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")

##############################################################################

# Decision Tree에서 오른쪽이 TRUE (예), 왼쪽이 FALSE (아니오)
# Size = Depth

# Random Forest 결과에서 mtry: 각 트리에서 랜덤하게 선택되는 분할 피쳐 후보 갯수
  # mtry가 작을수록 tree 크기도 작아짐

##############################################################################

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

# install.packages("tree")
library(tree)

treeRaw <- tree(Class~., data = train)
plot(treeRaw)
text(treeRaw)

cv_tree <- cv.tree(treeRaw, FUN = prune.misclass)
plot(cv_tree)

prune_tree <- prune.misclass(treeRaw, best = 4)
plot(prune_tree)
text(prune_tree, pretty = 0)

pred <- predict(prune_tree, test, type = 'class')
confusionMatrix(pred, test$Class)

ctrl <- trainControl(method = "repeatedcv", repeats = 5)
rfFit <- train(Class~.,
               data = train,
               method = "rf", # randomForest 사용
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")
rfFit
plot(rfFit)

pred_test <- predict(rfFit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_rf <- varImp(rfFit, scale = FALSE)
plot(importance_rf)

##############################################################################

# Support Vector Machine
# 중심선과 경계선 사이 여백 -> margin (마진)
# w vector = (w_1, w_2) -> 중심선에 수직인 벡터
# norm = 벡터의 길이

# w vector @ x vector = c -> 중심선
# w vector @ x vector > c -> +
# w vector @ x vector < c -> -

# 결정 조건 (decision rule):
# w vector @ x vector + b > 0

# 영역 표시:
# w vector @ x vector_+ + b >= 1 -> +
# w vector @ x vector_- + b <= -1 -> -

# 중심선과 경계선 사이 표현
# 0 <= w vector @ x vector + b <= 1
# -1 <= w vector @ x vector + b <= 0

# 두개의 제약식을 하나로 합침: y_i(w vector @ x vector + b) - 1 >= 0

# 경계선에 걸쳐 있는 데이터 표현:
# y_i(w vector @ x vector + b) - 1 = 0

# 서포트 벡터 간 너비 (width)
# = (x vector_+ - x vector_-) @ w vector/norm(w vector)
# = 2 / norm(w vector)
# 서포트 벡터 간 너비가 클수록 좋은 모형
# 너비 최대화:
# max(2 / norm(w vector))
# = max(1 / norm(w vector))
# = min(norm(w vector))
# = min(1/2 norm(w vector)^2)
# = min(1/2(w vector @ w vector)) -> 벡터 w 자기 자신과의 내적값의 최소화

# 최적화 (Optimization):
# - 목적 함수 (objective function): min(1/2 norm(w vector)^2)
# - 제약 조건 (constraint): y_i(w vector @ x vector + b) - 1 = 0 -> 경계선/서포터벡터
# 제약조건 하에서 목적 함수의 최적화가 목표!

# 라그랑지안 (Lagrangian): 목적함수와 제약식을 한꺼번에 표현
# 라그랑주 듀얼 함수 (Lagrange Dual Function) -> 데이터 자신과의 내적에 달려있다

##############################################################################

# 선형 서포트 벡터 머신
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_linear_fit <- train(Class~.,
                        data = train,
                        method = "svmLinear",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")
svm_linear_fit

# 비선형 서포트 벡터 머신
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_poly_fit <- train(Class~.,
                        data = train,
                        method = "svmPoly",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")
svm_poly_fit

# caret의 kernlab 패키지 사용

# 공간이 변함에 따라 나를 나타내는 좌표가 바뀜
# 비선형 구분 -> 피쳐 공간 변형 후 선형 모형으로 구분 -> 종이를 펴면 비선형 모양! (원래 공간으로 돌아가면 비선형 모양)

##############################################################################

# 비선형 서포트 벡터 머신 결과 해석:
  # degree: polynomial degree -> 커널의 차수 설정
  # scale: 데이터의 스케일 -> 다항식의 파라미터 (parameter)를 스케일링
  # C: Cost (loss) -> 로지스틱에서 배운 Cost와 동일, 학습 모형의 비용 (cost) 설정, 경계선의 복잡성을 컨트롤한다.

##############################################################################

# 선형 서포트 벡터 머신
# install.packages("caret", dependencies = TRUE)
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
svm_linear_fit <- train(Class~.,
                        data = train,
                        method = "svmLinear",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")
svm_linear_fit

pred_test <- predict(svm_linear_fit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_linear <- varImp(svm_linear_fit, scale = FALSE)
plot(importance_linear)

# 비선형 서포트 벡터 머신
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_poly_fit <- train(Class~.,
                        data = train,
                        method = "svmPoly",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")
svm_poly_fit

plot(svm_poly_fit)

pred_test <- predict(svm_poly_fit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_poly <- varImp(svm_poly_fit, scale = FALSE)
plot(importance_poly)

# 비선형 SVM이 선형 SVM보다 overfitting이 심할 수도 있다. (항상 그렇다는 것은 아님)
