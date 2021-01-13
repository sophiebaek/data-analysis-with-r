# 가설설정:
# - 귀무가설: ex) 두 집단 수입 차이 없다.
  # 기존에 존재하던 가설 /  차이가 없거나 영향도 없음 / 분석가의 주장과 반대 가설
# - 대립가설: ex) 노랑 집단 수입이 파랑 집단 보다 높다.
  # 분석가가 새롭게 제시한 가설 / 차이가 있거나 영향도 있음 / 분석가가 채택하고 싶은 가설
    # - 양측 검정: ex) 두 집단 수입 차이가 있다.
      # ex) 파랑색 수입 > 노랑색 수입 / 파랑색 수입 < 노랑색 수입
    # - 단측 검정: ex) 노랑 집단 수입이 파랑 집단 보다 높다. (대게 단측 검정 사용)
      # ex) 파랑색 수입 < 노랑색 수입

# 가설검정 순서:
# 1. 가설을 세운다
# 2. 기준을 세운다 (검정통계량을 구한다)
# 3. 결론을 내린다 (p-value 참고)

##############################################################################

# 두 집단 평균 차이 검정
# 모집단: 관심 대상 전체 집합 (모집단 != 모수)
# 표본: 모집단의 부분 집합

# T-test 하기전, 정규성 검정, 분산 동질성 검정 필요
# 가설 설정 -> 데이터 정규성 검정 -> 분산 동질성 검정 -> T-test -> 결론

# T-test를 위한 검정통계량 구하기
# T값 = (그룹1 평균 - 그룹2 평균) / 표준편차

##############################################################################

rawN3 <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest01.csv", header = T)

groupA <- rawN3[rawN3$group == 'A', 1:2]
groupB <- rawN3[rawN3$group == 'B', 1:2] # $: 데이터 프레임의 특정 열에 접근할 때 사용

# 각 집단의 평균 구해서 비교하기
mean(groupA[,2])
mean(groupB[,2]) # B > A

# T-test 과정
# 1. 가설검정
  # 귀무가설: 그룹A, 그룹B간 평균 키 차이 없다.
  # 대립가설: 그룹B의 평균 키가 그룹A의 평균 키보다 크다. (단측검정)
# 2. 데이터 정규성 검정
  # 귀무가설: 데이터셋이 정규분포를 따른다.
  # 대립가설: 데이터셋이 정규분포를 따르지 않는다.
  # 각 집단 샘플 사이즈 = 3 (정규성 검정: 집단 A)
shapiro.test(groupA[,2]) # p-value = 1 > 0.05 (귀무가설 채택)
# qq plot
qqnorm(groupA[,2])
qqline(groupA[,2]) # 집단 A는 정규성을 따른다.
  # 각 집단 샘플 사이즈 = 3 (정규성 검정: 집단 B)
shapiro.test(groupB[,2]) # p-value = 0.4173 > 0.05 (귀무가설 채택)
# qq plot
qqnorm(groupB[,2])
qqline(groupB[,2]) # 집단 B도 정규성을 따른다.
# 3. 분산 동질성 검정
  # 귀무가설: 두 집단 간 분산이 동일하다. (차이 X)
  # 대립가설: 두 집단 간 분산이 다르다. (차이 O)
var.test(groupA[,2], groupB[,2]) # p-value = 0.5385 (귀무가설 채택)
# 4. T-test (합동분산 사용: 두 집단간의 분산이 같기 때문)
t.test(groupA[,2], groupB[,2], alternative = "less", var.equal =  TRUE) # alternative: 왼쪽 값이 오른쪽 값보다 작다? 크다? # p-value = 0.1154 > 0.05 (귀무가설 채택)
# 5. 결론: 그룹A, 그룹B 간 평균 키 차이 없다.

##############################################################################

rawN10 <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest02.csv", header = T)

groupA2 <- rawN10[rawN10$group == 'A', 1:2]
groupB2 <- rawN10[rawN10$group == 'B', 1:2]

# 각 집단의 평균 구해서 비교하기
mean(groupA2[,2])
mean(groupB2[,2])

# 데이터 정규성 검정
shapiro.test(groupA2[,2]) # p-value = 0.2826 > 0.05 (귀무가설 채택)

qqnorm(groupA2[,2])
qqline(groupA2[,2])

shapiro.test(groupB2[,2]) # p-value = 0.9108 > 0.05 (귀무가설 채택)

qqnorm(groupB2[,2])
qqline(groupB2[,2])

# 분산 동질성 검정
var.test(groupA2[,2], groupB2[,2]) # p-value = 0.02602 < 0.05 (대립가설 채택)

# T-test
t.test(groupA2[,2], groupB2[,2], alternative = "less", var.equal = FALSE) # p-value = 0.01912 < 0.05 (대립가설 채택)

# 결론: 그룹B의 평균키가 그룹A의 평균 키보다 크다.

# 단순한 평균 비교로는 판단하기 어렵다.
# 데이터 수, 평균 파이, 분산 차이 등을 고려해야함.

##############################################################################

# 대응표본: 두 집단이 있을때, 같은 사람이 두 집단에 모두 속해 있는 경우
# 대응표본t검정: 두 집단을 비교하기 보단, 차이의 분포를 고려

raw_d <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest02d.csv", header = T)

groupAd <- raw_d[,1]
groupBd <- raw_d[,2]

# 각 집단의 평균 구해서 비교하기
mean(groupAd)
mean(groupBd)

# 귀무가설: 마케팅을 통해 판매액 변화 없음.
# 대립가설: 마케팅을 통해 팬매액이 증가함.

# 대응표본 T-test 과정에서는 분산 동질성 검정을 하지 않고 데이터 정규성 검정 이후 바로 T-test 진행
d = groupAd - groupBd

shapiro.test(d) # p-value = 0.1621 > 0.05 (귀무가설 채택)
qqnorm(d)
qqline(d)

t.test(groupAd, groupBd, alternative = "less", paired = TRUE) # p-value = 0.006745 < 0.05 (대립가설 채택)

# 결론: 마케팅을 통해 판매액이 증가함

##############################################################################

# z 검정 (데이터 개수 > 30 -> 대표본일때)
# 대표본 Z-test 과정:
# 1. 가설설정
# 2. Z-test
# 3. 결론
# 대표본이기 때문에 정규분포를 따름
# t분포의 꼬리가 더 두터움.

z.test <- function(x1, x2){
  
  n_x1 = length(x1)
  n_x2 = length(x2)
  
  mean_x1 = mean(x1)
  mean_x2 = mean(x2)
  
  cat("\n")
  cat("\tTwo Sample z-test\n")
  cat("\n")
  cat("mean of x1:", mean_x1, "\n")
  cat("mean of x2:", mean_x2, "\n")
  
  var_x1 = var(x1)
  var_x2 = var(x2)
  
  z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
  abs_z = abs(z)
  cat("z =", abs_z, "\n")
  
  p_value = 1-pnorm(abs_z)
  cat("p-value =", p_value)
}

rawN30 <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest03.csv", header = TRUE)

groupA3 <- rawN30[rawN30$group == 'A', 1:2]
groupB3 <- rawN30[rawN30$group == 'B', 1:2]

mean(groupA3[,2])
mean(groupB3[,2])

# 귀무가설: 그룹A, 그룹B 간 평균 키 차이 없다.
# 대립가설: 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

z.test(groupA3[,2], groupB3[,2]) # p-value = 0.04866272 < 0.05 (대립가설 채택)

# 결론: 그룹B의 평균 키가 그룹A의 평균 키보다 크다.

##############################################################################

# 여러 집단 평균 차이 검정

# 총 오차 = 집단 간 오차 + 집단 내 오차
# 집단 내 오차 = 집단 내 데이터들의 오차제곱의 합 = 시그마(각 데이터 값 - 해당집단 평균)^2
# 집단 간 오차 = 집단 간 데이터평균 오차제곱의 합
# 집단 간 평균 오차 = 시그마 (집단 내 데이터 개수) *(각 집단 평균 - 전체 평균)^2

# '집단 간 오차 >  집단 내 오차'이면 집단 간 평균 차이 존재

raw_anova <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest04.csv", header = TRUE)

groupA4 <- raw_anova[raw_anova$group == 'A', 1:2]
groupB4 <- raw_anova[raw_anova$group == 'B', 1:2]
groupC4 <- raw_anova[raw_anova$group == 'C', 1:2]

mean(groupA4[,2])
mean(groupB4[,2])
mean(groupC4[,2])

# ANOVA 테스트 과정:
# 1. 가설검정
# 2. 정규성 검정
# 3. 분산 동질성 검정
# 4. ANOVA 테스트
# 5. 결론

# 귀무가설: 세 집단간 평균 차이가 없다.
# 대립가설: 세 집단간 평균 차이가 있다. (양측 검정)

# 정규성 검정
shapiro.test(groupA4[,2]) # p-value = 0.8978 > 0.05 (귀무가설 채택)

qqnorm(groupA4[,2])
qqline(groupA4[,2])

shapiro.test(groupB4[,2]) # p-value = 0.9108 > 0.05 (귀무가설 채택)

qqnorm(groupB4[,2])
qqline(groupB4[,2])

shapiro.test(groupC4[,2]) # p-value = 0.6313 > 0.05 (귀무가설 채택)

qqnorm(groupC4[,2])
qqline(groupC4[,2])

# 분산 동질성 검정: levene 테스트 & bartlett 테스트
install.packages("lawstat")
library(lawstat)

levene.test(raw_anova$height, raw_anova$group) # p-value = 0.3298 > 0.05 (귀무가설 채택)
bartlett.test(height~group, data = raw_anova) # p-value = 0.3435 > 0.05 (귀무가설 채택)

# ANOVA 테스트
rawAnova <- aov(height~group, data = raw_anova)
summary(rawAnova) # p-value = 1.14e-05 < 0.05 (대립가설 채택)
# ANOVA 테이블에서 (group, Mean Sq)은 그룹 간 오차, (Residuals, Mean Sq)는 그룹 내 오차를 나타냄.

##############################################################################

# 분할표를 이용한 연관성 분석

# 귀무가설: 흡연여부와 폐암유무는 연관성이 없다.
# 대립가설: 흡연여부와 폐암유무는 연관성이 있다.

# 카이제곱 통계량 = 시그마(관측값 - 기대값)^2 / 기대값

raw_chisq <- read.csv(file = "/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/htest05.csv", header = TRUE)
rawTable <- table(raw_chisq)
rawTable

# 귀무가설: 흡연여부와 폐암유무는 연관성이 없다.
# 대립가설: 흡연여부와 폐암유무는 연관성이 있다.

chisq.test(rawTable, correct = FALSE) # p-value = 0.02686 < 0.05 (대립가설 채택)
# correct: 셀 기대도수 > 5 인 경우 -> FALSE / 셀 기대도수 < 5 인 경우 -> TRUE
