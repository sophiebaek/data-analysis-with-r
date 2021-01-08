library(ggplot2)

ggplot(data = mpg, aes(x = displ, y = hwy)) + # x축 displ, y축 hwy로 지정해 배경 생성
  geom_point() + # 배경에 산점 추가
  xlim(3, 6) + # x축 범위 3~6으로 지정
  ylim(10, 30) # y축 범위 10~30으로 지정

#############################################################################

# Q1
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()
# Q2
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)

#############################################################################
# <평균 막대 그래프>
# 1. 집단별 평균표 만들기
library(dplyr)
library(ggplot2)

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg

# 2. 그래프 생성하기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

# 3. 크기 순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

# <빈도 막대 그래프>
# x축 범주 변수, y축 빈도
ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

#############################################################################

# Q1
mpg_copy <- mpg
mpg_copy <- mpg_copy %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(cty_mean = mean(cty)) %>% 
  arrange(desc(cty_mean)) %>% 
  head(5)
mpg_copy
ggplot(data = mpg_copy, aes(x = reorder(manufacturer, -cty_mean), y = cty_mean)) + geom_col()

# Q2
mpg_copy2 <- mpg
ggplot(data = mpg_copy2, aes(x = class)) + geom_bar()

#############################################################################

# 3. 선 그래프 - 시간에 따라 달라지는 데이터 표현하기
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

#############################################################################

# Q1
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

#############################################################################

# 4. 상자 그림 - 집단 간 분포 차이 표현하기
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

#############################################################################

# Q1
mpg_copy <- mpg
mpg_copy <- mpg_copy %>% filter(class %in% c("compact", "subcompact", "suv"))
ggplot(data = mpg_copy, aes(x = class, y = cty)) + geom_boxplot()

#############################################################################

# 결측치 만들기
# NA 앞 뒤에 겹따옴표 없음
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

# 결측치 확인하기
is.na(df) # 결측치 확인
table(is.na(df)) # 결측치 빈도 출력

# 변수별로 결측치 확인하기
table(is.na(df$sex)) # sex 결측치 빈도 출력
table(is.na(df$score)) # score 결측치 빈도 출력

# 결측치 포함된 상태로 분석
mean(df$score) # 평균 산출
sum(df$score) # 합계 산출

# 결측치 있는 행 제거하기
df %>% filter(is.na(score)) # score가 NA인 데이터만 출력
df %>% filter(!is.na(score)) # score 결측치 제거 

# 결측치 제외한 데이터로 분석하기
df_nomiss <- df %>% filter(!is.na(score)) # score 결측치 제거
mean(df_nomiss$score) # score 평균 산출
sum(df_nomiss$score) # score 합계 산출

# score, sex 결측치 제외
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

# 결측치가 하나라도 있으면 제거하기
# 하지만 이건 자주 사용하지 않음. 두 항목만 비교할때 다른 항목에 NA가 하나라도 있다면 다 날려버리기 때문에 데이터 손실이 큼.
df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2 # 출력

# 함수의 결측치 제외 기능 이용하기 - na.rm = T
# rm: remove
mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T) # 결측치 제외하고 합계 산출

# summarise()에서 na.rm = T 사용하기
# 결측치 생성
exam <- read.csv("/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/csv_exam.csv") # 데이터 불러오기
exam[c(3, 8, 15), "math"] <- NA # 3, 8, 15행의 math에 NA 할당
# 평균 구하기
exam %>% summarise(mean_math = mean(math)) # 평균 산출
exam %>% summarise(mean_math = mean(math, na.rm = T)) # 결측치 제외하고 평균 산출
# 다른 함수들에 적용
exam %>% summarise(mean_math = mean(math, na.rm = T), # 평균 산출
                   sum_math = sum(math, na.rm = T), # 합계 산출
                   median_math = median(math, na.rm = T)) # 중앙값 산출

# 평균값으로 결측치 구하기
# 평균 구하기
mean(exam$math, na.rm = T) # 결측치 제외하고 math 평균 산출
# 평균으로 대체하기
exam$math <- ifelse(is.na(exam$math), 55, exam$math) # math가 NA면 55로 대체
table(is.na(exam$math)) # 결측치 빈도표 생성
exam # 출력
mean(exam$math) # math 평균 산출

#############################################################################

mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA # NA 할당하기

# Q1
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2
mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(hwy_mean = mean(hwy)) # f 구동방식의 hwy 평균이 가장 높음.

#############################################################################

# 이상한 데이터를 찾아라! - 이상치 정제하기
# 이상치 제거하기 - 1. 존재할 수 없는 값: 논리적으로 존재할 수 없으므로 바로 결측 처리 후 분석시 제외
# 이상치 포함한 데이터 생성 - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
# 이상치 확인하기
table(outlier$sex)
table(outlier$score)
# 결측 처리하기 - sex
# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
# 결측 처리하기 - score
# score가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
# 결측치 제외하고 분석
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# 이상치 제거하기 - 2. 극단적인 값
# 상자그림으로 극단치 기준 정해서 제거하기
# 상자그림 생성
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
# 상자그림 통계치 출력
boxplot(mpg$hwy)$stats # 상자그림 통계치 출력
# 결측 처리하기
# 12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
# 결측치 제외하고 분석하기
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

#############################################################################

mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k" # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) # cty 이상치 할당

# Q1
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)
table(mpg$drv)

# Q2
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

# Q3
mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(cty_mean = mean(cty))

#############################################################################