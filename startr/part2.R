history <- c(90, 80, 60, 70) # 역사점수 생성
history
math <- c(50, 60, 100, 20) # 수학점수 생성
math

# 변수 합해서 데이터프레임 만들기 
df_midterm <- data.frame(history, math)
df_midterm

# 반 추가하기
class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(history, math, class)
df_midterm

mean(df_midterm$history)
mean(df_midterm$math)

#############################################################################

# readxl 패키지 설치
install.packages("readxl")
library(readxl)

# 엑셀 파일 불러오기
df_finalexam <- read_excel("finalexam.xlsx", sheet = 1, col_names = T)
df_finalexam

mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)


read.csv("/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/csv_exam.csv", header = T)
# csv로 저장
write.csv(df_finalexam, file = "output_newdata.csv")

exam <- read.csv("/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/csv_exam.csv")
head(exam) # 앞에서부터 6행까지 출력
head(exam, 10) # 앞에서부터 10행까지 출력
tail(exam) # 뒤에서부터 6행까지 출력
tail(exam, 10) # 뒤에서부터 10행까지 출력
View(exam)
dim(exam) # 행, 열 출력
str(exam) # 데이터 속성 확인 (structure)
summary(exam) # 요약통계량 출력

# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)
mpg
head(mpg)
head(mpg, 10)
tail(mpg)
tail(mpg, 10)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)

install.packages("dplyr")
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_new <- df_raw # 복사본 생성
df_new # 출력
df_new <- rename(df_new, v2 = var2) # var2를 v2로 수정
df_new

#############################################################################

# mpg의 cty를 city로, hwy를 highway로 rename 해보기
mpg <- as.data.frame(ggplot2::mpg)
mpg_copy <- mpg
mpg_copy <- rename(mpg_copy, city = cty)
mpg_copy <- rename(mpg_copy, highway = hwy)
mpg_copy

#############################################################################

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df
df$var_sum <- df$var1 + df$var2 # var_sum 파생변수 생성
df
df$mean <- df$var_sum / 2
df

mpg$total <- (mpg$cty + mpg$hwy)/2 # 통합 연비 변수 생성
head(mpg)
mean(mpg$total)
summary(mpg$total) # 요약 통계량 산출
hist(mpg$total) # 히스토그램 생성
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
mpg$test
head(mpg, 20)
table(mpg$test) # 연비 합격 빈도표 생성
library(ggplot2) # ggplot2 로드
qplot(mpg$test) # 연비 합격 빈도 막대 그래프 생성

# total을 기준으로 A, B, C등급 부여
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
head(mpg, 20) # 데이터 확인
table(mpg$grade)
qplot(mpg$grade)

# A, B, C, D등급 부여
mpg$garde2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))

############################################################################

# 문제 1
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
dim(midwest)
# 문제 2
midwest_copy <- midwest
midwest_copy <- rename(midwest_copy, total = poptotal)
midwest_copy <- rename(midwest_copy, asian = popasian)
midwest_copy
# 문제 3
midwest_copy$asian_percentage <- midwest_copy$asian / midwest_copy$total * 100
midwest_copy
hist(midwest_copy$asian_percentage)
# 문제 4
asian_mean <- mean(midwest_copy$asian_percentage)
asian_mean
midwest_copy$asian_size <- ifelse(midwest_copy$asian_percentage > asian_mean, "large", "small")
midwest_copy
# 문제 5
table(midwest_copy$asian_size)
qplot(midwest_copy$asian_size)

############################################################################

exam <- read.csv("csv_exam.csv")
exam

# exam에서 class가 1인 경우만 추출하여 출력
exam %>% filter(class == 1) # %>%: control + shift + m
exam %>% filter(class == 2)
exam %>% filter(class != 1) # 1반 아닌 경우
exam %>% filter(class != 3) # 3반 아닌 경우

# 수학 점수가 50점을 초과한 경우
exam %>% filter(math > 50)
# 수학 점수가 50점 미만인 경우
exam %>% filter(math < 50)
# 영어 점수가 80점 이상인 경우
exam %>% filter(english >= 80)
# 영어 점수가 80점 이하인 경우
exam %>% filter(english <= 80)
# 1반이면서 수학 점수가 50점 이상인 경우
exam %>% filter(class == 1 & math >= 50)
# 2반이면서 수학 점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)
# 수학 점수가 90점 이상이거나 영어 점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)
# 영어 점수가 90 미만이거나 과학 점수가 50 미만인 경우
exam %>% filter(english < 90 | science < 50)
# 1, 3, 5반에 해당되면 추출
exam %>% filter(class == 1 | class == 3 | class ==5)
exam %>% filter(class %in% c(1, 3, 5)) # 간결 버전 (%in%: matching operation)

class1 <- exam %>% filter(class == 1) # class가 1인 행 추출, class1에 할당
class2 <- exam %>% filter(class == 2) # class가 2인 행 추출, class2에 할당
mean(class1$math) # 1반 수학 점수 평균 구하기
mean(class2$math) # 2반 수학 점수 평균 구하기

#############################################################################

# Q1
displ_less_equal_4 <- mpg %>% filter(displ <= 4)
displ_more_eqaul_5 <- mpg %>% filter(displ >= 5)
mean(displ_less_equal_4$hwy) # higher hwy
mean(displ_more_eqaul_5$hwy)
# Q2
audi <- mpg %>% filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")
mean(audi$cty)
mean(toyota$cty) # higher cty
# Q3
chevrolet <- mpg %>% filter(manufacturer == "chevrolet")
ford <- mpg %>% filter(manufacturer == "ford")
honda <- mpg %>% filter(manufacturer == "honda")
chevrolet_hwy_mean <- mean(chevrolet$hwy)
chevrolet_hwy_mean
ford_hwy_mean <- mean(ford$hwy)
ford_hwy_mean
honda_hwy_mean <- mean(honda$hwy)
honda_hwy_mean
total_mean <- (chevrolet_hwy_mean + ford_hwy_mean + honda_hwy_mean) / 3
total_mean

############################################################################

exam %>% select(math) # math 추출
exam %>% select(english) # english 추출
exam %>% select(class, math, english) # class, math, english 추출
exam %>% select(-math) # math 제외
exam %>% select(-math, -english) # math, english 제외

# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english)
exam %>%
  filter(class == 1) %>% # class가 1인 행 추출
  select(english) # english 추출 

exam %>%
  select(id, math) %>% # id, math 추출 
  head(10) # 앞부분 10행까지 추출

############################################################################

# Q1
install.packages("ggplot2")
mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg %>% select(class, cty)
head(mpg_new, 10)
# Q2
suv <- mpg_new %>% filter(class == "suv")
compact <- mpg_new %>% filter(class == "compact")
suv_cty_mean <- mean(suv$cty)
suv_cty_mean
compact_cty_mean <- mean(compact$cty)
compact_cty_mean # higher cty

############################################################################

exam %>% arrange(math) # math 오름차순 정렬
exam %>% arrange(desc(math)) # math 내림차순 정렬
exam %>% arrange(class, math) # class 및 math 오름차순 정렬

############################################################################

audi <- mpg %>% filter(manufacturer == "audi")
audi <- audi %>% arrange(desc(hwy))
head(audi, 5)

############################################################################

exam %>% 
  mutate(total = math + english + science) %>% # 총합 변수 추가 
  head # 일부 추출

exam %>% 
  mutate(total = math + english + science, # 총합 변수 추가 
         mean = (math + english + science)/3) %>% # 총평균 변수 추가 
  head # 일부 추출

exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head

############################################################################

# Q1
mpg_copy <- mpg
mpg_copy <- mpg_copy %>% mutate(sum = cty + hwy)
# Q2
mpg_copy <- mpg_copy %>% mutate(avg = (cty + hwy) / 2)
# Q3
mpg_copy %>% arrange(desc(avg)) %>% head(3)
# Q4
mpg %>% 
  mutate(sum = cty + hwy,
               avg = (cty + hwy) / 2) %>% 
  arrange(desc(avg)) %>% 
  head(3)

############################################################################

exam %>% summarise(mean_meath = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math), # math 평균
            sum_math = sum(math), # math 합계
            median_math = median(math), # math 중앙값 
            n = n()) # 학생 수

mpg %>% 
  group_by(manufacturer, drv) %>% # 회사별, 구방방식별 분리
  summarise(mean_cty = mean(cty)) %>% # cty 평균 산출
  head(10) # 일부 출력

############################################################################

library(dplyr)
# Q1
mpg %>% 
  group_by(class) %>% 
  summarise(cty_mean = mean(cty))

# Q2
mpg %>% 
  group_by(class) %>% 
  summarise(cty_mean = mean(cty)) %>% 
  arrange(desc(cty_mean))

# Q3
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(hwy_mean = mean(hwy)) %>% 
  arrange(desc(hwy_mean)) %>% 
  head(3)

# Q4
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(num_compact = n()) %>% 
  arrange(desc(num_compact))

#############################################################################

# 가로로 합치기
# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1 
test2
total <- left_join(test1, test2, by = "id") # id 기준으로 합쳐 total에 할당
total # total 출력

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new

# 세로로 합치기
# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
# 학생 6~10번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b
group_all <- bind_rows(group_a, group_b) # 데이터 합쳐서 group_all에 할당
group_all # group_all 출력

#############################################################################

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

# Q1
mpg <- left_join(mpg, fuel, by = "fl")
mpg

# Q2
mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)

#############################################################################

# Q1
midwest <- as.data.frame(ggplot2::midwest)
midwest_copy <- midwest
midwest_copy <- midwest_copy %>% mutate(popteenagers_ratio = (poptotal - popadults) / poptotal * 100)
midwest_copy

# Q2
midwest_copy %>% 
  group_by(county) %>% 
  summarise(popteenagers_ratio_mean = mean(popteenagers_ratio)) %>% 
  arrange(desc(popteenagers_ratio_mean)) %>% 
  select(county, popteenagers_ratio_mean) %>% 
  head(5)

# Q3
midwest_copy <- midwest_copy %>% mutate(popteenagers_size = ifelse(popteenagers_ratio < 30, "small",
                                                   ifelse(popteenagers_ratio < 40, "middle", "large")))
table(midwest_copy$popteenagers_size)

# Q4
midwest_copy <- midwest_copy %>% mutate(popasian_ratio = popasian / poptotal * 100)
midwest_copy
midwest_copy %>% 
  arrange(popasian_ratio) %>% 
  select(state, county, popasian_ratio) %>% 
  head(10)
