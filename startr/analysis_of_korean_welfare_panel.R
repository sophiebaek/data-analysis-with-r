# foreign 패키지 설치
install.packages("foreign")

# 패키지 로드
library(foreign)
library(dplyr)
library(ggplot2)

# 데이터 불러오기
# 복지패널데이터 로드
raw_welfare <- read.spss("/Users/jiyeonbaek/Documents/Jiyeon\ Baek/Projects/data-analysis-with-r/startr/data/data_spss_Koweps2014.sav", to.data.frame = T)

# 데이터 copy
welfare <- raw_welfare

# 데이터 검토
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)

# 변수명
welfare <- rename(welfare,
                  sex = h0901_4, # 성별
                  birth = h0901_5, # 태어난 연도
                  income = h09_din) # 소득

#############################################################################

# 분석1: 성별에 따른 소득
# 절차:
# 1. 변수 검토 및 정제 - 성별
# 2. 변수 검토 및 정제 - 소득
# 3. 성별 소득 평균 분석

# 1. 변수 검토 및 정제 - 성별
class(welfare$sex)
summary(welfare$sex)
# 이상치 확인
table(welfare$sex)
# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
# 결측치 확인
table(is.na(welfare$sex)) # 현재 결측치 없음
# 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 2. 변수 검토 및 정제 - 소득
class(welfare$income)
summary(welfare$income)
qplot(welfare$income) # 보기 힘듬
qplot(welfare$income) + xlim(0, 10000) # x축 설정 (수정 후)
table(is.na(welfare$income)) # 소득 이상치: 모름/무응답 없음

# 3. 성별 소득 평균 분석
# 성별 소득 평균표 생성
sex_income <- welfare %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income
# 그래프 생성
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

#############################################################################

# 분석2: 나이와 소득의 관계
# 절차:
# 1. 변수 검토 및 정제 - 나이
# 2. 변수 검토 및 정제 - 소득 (앞 분석 과정에서 이미 전처리를 완료함)
# 3. 나이별 소득 평균 분석

# 1. 변수 검토 및 정제 - 나이
class(welfare$birth)
# 이상치 확인
summary(welfare$birth)
qplot(welfare$birth)
# 이상치 결측처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
# 결측치 확인
table(is.na(welfare$birth)) # 현재 결측치 없음
# 나이 변수 생성
welfare$age <- 2014 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# 2. 변수 검토 및 정제 - 소득 (앞 분석 과정에서 이미 전처리를 완료함)

# 3. 나이별 소득 평균 분석
# 나이별 소득 평균 분석
age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
age_income
# 그래프 생성 - 산점도
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_point()

##############################################################################

# 분석3: 연령대에 따른 소득
# 절차:
# 1. 변수 검토 및 정제 - 연령대
# 2. 변수 검토 및 정제 - 소득 (앞 분석 과정에서 이미 전처리를 완료함)
# 3. 연령대별 소득 평균 분석

# 1. 변수 검토 및 정제 - 연령대
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

# 2. 변수 검토 및 정제 - 소득 (앞 분석 과정에서 이미 전처리를 완료함)

# 3. 연령대별 소득 평균 분석
# 연령대별 소득 평균표 생성 (초년 빈도 적으므로 제외)
welfare_income <- welfare %>% 
  filter(ageg != "young") %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
welfare_income
# 그래프 만들기
ggplot(data = welfare_income, aes(x = ageg, y = mean_income)) + geom_col()

##############################################################################

# 분석4: 연령대 및 성별에 따른 소득
# 절차:
# 1. 연령대 및 성별 소득 평균표 생성
# 2. 그래프 만들기

# 1. 연령대 및 성별 소득 평균표 생성 (초년 제외)
sex_income <- welfare %>% 
  filter(ageg != "young") %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

# 2. 그래프 만들기
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col()
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position = "dodge") # position 변경 (기본값 = "stack")
