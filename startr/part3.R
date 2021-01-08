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
