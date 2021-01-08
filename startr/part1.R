install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

head(mpg)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)

mpg %>%
  group_by(manufacturer) %>%
  summarise(mean.hwy=mean(hwy)) %>%
  arrange(desc(mean.hwy))

mpg %>%
  filter(manufacturer=="ford") %>%
  group_by(model) %>%
  arrange(desc(hwy))

lm.mpg <- lm(data=mpg, hwy ~ displ)
summary(lm.mpg)

qplot(data = mpg, x = displ, y = hwy)

mean(mpg$hwy)
max(mpg$hwy)
min(mpg$hwy)

a <- 1
a
b <- 2
b
c <- 3
c
ab <- 3.5
ab

d <- c(1,2,3,4,5)
d
e <- c(1:5)
e
f <- seq(1, 5)
f
g <- seq(1, 10, by=2) # 1~10 까지 2씩 증가
g
d+2
a2 <- "a"
a2
b2 <- "text"
b2
c2 <- "Hello world!"
c2
d2 <- c("a", "b", "c")
d2
e2 <- c("Hello!", "World", "is", "good!")
e2

b <- c("a", "a", "b", "c")
b
qplot(b) # 빈도 그래프 만들기

e2_paste <- paste(e2, collapse = " ") # 빈칸 구분자로 문자 붙이기 
e2_paste

e3_paste <- paste(e2, collapse = ",") # 빈칸 구분자로 문자 붙이기 
e3_paste

library(ggplot2)
b
qplot(b)

qplot(data = mpg, x = hwy)
qplot(data = mpg, x = cty)
qplot(data = mpg, y = hwy, x = drv, geom = "point")
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot")
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot", colour = drv)
?qplot

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
