setwd("C:/R/bigdata/project")
getwd()

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

# 24개 암종_성_연령별 암발생현황 파일 불러오기
cancer_df <- read.csv("24개 암종_성_연령별 암발생현황.csv",
                          header=T,
                          fileEncoding='CP949')
View(cancer_df)
names(cancer_df) <- c('cancer_type','gender','age','category','unit',
                          '1999','2000','2001','2002','2003','2004','2005','2006',
                          '2007','2008','2009','2010','2011','2012','2013','2014',
                          '2015','2016','2017','2018','2019')
names(cancer_df)
cancer_df[,1] <- gsub("\\(.*\\)$","",cancer_df[,1]) # 이름만 남기고 제거

# title : 1999년부터 2019년까지 암 발병률 현황 분석

# 1. 연도별 암 발생 현황 그래프로 시각화
# 가설: 매년 암발생률이 증가한다.
## 연도별 간, 위, 폐암 발생률 (꺾은선 그래프)
cancer3_by_year <- cancer_df %>%
  filter(cancer_type=='간'|cancer_type=='위'|cancer_type=='폐') %>% 
  filter(category=='조발생률[명/10만명]'&age=='계'&gender=='계')
View(cancer3_by_year)

### 위
stomach <- cancer3_by_year[1,c(6:26)]
stomach <- stomach %>% gather(key=years, value=stomach_cancer_rate)
stomach$stomach_cancer_rate <- as.numeric(stomach$stomach_cancer_rate)
ggplot(stomach,aes(years,stomach_cancer_rate,group=1))+
  geom_line(mapping=aes(x=years,y=stomach_cancer_rate),color='red')+geom_point()+
  ggtitle("연도별 위암 발병률")
### 간
liver <- cancer3_by_year[2,c(6:26)]
liver <- liver %>% gather(key=years, value=liver_cancer_rate)
liver$liver_cancer_rate <- as.numeric(liver$liver_cancer_rate)
ggplot(liver,aes(years,liver_cancer_rate,group=1))+
  geom_line(mapping=aes(x=years,y=liver_cancer_rate),color='red')+geom_point()+
  ggtitle("연도별 간암 발병률")
### 폐
lung <- cancer3_by_year[3,c(6:26)]
lung <- lung %>% gather(key=years, value=lung_cancer_rate)
lung$lung_cancer_rate <- as.numeric(lung$lung_cancer_rate)
ggplot(lung,aes(years,lung_cancer_rate,group=1))+
  geom_line(mapping=aes(x=years,y=lung_cancer_rate),color='red')+geom_point()+
  ggtitle("연도별 폐암 발병률")

Comment1 = "
	위암과 간암은 2011년도를 기점으로 점점 감소하는 추세를 보인다.
	그러나 폐암은 단 한번의 감소도 없이 지속적인 증가 추세를 보인다.
"

## 연도별 전체 암 발생률 (꺾은선 그래프)
cancer_by_year <- cancer_df %>% 
  filter(cancer_type=='모든 암'&gender=='계'&
           age=='계'&category=='조발생률[명/10만명]')
View(cancer_by_year)

cancer_by_year <- cancer_by_year[c(6:26)]
cancer_by_year <- cancer_by_year %>% gather(key=years, value=cancer_rate)
cancer_by_year$cancer_rate <- as.numeric(cancer_by_year$cancer_rate)
ggplot(cancer_by_year,aes(years,cancer_rate,group=1))+
  geom_line(mapping=aes(x=years,y=cancer_rate), color='red')+geom_point()+
  ggtitle("연도별 전체 암발병률")


# 2. 연령대별, 성별 암발병률 분석
# 2019년 연령대별 발생 암 종류 분석
# 가설: 연령대가 증가할수록 암발병률이 증가한다.
## 꺾은선그래프
cancer_by_age <- cancer_df %>% 
  filter(cancer_type=='모든 암'&gender=='계'&
           category=='조발생률[명/10만명]') %>% 
  filter(age!='계'&age!='연령미상') %>%
  select(age, '1999', '2009', '2019')
names(cancer_by_age)[2] = 'cancer_rate_1999'
names(cancer_by_age)[3] = 'cancer_rate_2009'
names(cancer_by_age)[4] = 'cancer_rate_2019'
cancer_by_age$cancer_rate_1999 <- as.numeric(cancer_by_age$cancer_rate_1999)
cancer_by_age$cancer_rate_2009 <- as.numeric(cancer_by_age$cancer_rate_2009)
cancer_by_age$cancer_rate_2019 <- as.numeric(cancer_by_age$cancer_rate_2019)

cancer_by_age$age <- as.factor(cancer_by_age$age)
View(cancer_by_age)

ggplot(cancer_by_age,aes(x=age,y=cancer_rate_1999))+geom_col()+
  scale_x_discrete(limits=cancer_by_age$age)+
  ggtitle("연령별 암발병률(1999)")
ggplot(cancer_by_age,aes(x=age,y=cancer_rate_2009))+geom_col()+
  scale_x_discrete(limits=cancer_by_age$age)+ 
  ggtitle("연령별 암발병률(2009)")
ggplot(cancer_by_age,aes(x=age,y=cancer_rate_2019))+geom_col()+
  scale_x_discrete(limits=cancer_by_age$age)+ 
  ggtitle("연령별 암발병률(2019)")

Comment1 = "
	1999, 2009, 2019년 모두 공통적으로 연령이 증가함에 따라 암발병률도 증가하는 모습을 보인다.
"

## 회귀분석
cancer_by_age$age = c(2,7,12,17,22,27,32,37,
            42,47,52,57,62,67,72,77,82,87) # 나이 각 범위의 중간 값 취해줌

LM <- lm(cancer_rate_2019 ~ age, data=cancer_by_age) 
summary(LM)
plot(cancer_rate_2019 ~ age, data=cancer_by_age)
abline(LM, col='red')

Comment1 = "
1) 추정된 회귀식
  y = b0 + b1x
    = -449.324 + 24.74x
2) 회귀모형의 타당성 검정
  귀무가설 : 모든 독립변수가 종속변수에 영향을 주지 않는다.
  (단순선형회귀분석에서는 독립변수가 한 개이므로 회귀계수에 대한 검정과 동일한 결과가 나옴)
  p-value=2.101e-08 < 0.05이므로 귀무가설 기각
  즉, 추정된 회귀식은 의미가 있다.
3) 회귀계수에 대한 검정(유의수준 5%에서 검정)
  -절편에 대한 검정
    p-value=0.00243 < 0.05 → 귀무가설(b0=0) 기각
  -기울기에 대한 검정
    p-value=2.1e-08 < 0.05 → 귀무가설(b1=0) 기각
    => 두 회귀 계수가 영향을 준다는 말이므로 추정된 회귀식을 써도 된다.
4) 결정계수
  R^2 = 0.8666
  => 비교적 높은 값으로 추정된 회귀식의 설명력이 충분하다.
"

# 2019년 성별 발생 암 종류 분석
# 가설: 남성과 여성의 3대 암발병률이 다르다. (간, 위, 폐) 
cancer3_by_gender <- cancer_df %>% 
  filter(cancer_type=='간'|cancer_type=='위'|cancer_type=='폐') %>% 
  filter(gender!='계'&category=='조발생률[명/10만명]'&age=='계') %>% 
  select(gender,cancer_type,'2019')
names(cancer3_by_gender)[3] = 'cancer_rate'
View(cancer3_by_gender)

man <- cancer3_by_gender %>% 
  filter(gender=='남자')
woman <- cancer3_by_gender %>% 
  filter(gender=='여자')

ggplot(data=man, aes(x=cancer_type,y=cancer_rate))+
  geom_col()+ggtitle("남성의 3대 암발병률(2019)")
ggplot(data=woman, aes(x=cancer_type,y=cancer_rate))+
  geom_col()+ggtitle("여성의 3대 암발병률(2019)")

Comment1 = "
	남성은 폐암이 가장 많은 반면 여성은 위암이 가장 많다.
"

# 성별에 따라 많이 발생하는 암종 분석
ctype_by_gender <- cancer_df %>% 
  filter(cancer_type!='모든 암'&cancer_type!='기타 암'&
           category=='조발생률[명/10만명]'&
           age=='계'&gender!='계') %>% 
  select(gender,cancer_type,'2019')
names(ctype_by_gender)[3] = 'cancer_rate' # 3열 이름 바꾸기

## 남성 암종별 암 발생률
man <- ctype_by_gender %>% 
  filter(gender=='남자') %>% 
  select(-gender)
replace(man$cancer_rate,man$cancer_rate=="-",0)->man$cancer_rate
man$cancer_rate <- as.numeric(man$cancer_rate)
man <- man %>% arrange(desc(man$cancer_rate))
View(man)

## 여성 암종별 암 발생률
woman <- ctype_by_gender %>% 
  filter(gender=='여자') %>% 
  select(-gender)
replace(woman$cancer_rate,woman$cancer_rate=="-",0)->woman$cancer_rate
woman$cancer_rate <- as.numeric(woman$cancer_rate)
woman <- woman %>% arrange(desc(woman$cancer_rate))
View(woman)

#### 성별에 따라 많이 발생하는 암종 평균막대그래프로 시각화
ggplot(man,aes(x=reorder(cancer_type,-cancer_rate),
               y=cancer_rate))+geom_col(fill='blue')+
  ggtitle("암종별 암발병률(남성)")
ggplot(woman,aes(x=reorder(cancer_type,-cancer_rate),
               y=cancer_rate))+geom_col(fill='red')+
  ggtitle("암종별 암발병률(여성)")

Comment1 = "
	남성은 폐,위,대장,전립선,간 순으로 발병률이 높다.
	여성은 유방,갑상선,대장,위,폐 순으로 발병률이 높다.
"