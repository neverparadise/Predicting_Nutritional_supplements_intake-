


################################################################
# 기말 프로젝트


#Cluster Analysis



################################################################

#1. 2개의 Attribue에 대한 군집분석


# 라이브러리 import
library(factoextra)
library(arules) ## 연관관계 분석을 위한 aprior() 를 쓰기 위함
library(knitr)
library(dplyr)
library(arulesViz) ## 시각화에 필요
library(visNetwork) ## 시각화 중 네트워크 표현에 필요
library(igraph) ## 시각화 결과물을 인터렉티브(움직이게) 해줌
library(NbClust)
library(fpc)
library(scatterplot3d)
library(dplyr)
library(ggplot2)
library(cluster)
library(readxl)

# 데이터 로드
data <- read.csv("preprocess 3.csv")

# 필요한 열만 추출
selected_data <- data %>% select(sex, age, ainc, HE_ht, HE_wt,
                                 HE_BMI, N_INTK, N_WATER, N_PROT, N_FAT, N_MUFA, N_CHOL, N_CHO, 
                                 N_TDF, N_SUGAR, N_CA, N_PHOS, N_FE, N_NA, N_K, N_VA, N_VITC, 
                                 N_RETIN, N_B1, N_B2, N_NIAC)

# 데이터간의 상관관계 분석
kable(cor(selected_data[1:12]))
kable(cor(selected_data[13:25]))
pairs(selected_data[1:3])
pairs(selected_data[4:6])
pairs(selected_data[7:11])

#Min-Max 연산을 수행하는 함수작성
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# 데이터의 정규화
selected_data$ainc <- normalize(selected_data$age)
selected_data$ainc <- normalize(selected_data$ainc)
selected_data$HE_ht <- normalize(selected_data$HE_ht)
selected_data$HE_wt <- normalize(selected_data$HE_wt)
selected_data$HE_BMI <- normalize(selected_data$HE_BMI)
selected_data$N_INTK <- normalize(selected_data$N_INTK)
selected_data$N_WATER <- normalize(selected_data$N_WATER)
selected_data$N_PROT <- normalize(selected_data$N_PROT)
selected_data$N_FAT <- normalize(selected_data$N_FAT)
selected_data$N_CHOL <- normalize(selected_data$N_CHOL)
selected_data$N_CHO <- normalize(selected_data$N_CHO)
selected_data$N_TDF <- normalize(selected_data$N_TDF)
selected_data$N_SUGAR <- normalize(selected_data$N_SUGAR)
selected_data$N_CA <- normalize(selected_data$N_CA)
selected_data$N_PHOS <- normalize(selected_data$N_PHOS)
selected_data$N_FE <- normalize(selected_data$N_FE)
selected_data$N_NA <- normalize(selected_data$N_NA)
selected_data$N_K <- normalize(selected_data$N_K)
selected_data$N_VA <- normalize(selected_data$N_VA)
selected_data$N_VITC <- normalize(selected_data$N_VITC)
selected_data$N_RETIN <- normalize(selected_data$N_RETIN)
selected_data$N_B1 <- normalize(selected_data$N_B1)
selected_data$N_B2 <- normalize(selected_data$N_B2)
selected_data$N_NIAC <- normalize(selected_data$N_NIAC)
selected_data$N_MUFA <- normalize(selected_data$N_MUFA)



# 클러스터의 생성

cluster1 <- selected_data %>% select(N_CHO, N_SUGAR)
result <- kmeans(cluster1, 4)
fit <- pam(cluster1, k=4)
cluster1$clustering <- factor(fit$clustering)
ggplot(data = cluster1, aes(x=N_SUGAR, y=N_CHO
                            , color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster2 <- selected_data %>% select(N_FE, N_PHOS)
result <- kmeans(cluster2, 6)
fit <- pam(cluster2, k=6)
cluster2$clustering <- factor(fit$clustering)
ggplot(data = cluster2, aes(x=N_FE, y=N_PHOS, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster3 <- selected_data %>% select(age, N_CA)
result3 <- kmeans(cluster3, 6)
fit <- pam(cluster3, k=6)
cluster3$clustering <- factor(fit$clustering)
ggplot(data = cluster3, aes(x=age, y=N_CA, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster4 <- selected_data %>% select(ainc, N_INTK)
result4 <- kmeans(cluster4, 4)
fit <- pam(cluster4, k=4)
cluster4$clustering <- factor(fit$clustering)
ggplot(data = cluster4, aes(x=ainc, y=N_INTK, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster5 <- selected_data %>% select(age, N_CHOL)
result5 <- kmeans(cluster5, 4)
fit <- pam(cluster5, k=4)
cluster5$clustering <- factor(fit$clustering)
ggplot(data = cluster5, aes(x=age, y=N_CHOL, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster6 <- selected_data %>% select(N_B2, N_B1)
result6 <- kmeans(cluster6, 4)
fit <- pam(cluster6, k=4)
cluster6$clustering <- factor(fit$clustering)
ggplot(data = cluster6, aes(x=N_B2, y=N_B1, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")

cluster7 <- selected_data %>% select(N_CA, N_NA)
result7 <- kmeans(cluster7, 3)
fit <- pam(cluster7, k=3)
cluster7$clustering <- factor(fit$clustering)
ggplot(data = cluster7, aes(x=N_CA, y=N_NA, color=clustering, shape=clustering)) + geom_point() + ggtitle("Clustering Result")


# 3D Plot
result2 <- kmeans(selected_data, 4)
result2
plotcluster(data2, result2$cluster, color=T, shade=T)
x_col <- selected_data$N_B1
y_col <- selected_data$N_B2
z_col <- selected_data$N_VITC
scatterplot3d(x=x_col, y=y_col, z=z_col, pch=14, angle = 70, color=rainbow(4)[result2$cluster])


# 계층군집분석 코드
sm_df <- cluster1 %>% sample_n(100) # 클러스터1 (2개의 열로 이루어진 데이터셋)에서 샘플 100개를 뽑는다.

d = dist(sm_df) # sample_dataframe의 거리 행렬을 만든다.

fit.average=hclust(d, method="average") #average 기반으로 클러스터들을 만든다.

plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")
clusters<-cutree(fit.average,k=5) # 만든 클러스터들을 시각화 한다.

table(clusters)

aggregate(cluster1,by=list(cluster=clusters),median)
plot(fit.average,hang=-1,cex=.8,
     main="Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average,k=4)

# 샘플링 후의 군집분석
sample_df <- selected_data %>% sample_n(100)
d2 = dist(sample_df)
fit.sample_average=hclust(d2, method="average")
plot(fit.sample_average, hang=-1, cex=.8, main="평균 클러스터링")


# 3D로 시각화한 군집분석 결과

kmeans()
{
  require(graphics)x<-cbind(datas2$age,datas2$HE_BMI,datas2$earn_month)
  colnames(x) <- c("x", "y")
  (cl <- kmeans(x, 2))
  plot(x, col = cl$cluster)
  points(cl$centers, col = 1:2, pch = 8, cex = 2)
  
  # sum of squares
  ss <- function(x) sum(scale(x, scale = FALSE)^2)
  
  fitted.x <- fitted(cl);  head(fitted.x)
  resid.x <- x - fitted(cl)
  cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
        c(ss(fitted.x), ss(resid.x),    ss(x)))
  stopifnot(all.equal(cl$ totss,        ss(x)),
            all.equal(cl$ tot.withinss, ss(resid.x)),
            all.equal(cl$ betweenss,    ss(fitted.x)),
            all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
            all.equal(ss(x), ss(fitted.x) + ss(resid.x))
  )
  
  kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))
  
  (cl <- kmeans(x, 3, nstart = 16))
  plot(x, col = cl$cluster)
  points(cl$centers, col = 1:5, pch = 8)
}

#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x=datas2$age,y=datas2$HE_BMI,z=datas2$earn_month,pch=20,color=rainbow(3)[cl$cluster])








################################################################

#2 PCA 분석을 통해 주성분 추출을 한 후의 군집분석


## 연속형 변수 추출 및 정규화
start <- read.csv("preprocess 3.csv")
k <- start %>% select(earn_month, age, make_energy_series,HE_ht,HE_wt)
g <- k %>% head(100)
gg <- scale(g)
k <- scale(k)

## Distance measures 분석 
res.dist <- get_dist(k, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## Partitioning clustering  최적의 클러스터링 개수 파악 

fviz_nbclust(k, kmeans, method = "gap_stat")

## 클러스터링 계산 

set.seed(123)
km.res <- kmeans(k, 4, nstart = 25)
# Visualize
fviz_cluster(km.res, data = k,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

##요인분석
pca.res<-PCA(k, graph=FALSE)
fviz_contrib(pca.res, choice="var", axes=1, top=4)
fviz_contrib(pca.res, choice="var", axes=2, top=4)

raw<-read.csv("preprocess 3.csv")
x <- raw$make_energy_series
y<- raw$age

xy <- data.frame(cbind(x,y))

plot(xy, pch = 19, xlab = c("x coordinate"), ylab = c("y coordinate"), 
     xlim = c(-5, 5), ylim = c(20,80), 
     main = "scatter plot of xy")

# adding student label
text(xy[,1], xy[,2], labels = abbreviate(rownames(xy)), 
     +      cex = 0.8, pos = 1, col = "blue") # pos=1 : at the bottom


# adding dotted line
abline(v=c(3), col = "gray", lty = 2) # vertical line
abline(h=c(3), col = "gray", lty = 2) # horizontal line

hc_cent <- hclust(dist(xy)^2, method="centroid")
hc_cent

my_par = par(no.readonly = TRUE)
par(oma = c(0, 0, 1, 0))
par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom







#######################################################################




# 3. Centroid 생성 함수


  
  # Centroid 개선 하기
  
  #Centroid 함수 - 초기 Centroid의 x, y좌표값을 sample()을 통해 random하게 받음
  centroid_x=sample(x=min(x[1]):max(x[1]),size=2) #random값의 x값이 data의 최소값과 최대값 안에 오게끔 함
  centroid_x[2]
  centroid_y=sample(x=min(x[2]):max(x[2]),size=2)#random값의 y값이 data의 최소값과 최대값 안에 오게끔 함
  centroid_y[1]
  
  centroid<-function(x){#함수의 선언
    mat<-matrix(,nrow=2,ncol=3007)#centroid와 Euclidean distance를 계산하고, 비교한 값을 저장하는 matrix를 선언
    #이 때 3007은 data set의 개수, nrow=2는 축의 수로 분석 목적에 따라 조정 가능함 
    
    for(i in 1:3007){#for문을 통해 모든 data가 해당 과정을 반복 
      if( ((x[i,1]-centroid_x[1])^2+(x[i,2]-centroid_y[1])^2) > ((x[i,1]-centroid_x[2])^2+(x[i,2]-centroid_y[2])^2) ){
        #if()문을 통해 데이터와 두 Centroid 간의 Euclidean distance를 서로 비교 수행
        mat[1,i]<-0#2번째 센트로이드와 거리가 더 가까울 경우 
        mat[2,i]<-1#행렬의 1행에 0을, 2행에 1을 대입.  
      }
      else{#반대의 경우도 마찬가지로 동작
        mat[1,i]<-1
        mat[2,i]<-0
      }
    }
    centroid_x[1]=0 #Update에 앞서 값을 초기화
    centroid_x[2]=0
    centroid_y[1]=0
    centroid_y[2]=0
    count1=0
    count2=0
    sum1_x=0
    sum1_y=0
    sum2_x=0
    sum2_y=0
    for(i in 1:3007){#데이터의 수만큼 반복하여 Centroid를 update
      if(mat[1,i]==0){
        sum2_x=sum2_x+x[i,1]
        sum2_y=sum2_y+x[i,2]
        count2=count2+1
      }
      else{
        sum1_x=sum1_x+x[i,1]
        sum1_y=sum1_y+x[i,2]
        count1=count1+1
      }
    }
    centroid_x[1]<-sum1_x/count1#합을 Count로 나누어 최종적으로 centroid를 업데이트
    centroid_x[2]<-sum2_x/count2
    centroid_y[1]<-sum1_y/count1
    centroid_y[2]<-sum2_y/count2
    print(centroid_x[1])
    print(centroid_x[2])
    print(centroid_y[1])
    print(centroid_y[2])
    
  }

  
  
  
  


# Association Rule Discovery


########################################################
#데이터를 transaction 형태로 변환 위한 전처리
rm(list = ls())
new <- read.csv("preprocess 3.csv")
new <- new %>% select(-X, -town_t, -ho_incm5, -cfam, -allownc, -house, -live_t,-marri_1, -tins, -npins, -BD1, -BD7_5,-BA2_22, -BP6_2 , -BP6_31, -BP7, -mh_stress, -BS9_2, -BS13, -BS12_1, -sm_presnt, -HE_wt, -HE_ht, -BM1_0, -BM2_4, - BM2_5, -BM13, -BM8, -BM14, -T_ex, -L_BR_TO, -L_LN_TO, -L_DN_TO, -LK_EDU, -LK_LB_CO, -N_DUSUAL, -make_energy_series, -fat_series, -vitamin_A_series, -fatty_acid_series,-LF_BUYER, -LK_LB_IT, -LQ2_ab )
new$earn_month <- ifelse(new$earn_month < 410.1667, 0, 1)
new$genertn <- ifelse(new$genertn %in% c(4,5),1 , 0) #미혼자녀 + 부모(편부모) : 1, 그 외 0
new$BO1 <- ifelse(new$BO1 %in% c(4, 5),1 , 0) # 자기체형을 뚱뚱하다고 인식 1, 마르다고 인식 0
new$BO1_1 <- ifelse(new$BO1_1 %in% c(2, 3),1 , 0) # 1년간 체형변화 있음:1, 없음:0
new$BO2_1 <- ifelse(new$BO2_1 %in% c(1, 2, 3),1 , 0) # 체중변화 시도:1, 없음:0
new$BP1 <- ifelse(new$BP1 %in% c(1, 2),1 , 0) # 스트레스 많음:1, 스트레스 조금:0
new$smoking <- ifelse(new$smoking %in% c(1, 2),1 , 0) # 흡연 5갑이상:1, 흡연 경험 5값 이하:0
new$HE_BMI <- ifelse(new$HE_BMI < 23.86631, 0, 1) # bmi 중간아래 : 0, bmi 중간값 이상:1
new$BM7 <- ifelse(new$BM7 %in% c(1, 2, 3),1 , 0) # 씹기문제있음:1, 문제 없음:0
new$L_BR_FQ <- ifelse(new$L_BR_FQ %in% c(1),1 , 0) # 주 아침식사 5회이상:1, 문제 없음:0
new$L_LN_FQ <- ifelse(new$L_LN_FQ %in% c(1),1 , 0) # 주 점심식사 5회이상:1, 문제 없음:0
new$L_DN_FQ <- ifelse(new$L_DN_FQ %in% c(1),1 , 0) # 주 저녁식사 5회이상:1, 문제 없음:0
new$L_OUT_FQ <- ifelse(new$L_OUT_FQ %in% c(1,2,3,4),1 , 0) # 주 3,4회 이상 외식:1, 주1,2회 이하:0
new$target_ingest <- ifelse(new$target_ingest == "NO",0 , 1) # 타겟인제스트 복용:1, 비복용:0
new$N_DIET <- ifelse(new$N_DIET %in% c(2),0 , 1) # 식단조절함:1, 조절안함:0
new$LF_SAFE<- ifelse(new$LF_SAFE %in% c(1),1 , 0) # 식생활 좋음:1, 안좋음:0
new$region <- ifelse(new$region %in% c(1,4,6,8,9,11,12),1 , 0) #윗지방 : 1, 아랫지방:0
new$age <- ifelse(new$age > 53 ,1 , 0) #평균이상나이 : 1, 평균이하나이:0
new$sex < - ifelse(new$sex == "2" ,0 , 1) #남자 : 1, 여자:0
new

df <- data.frame(ID=integer(), Item=character()) # 데이터프레임 껍데기 생성
table(df)

col <- length(colnames(new)) # test의 열 수
row <- length(row.names(new)) # test의 행 수
col_names <- colnames(new)

# test[1,2] 1행 2열 값 추출
new
count <- 1
for(i in 1:row){
  for(j in 1:col){
    if(new[i,j] == 1){ # 만약 값이 1이면 df에 (id, input_colnames)삽입
      input_colnames <- col_names[j] # 대입하는 속성 이름
      newrow = data.frame(ID=count, Item=input_colnames) # 아래와 함께 행 결합
      df <- rbind(df, newrow)
    }
  }
  count = count + 1
}

df
df$Item <- as.character(df$Item) # 형변환
str(df)
# id에 대한 구매 데이터 완료
# 진짜 거래 데이터로 변환

# 먼저 id별 구매 데이터 분리
library("arules")
df.list <- split(df$Item, df$ID)
df.list

# 거래 데이터로 변환
df.trans <- as(df.list, "transactions")
df.trans
image(df.trans)
summary(df.trans)

df.rules <- apriori(df.trans)
summary(df.rules)

rules <- apriori(df.trans, parameter=list(support=0.1, confidence=0.9,
                                          target="rules"))
summary(rules)
inspect(rules)

######### 타켓값 지정 가능, 원하지 않는 At 제거 가능############
rule_s <- apriori(df.trans, parameter=list(support=0.1,confidence=0.6),
                  appearance=list(rhs="target_ingest",default='lhs', none=c("L_LN_FQ","L_DN_FQ")))
inspect(rule_s)
##################


install.packages("arulesViz")
library(arulesViz)
plot(rule_s)
plot(rule_s, method = "paracoord", control = list(reorder = TRUE))
plot(rule_s, method = "graph", control = list(type = "items"))
plot(rule_s, method = "graph")
plot(rule_s, method = "grouped")






### 기타 : 데이터의 균형을 깨뜨려서 군집 생성이 가능한지 확인  
# 정규분포 파괴된 데이터 생성

str(raw)

age<-raw$age
age
hist(age, breaks=10) # 모든 나이 100명씩 샘플링 하면 된다...
# 방법 - 1. filter를 써서 20~30 / 30 ~ 40 / 40 ~ 50 / 50 ~ 60 / 60 ~ 70 / 70 ~ 80 를 나눈다
# 2. 각각에서 100개씩 샘플링 한다
# 3. 샘플링 한걸 합친다
# 4. 합친걸로 돌려본다

two_3<- raw %>% filter(age >= 20 & age < 30) # 20 ~ 30
three_4<- raw %>% filter(age >= 30 & age < 40) # 30 ~ 40
four_5<- raw %>% filter(age >= 40 & age < 50) # 40 ~ 50
five_6<- raw %>% filter(age >= 50 & age < 60) # 50 ~ 60
six_7<- raw %>% filter(age >= 60 & age < 70) # 60 ~ 70
seven_8<- raw %>% filter(age >= 70 & age < 80) # 70 ~ 80

# 랜덤 샘플링
two3 <- two_3[sample(nrow(two_3), 60),]
three4 <- three_4[sample(nrow(three_4), 60),]
four5 <- four_5[sample(nrow(four_5),60),]
five6 <- five_6[sample(nrow(five_6), 60),]
six7 <- six_7[sample(nrow(six_7), 60),]
seven8 <- seven_8[sample(nrow(seven_8), 60),]

random <- rbind(two3, three4, four5, five6, six7, seven8)
random


#사용 가능한 연속형 변수 9가지 추출
continuous <- start %>% select(target_ingest,earn_month,HE_ht,HE_wt,HE_BMI,make_energy_series,fat_series,vitamin_A_series,fatty_acid_series,age) 

#전체 데이터 중 50%만 램덤 샘플링
idx <- sample(1:nrow(continuous),nrow(continuous)*0.5)
data <- continuous[idx,]#데이터 프레임 형태로
table(data$target_ingest)

#층화 균등 샘플링[계층 별로 비율을 동등하게 검출]
idx <-createDataPartition(continuous$target_ingest,p=0.5,list=F)
data <- continuous[idx,]
table(data$target_ingest)

#계층 비율 샘플링[계층 별 비율 반영하여 검출]
continuous$cut <- cut(continuous$age, breaks = c(0,30,40,50,60, 1E7), labels = c("A","B","C","D","E"), right = TRUE)
table(continuous$cut)
#나이그룹 별 비율 맞춰 300개 샘플링
data <- stratified(continuous, "cut", size = c(A = 20,B = 50,C = 70,D =70 ,E =90)) 

#유클리디안 dist, 맨하튼 dist 비교 함수
my_dist_calculator <- function(a, b, metric = "euclidean"){
  if(metric == "manhattan"){
    d<-abs(a-b)
    d<-sum(d)
  }else{
    d<-(a-b)^2
    d<-sum(d)
    d<-sqrt(d)
  }
  return(d) 
}

x<-t(data.frame(data))
(euc_dist <- dist(x, method = "euclidean"))
(min_dist <- dist(x, method = "minkowski"))
(man_dist <- dist(x, method = "manhattan"))
(can_dist <- dist(x, method = "canberra"))

  # dendrogram: 군집결과를 나무구조로 나타내 군집간의 구조관계를 알 수있도록 한 그래프
  #===========계층적 군집화============
#최단연결법:두 집단 관측치 거리의 최소값을 거리값으로 정의
sin_den <- hclust(can_dist^2, method = "single")
plot(sin_den,hang = -1, main = "Cluster Dendrogram - single")
rev(com_den)
hclust(d = can_dist^2, method = "single")
#최장 연결법: 두집단 간 관측치들의 거리의 최댓값을 군집간의 거리로 정의
com_den <- hclust(can_dist^2, method = "complete")
plot(com_den, hang = -1, main = "Cluster Dendrogram - complete")
#평균연결법: 두집단 간 관측치들의 거리의 평균을 군집간의 거리로 정의
avg_den <- hclust(can_dist^2, method = "average")
plot(avg_den, hang = -1, main = "Cluster Dendrogram - average")
#중앙값연결법: 두집단 간 관측치들의 중앙값의 거리를 군집간의 거리로 정의
med_den <- hclust(can_dist^2, method = "median")
plot(med_den, hang = -1, main = "Cluster Dendrogram - median")
#중심연결법: 두집단의 각 중심 사이의 거리
cen_den <- hclust(can_dist^2, method = "centroid")
plot(cen_den, hang = -1, main = "Cluster Dendrogram - centroid")

