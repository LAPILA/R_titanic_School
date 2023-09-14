# 작업 디렉토리 설정: "setwd()"는 작업하려는 디렉토리 위치를 설정한다.
setwd("C:\\Users\\doo71\\OneDrive\\문서\\Rworks")

# 패키지 설치: "install.packages()"는 R에서 필요한 패키지를 설치하는 함수다.
install.packages(c("tidyverse", "readr", "ggplot2", "dplyr", "svDialogs"))

# 패키지 불러오기: "library()"는 R에서 필요한 패키지를 불러오는 함수다.
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(svDialogs)

#%>%왼쪽의 결과를 오른쪽의 함수로 전달
# 데이터 불러오기: "read_csv()"는 csv 파일을 읽어와 데이터 프레임 형태로 저장하는 함수다.
titanic_data <- read_csv("train.csv")



main <- function(){
  user_input <- dlgInput('출력할 자료를 입력하세요. 1.선그래프 2.막대그래프 3.ggplot으로 만든 점그래프 4.gglot으로 만든 선그래프 5.ggplot으로 만든 막대그래프')$res
  
  if (is.null(user_input) || user_input == "") {
    print("올바른 자료를 선택해주세요.")
    return()
  }
  
  if (user_input == "1") {
    line_graph()
  } else if (user_input == "2") {
    bar_graph()
  } else if (user_input == "3") {
    point_graph()
  } 
  else if(user_input=="4"){
    line_graph_g()  
  }
  else if(user_input=="5"){
    bar_graph_g()
  }
  else {
    print("올바른 자료를 선택해주세요.")
  }
}


# 클래스별 생존률 계산: "group_by()"는 특정 변수를 기준으로 데이터를 그룹화하고, "summarise()"는 그룹화된 데이터에 대한 요약 통계를 계산한다.
survival_by_class <- titanic_data %>%
  group_by(Pclass) %>%
  summarise(
    survival_rate = mean(Survived, na.rm = TRUE),
    survival_sd = sd(Survived, na.rm = TRUE),
    survival_variance = var(Survived, na.rm = TRUE),
    survival_median = median(Survived, na.rm = TRUE)
  )

# 선 그래프 생성: "ggplot()"는 그래프를 생성하고, "aes()"는 그래프의 x축, y축, 색상 등을 설정한다. "geom_line()"은 선 그래프를, "geom_point()"는 점 그래프를 생성한다.
line_graph_g <- function(){
  survival_by_class <- titanic_data %>%
    group_by(Pclass) %>%
    summarise(
      survival_rate = mean(Survived, na.rm = TRUE),
      survival_sd = sd(Survived, na.rm = TRUE),
      survival_variance = var(Survived, na.rm = TRUE),
      survival_median = median(Survived, na.rm = TRUE)
    )
  ggplot(survival_by_class, aes(x = Pclass, y = survival_rate, color = as.factor(Pclass), group = 1)) +
  geom_line() +
  geom_point(size = 4) +
  scale_color_discrete(name = "승객 등급") +
  labs(x = "승객 등급", y = "생존률", 
       title = "승객 등급별 생존률 (선 그래프)")
}

line_graph<-function(){
  ticket<-titanic_data$Pclass
  survived_cnt_class<-tapply(titanic_data$Survived,ticket,function(x) sum(x==1))
  
  total_cnt_class<-tapply(titanic_data$Survived,ticket,length)
  survival_rate_class<-survived_cnt_class/total_cnt_class
  
  tclass = 1:3 # 데이터 입력
rate = survival_rate_class # 데이터 입력
plot(tclass, # x data
     rate, # y data
     main='타이타닉호 탑승 티켓 등급별 생존률', # 제목
     type='o', # 그래프의 종류 선택(알파벳)
     col=c('red','green','blue'),
     lty=1, # 선의 종류(line type) 선택
     lwd=1, # 선의 굵기 선택
     xlab='등급별', # x축 레이블
     ylab='생존률' # y축 레이블
)
legend("topright", legend = c("생존률"), col = c("red"), lwd = 2)
}

# 막대 그래프 생성: "geom_bar()"는 막대 그래프를 생성한다.
bar_graph_g <- function(){
  survival_by_class <- titanic_data %>%
    group_by(Pclass) %>%
    summarise(
      survival_rate = mean(Survived, na.rm = TRUE),
      survival_sd = sd(Survived, na.rm = TRUE),
      survival_variance = var(Survived, na.rm = TRUE),
      survival_median = median(Survived, na.rm = TRUE)
    )
  ggplot(survival_by_class, aes(x = Pclass, y = survival_rate, fill = as.factor(Pclass))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "승객 등급") +
  labs(x = "승객 등급", y = "생존률", 
       title = "승객 등급별 생존률 (막대 그래프)")
}



bar_graph<-function(){
  ticket<-titanic_data$Pclass
  survived_cnt_class<-tapply(titanic_data$Survived,ticket,function(x) sum(x==1))
  
  total_cnt_class<-tapply(titanic_data$Survived,ticket,length)
  survival_rate_class<-survived_cnt_class/total_cnt_class
  
  par(mfrow=c(1, 1), mar=c(5, 5, 5, 7))
  barplot(survival_rate_class, main='승객 등급별 생존률(막대그래프)',
        col=c('red','green','blue'),
        names=c('1등급','2등급','3등급'),
        beside=TRUE,
        legend.text=c('1등급 탑승객','2등급 탑승객','3등급 탑승객'),
        args.legend = list(x='topright', bty='n', 
                           inset=c(-0.25,0)))}



# 점 그래프 생성: 이 코드는 점 그래프를 생성한다.
point_graph <- function(){ggplot(survival_by_class, aes(x = Pclass, y = survival_rate, color = as.factor(Pclass))) +
  geom_point(size = 4) +
  scale_color_discrete(name = "승객 등급") +
  labs(x = "승객 등급", y = "생존률", 
       title = "승객 등급별 생존률 (점 그래프)")
}
# 그래프 출력 및 메시지 박스 보여주기: "print()"는 데이터를 출력하고, "msgBox()"는 사용자에게 메시지 박스를 보여준다.
#dialogs <- list(line_graph, bar_graph, point_graph)
#for (i in 1:length(dialogs)) {
#  print(dialogs[[i]])
#  if (i != length(dialogs)) {
#    msgBox("다음 그래프를 보려면 '다음'을 클릭하세요.")
#  }
#}



# 최고 생존률 클래스 찾기: "filter()"는 특정 조건을 만족하는 데이터만 추출하고, "pull()"는 특정 변수의 값을 추출한다.
highest_survival_class <- survival_by_class %>%
  filter(survival_rate == max(survival_rate)) %>%
  pull(Pclass)

# 최고 생존률 클래스 출력하기: 최고 생존률을 가진 클래스를 출력한다.
print(paste("최고 생존률을 가진 클래스는", highest_survival_class, "입니다."))
# t-검정 수행: "t.test()"는 두 집단의 평균이 통계적으로 유의하게 차이가 있는지 확인하는 t-검정을 수행한다.
t_test_result <- t.test(survival_by_class$survival_rate)


main()
# 귀무가설 검정 결과 출력: t-검정의 결과를 바탕으로 귀무가설을 기각할지 채택할지 결정한다.
# p-value가 0.05보다 작으면 귀무가설을 기각하고, 그렇지 않으면 귀무가설을 채택한다.
if (t_test_result$p.value < 0.05) {
  msgBox("귀무가설을 기각함. 즉, 각 클래스의 생존률은 차이가 납니다다.")
} else {
  msgBox("t-검정 결과 귀무가설을 기각하지 않음. 즉, 각 클래스의 생존률은 차이나지 않습니다.")
}

#p-value값 확인
a<-t_test_result$p.value
a 

