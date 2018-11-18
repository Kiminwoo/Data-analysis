library(rvest)
library(xml2)
# 크롤링을 해오기 위해서 필요한 라이브러리들
install.packages("tm")
install.packages("wordcloud2")
install.packages("NLP")
install.packages("extrafont")
install.packages("readr")

# 필요한 패키지들 
guess_encoding()
#wordcloud2를 구동하기 위한 라이브러리들 
library(devtools)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)
library(tm)
library(NLP)
library(wordcloud2)
library(extrafont)
First_url <- "http://www.dbpia.co.kr/SearchResult/Search?q=%28%5BIT%C2%A7coldb%C2%A72%C2%A751%C2%A73%5D%29&searchWord=%EC%A0%84%EC%B2%B4%3D%5E%24IT%5E*&Collection=0&nSort=1&nSorttype=desc&Page="

# First_url :  Page라는 페이지의 이동 변수를 기준으로  앞 주소  

second_url <- "&nPagesize=20&searchAll=IT&Multimedia=0&isFullText=0&Collection=0&SearchMethod=0&SrvYN=&PublishDate=&PublishSttDate=&PublishEndDate="

# second_url : Page 라는 페이지의 이동 변수를 기준으로 뒤 주소 
urls <- NULL

for(x in 1:5)
{
  urls[x] <- paste0(First_url,as.character(x),second_url)
}

# urls에 1~5페이지의 주소 url을 저장시킨다.


links <- NULL 

for(url in urls)
{
  
  #에러 방지
  download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
  html <- read_html("scrapedpage.html")
  
  links <- c(links, html %>% html_nodes(".titleWarp") %>% html_nodes('a') %>% html_attr('href') %>% unique())
  
  # titleWarp는 메인 기사 본문을 의미한다.
  # 찾고자 하는 부분이 class일 경우 앞에 . 을 붙이고 , id이면 #을 붇인다.
  # 가져오고자 하는 클래스명에 띄어쓰기가 있을 경우 띄어쓰기 있는 부분에 .을 사용하면 된다.
  # class = titleWarp : main article 
}

# 기사가 존재하는 url을 모아서 links에 저장한다.

links

num_GCFurl <- grep("Journal/",links)

num_GCFurl


links_green <- NULL

# 각 번호에 해당되는 links의 url 을 links_green에 담는다
# 이 말은 즉슨 , green 기사 url만 links_green에 저장시키는 것이다.

for(num in num_GCFurl)
{
  links_green <- c(links_green, links[num])
}

#links_green

links_green

links_dbpia <- NULL

links_dbpia <- paste0("http://www.dbpia.co.kr",links_green[1:98])
# 메인 주소값 추가 부분.

# 제외시키고 남은 기사의 개수를 확인한다.
length(links_dbpia)


main_article <- NULL # main_article , 본문기사 

write_name <- NULL # wirte_name , 저자 

Filter_name <- NULL  # Maping group user , 논문제작을 같이한 저자끼리 묶기 위해 

small_imformation <- NULL # small_imformation , 소제목과 발간일

#small_main_text <- NULL

# 추출된 url에서 글을 가져올 것이다 . 

# 같은 저자끼리 묶기위한 function 
plus_Str <- function(str,sep=",")
{
  paste(str,collapse = sep)
}

a = 0 

# 같은 저자끼리 묶기위한 사전 작업 

for(link in links_dbpia)
{
  download.file(link, destfile = "scrapedpage.html" , quite=TRUE)
  
  html <- read_html(link)
  
  #a <-a + 1
  
  #main_article <- c(main_article, html %>% html_nodes(".book_info") %>% html_nodes("h3") %>% html_text())
  
  #write_name <- c(write_name, html %>% html_nodes(".writeInfo") %>% html_nodes(".noticen") %>% html_text()) 
  
  #Filter_name[a] <-plus_Str(write_name,sep=",")
  
  #small_main_text <- c(small_main_text, html %>% html_nodes(".con_txt") %>% html_text())
  
  
  small_imformation <- c(small_imformation, html %>% html_nodes(".book_info") %>% html_node("dl") %>% html_nodes("dt") %>% html_text())
  
}
#txts

main_article
write_name
Filter_name
write_name <- NULL 
Filter_name <- NULL 

# 정리되는 저자들을 저장해줄 변수 
Filter_real_name <- NULL

#Filter_name 첫 부분 제거 
Filter_name <- Filter_name[-1]

# 같은 저자끼리 묶기 위한 , 정제 1차 정제 작업 
# 우선적으로 크롤링은 , 하나의 변수에 크롤링한 값을 연속적으로 넣는 성질을 가지고 있다 . 
# 물론 R에서 말이다 . 그렇기 때문에 논문에 따른 저자들을 묶어 줄 것인데 , 
# 논문에는 한명의 이름뿐만 아니라 여러명의 저자들이 논문들 같이 만들었을수도 있기 때문에
# 또하나의 Filter_real_name 을 만들어서 , 물론 이 변수는 Filter_name의 값들을 다 가지고 있어야한다
# 그리고 , gsub 함수를 사용해서 Filter_name[1] 의 값이 Filter_real_name [2] 자리에 존재한다면
# ""으로 처리해 주는 것이다. 쉽게 말해서 연속적인 값들에서 전 주소 값에 있는 값들을 없애는 것이다
# 이렇게 함으로써 연속적인 값들이 아닌 , 한 주소 url에 맞게 그 논문에 매치가 되는 것이다
Filter_real_name <- Filter_name[1:97]

Filter_real_name
Filter_name

Filter_real_name[2] <- gsub(Filter_name[1],"",Filter_real_name[2])
# 똑같이 복사해준 Filter_real_name을 정제해 줄 것인데 , gsub를 사용해서 스택되어진 값들을 제거해 줄 
# 것이다 . 빈 공백으로 1차 정제를 해준다 .


Filter_real_name[4] <- gsub(Filter_name[3],"",Filter_real_name[4])


for(number in 3:97)
{
  Filter_real_name[number] <- gsub(Filter_name[number-1],"",Filter_real_name[number])
}

Filter_real_name
# 제 2차 정제 작업

#for(number in 2:97)
#{
#  Filter_real_name[number] <- gsub(",","",Filter_real_name[number])
#}

small_imformation <- NULL

small_imformation
# small_imformation 정제 작업 
small_imformation <- gsub("\r","",small_imformation)

small_imformation <- gsub("\n","",small_imformation)

small_imformation <- gsub(" ","",small_imformation)

small_imformation

# 소제목과 , 발간일을 가져올 것인데,
# 크롤링한 값들을 보면 2행당 1개의 행에만 값이 들어가져 있다.
# 우리는 이쁘게 정제해줄 필요가 있다.
# 그런데 , R 에서는 한개의 항을 지워주면 , 자동으로 다음값이 지워진 값에 
# 대체 되는것을 알 수 있었다.
# 그래서 규칙적으로 보면 다음 for문을 사용해서 소제목 정제시킨다.
for(user in 2:190){
small_imformation <- small_imformation[-user]
}
small_imformation


# union : 문자는 문자별로 , 숫자는 같은 주소값에 이어서 저장 


# Create Dataframe 
dt <- data.frame(본문기사=main_article,저자=Filter_real_name,발간일=small_imformation)

dt
dt$본문기사 <- format(dt$본문기사,justify = 'left')
dt$저자 <- format(dt$저자,justify = 'left')
dt$발간일 <- format(dt$발간일,justify = 'left')

dt

# each other saving 

write.csv(main_article,"C://Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말_HW2/MainArticle.csv",append=F)
write.csv(Filter_real_name,"C://Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말_HW2/Filter_real_name.csv",append=F)
write.csv(small_imformation,"C://Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말_HW2/small_imformation.csv",append=F)



# 가져온 기사들을 csv 파일로 저장시킬 것이다 
# 이때 사용되는 함수는 write.csv이며 , 절대경로를 지정시켜준다.


# 저장 시켜준 csv파일을 상대경로로써 가져올 것이다.
# 이때 내가 저장시켜준 파일을 경로변경을 해주어야 한다.
# 왜 파일 경로를 바꿔주면 , 상대경로가 USer/user/Document로
# 경로 지정이 되어 있기 때문이다.
news_path <- NULL
modi_txt <- NULL


news_path <- paste0(getwd(),"/MainArticle.csv")
modi_txt <- readLines(news_path)

# 가져온 데이터에서 불필요한 문자 or 특수기호들을 제거해줄 것인데 이때 gsub를 사용할 것이다
# 특이한 점은 gsub는 정규표현식이 작동이 한다는 것
# <U+2013> or < U+00A0> 를 제거할 것인데 , 이것은 
# 각각 하이픈 , 빈칸이다 . 
# 결과적으로 가져온 데이터에서 하이픈과 공백은 제거가 되는 것

modi_txt <- gsub("<U.00A0>"," ",modi_txt)
modi_txt <- gsub("<U.2013>","-",modi_txt)


# 특이한 점은 's 를 제거해 주게 될 것인데 , 
# 영어 사이트이다 보니 소유격인 's또한 문자로 되어있을 것이다
# 소유격인 's는 굳이 필요가 없기 때문에 제거해준다.
modi_txt <- gsub("'s","",modi_txt)

# txt 파일의 총 Line수 구하기 
# 그리고 총 line수만큼 1씩 증가하는 벡터를 생성 

last_number_of_lines <- length(readLines(news_path))
line_numbers <- seq(1,last_number_of_lines,1)

# line별 마다 번호를 매기기 
# data.frame을 생성해 줄 것인데 , line별 마다의 번호 , txt ,To avoid problems delay re-encoding of strings by using 
# https://www.r-bloggers.com/r-tip-use-stringsasfactors-false/

doc_ids <- line_numbers
df <- data.frame(doc_id = doc_ids,text = modi_txt, stringsAsFactors = FALSE)

# https://thebook.io/006723/ch10/07/01/
# Corpus : 여기서는 DFSource의 df의 내용을 볼 것이다 정도 이해 
modi_data <- Corpus(DataframeSource(df))



modi_data

# 영어 ( us ) 로 언어설정
Sys.setlocale(category = "LC_ALL", locale ="us")

# 한글 언어로 설정
Sys.setlocale(category = "LC_ALL",locale ="korean")
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")

Sys.getlocale()
# 공백이 2개 이상인 걸 1개로 만든다( 문장내에서 혹은 문장 끝에서) 
modi_data <- tm_map(modi_data,stripWhitespace)
# 대문자를 소문자로 변경 , 같은 단어의 대,소문자 차이로 다른 단어로 인식이 될 수도 있기 때문에 
modi_data <- tm_map(modi_data,tolower)
#단어를 추출할거기 때문에 , 내부 숫자들은 다 제거해준다.
modi_data <- tm_map(modi_data,removeNumbers)

#stopwords("") <-- ""안에 있는 어떤 단어들이 있는지 확인해주는 기능인데 , 여기서 우리는 a , an , the , does 등을 제거할 것이다.
modi_data <- tm_map(modi_data, removeWords,stopwords("english"))

#구둣점 제거 ( 예를 들어서 `!~@#$%^&*등등 제거 )
modi_data <- tm_map(modi_data,removePunctuation)

# 각 line속의 단어들을 매트릭스 형태로 만들어서 엑셀에 저장
# 현재 line별로 되어 있는 modi_data --> matrix 형태로 만듦

# TDM 생성
tdm_modi <- TermDocumentMatrix(modi_data)

tdm_modi
# matrix format 
TDM1 <- as.matrix((tdm_modi)) 
TDM1
# 모든 단어들의 빈도수를 체크 
v=sort(rowSums(TDM1), decreasing = TRUE)
profile = data.frame(word=names(v),freq=v)
#최상위 10개 결과 확인 
head(profile,10)

# 저장위치 설정 
word_path <- paste0(getwd(),"/MainArticle.csv")
# csv파일로 저장 
write.csv(profile, word_path)

#수정한 csv파일 불러오기 
data <- read.csv(word_path)
#첫 column은 단어의 중복이므로 삭제한다.
data <- data[,-1]
#일정 횟수 이상 검색된 항목만 추출한다.
data_pick <- subset(data, freq >= 0)
# 제일 많이 검색된 순서대로 보기 (10개까지)
head(data_pick,10)

#https://html-color-codes.info/Korean/
#특정 개수 이상 추출된 글자만 색깔을 변경할 것 이다.
in_out_colors = "function(word,weight)
{return(weight > 5 ? '#4B088A':'#81F7F3')}"


# word cloud 그리기 
# 기존모형으로 wordcloud 생성
# 모양선택 : shape = 'circle' , 'cardioid' , 'diamond ' , 'triangle -forward', 'triangle' , 'pentagon' , 'star'

wordcloud2(data_pick,shape = "diamond",size=0.3,color=htmlwidgets::JS(in_out_colors), backgroundColor = "black")  