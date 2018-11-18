library(rvest)
library(XML)
# 크롤링을 해오기 위해서 필요한 라이브러리들
install.packages("tm")
install.packages("wordcloud2")
# 필요한 패키지들 

#wordcloud2를 구동하기 위한 라이브러리들 
library(devtools)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)
library(tm)
library(wordcloud2)

First_url <- "https://www.greenclimate.fund/what-we-do/newsroom/news-stories?p_p_id=101_INSTANCE_tLw79zWwerZZ&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-2&p_p_col_count=1&_101_INSTANCE_tLw79zWwerZZ_delta=30&_101_INSTANCE_tLw79zWwerZZ_keywords=&_101_INSTANCE_tLw79zWwerZZ_advancedSearch=false&_101_INSTANCE_tLw79zWwerZZ_andOperator=true&p_r_p_564233524_resetCur=false&_101_INSTANCE_tLw79zWwerZZ_cur="

# First_url : greenclimate 에서의 뉴스정보들을 모아논 곳의 주소  

second_url <- "#portlet_101_INSTANCE_tLw79zWwerZZ"

# second_url : greenclimate 에서의 뉴스정보들을 모아논 곳의 주소 두번째 

urls <- NULL

for(x in 1:5)
{
  urls[x] <- paste0(First_url,as.character(x),second_url)
}

# urls에 greenclimate NEWS + stories Save


links <- NULL 


for(url in urls)
{
  
  #에러 방지
  download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
  html <- read_html("scrapedpage.html")
  
  links <- c(links, html %>% html_nodes('#news-mosaic') %>% html_nodes('a') %>% html_attr('href') %>% unique())
  # 찾고자 하는 부분이 class일 경우 앞에 . 을 붙이고 , id이면 #을 붇인다.
  # 가져오고자 하는 클래스명에 띄어쓰기가 있을 경우 띄어쓰기 있는 부분에 .을 사용하면 된다.
}

# 기사가 존재하는 url을 모아서 links에 저장한다.

links

# News-stories의 기사에는 다른 사이트의 기사로 링크되는 기사도 많다 
# 그렇기 때문에 grep함수를 사용해서 greenclimate.fund가 있는 열의 위치를 가져올 것이다 . 쉽게 말해 이 사이트가 포함된 것만 가져올 것이라는 얘기이다.

num_GCFurl <- grep("www.greenclimate.fund",links)

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

# 우리는 인터넷 기사만 가져올 것이다 . 그렇기 때문에 , 
# 불필요한 video or youtube or pdf 는 제외시킨다.
# 그런데 결과를 보면 links_green에는 불필요한 것들이 
# 없는 것을 확인 할 수 있다.
grep("video",links_green)
grep("youtube",links_green)
grep("pdf",links_green)


#links_green

links_green <- links_green[-c(grep("video",links_green),grep("youtube",links_green),grep("pdf",links_green))]
# 검색된 열들은 links_green에서 빼준다 . 
# 허나 우리는 이 기능은 사용안해도 될 것같다 . 
# 불필요한 요소들이 이미 제외되어 있기 때문에.

# 제외시키고 남은 기사의 개수를 확인한다.
length(links_green)


txts <- NULL

# 추출된 url에서 글을 가져올 것이다 . 
for(link in links_green)
{
  download.file(link, destfile = "scrapedpage.html" , quite=TRUE)
  
  html <- read_html("scrapedpage.html")
  
  
  txts <- c(txts, html %>% html_nodes(".body.html-editable") %>% html_text())
  # 사이트의 주소를 보면 내가 가져오고자 하는 기사의 클래스명이 body html-editable이며 , 이 클래스의 해당 문자들을 전부 가져온다 
}
#txts

write.csv(txts,"C://Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말_HW2/GettingArticleImfor.csv")

# 가져온 기사들을 csv 파일로 저장시킬 것이다 
# 이때 사용되는 함수는 write.csv이며 , 절대경로를 지정시켜준다.


# 저장 시켜준 csv파일을 상대경로로써 가져올 것이다.
# 이때 내가 저장시켜준 파일을 경로변경을 해주어야 한다.
# 왜 파일 경로를 바꿔주면 , 상대경로가 USer/user/Document로
# 경로 지정이 되어 있기 때문이다.

news_path <- paste0(getwd(),"/GettingArticleImfor.csv")
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

# 영어 ( us ) 로 언어설정
Sys.setlocale(category = "LC_ALL", locale ="us")

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
# matrix format 
TDM1 <- as.matrix((tdm_modi)) 
# 모든 단어들의 빈도수를 체크 
v=sort(rowSums(TDM1), decreasing = TRUE)
profile = data.frame(word=names(v),freq=v)
#최상위 10개 결과 확인 
head(profile,10)

# 저장위치 설정 
word_path <- paste0(getwd(),"/GettingArticleImfor_word.csv")
# csv파이로 저장 
write.csv(profile, word_path)

#수정한 csv파일 불러오기 
data <- read.csv(word_path)
#첫 column은 단어의 중복이므로 삭제한다.
data <- data[,-1]
#일정 횟수 이상 검색된 항목만 추출한다.
data_pick <- subset(data, freq >= 5)
# 제일 많이 검색된 순서대로 보기 (10개까지)
head(data_pick,10)

#https://html-color-codes.info/Korean/
#특정 개수 이상 추출된 글자만 색깔을 변경할 것 이다.
in_out_colors = "function(word,weight)
{return(weight > 500 ? '#4B088A':'#81F7F3')}"


# word cloud 그리기 
# 기존모형으로 wordcloud 생성
# 모양선택 : shape = 'circle' , 'cardioid' , 'diamond ' , 'triangle -forward', 'triangle' , 'pentagon' , 'star'
wordcloud2(data_pick,shape = "diamond",size=0.9,color=htmlwidgets::JS(in_out_colors), backgroundColor = "black")  