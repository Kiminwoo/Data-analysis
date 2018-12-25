library(rvest)
library(xml2)
library(dplyr) #파이프 구조 ( %>% ) 를 쓰기 위한 라이브러리 
library(stringr)
# 크롤링을 해오기 위해서 필요한 라이브러리들
install.packages("tm")
install.packages("wordcloud2")
install.packages("NLP")
install.packages("extrafont")
install.packages("readr")

# 필요한 패키지들 
guess_encoding()
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

First_url <- "https://store.steampowered.com/tags/en/Indie/#p="

# First_url :  Page라는 페이지의 이동 변수를 기준으로  앞 주소  

second_url <- "&tab=NewReleases"

# second_url : Page 라는 페이지의 이동 변수를 기준으로 뒤 주소 

urls <- NULL

# 여기 포인트 : 벡터의 주소값은 1 부터 시작한다 , 그런데 url 주소를 보면 페이지의 이동이 p=0부터 시작한다
# 그래서 x -1 을 해줌으로써 p=0을 맞춰줌 

for(x in 1:2)
{
  urls[x] <- paste0(First_url,as.character(x-1),second_url)
}

urls

# encdoing 확인 작업 
#guess_encoding('urls')

# ---------------------- Get Steam _ game _ name (Indie)
# 그 페이지에 존재하는 class 값이 존재한다면 그 값을 다 가져와 버린다 . 
# 그렇기 때문에 나는 , 특정 범위를 잡아서 그 범위 내에 존재하는 class 이름들만 검색한후 
# 해당하는 값들만 가져온다 . 

game_name <- NULL

for(url in urls)
  
{
  
  html <- read_html(url)
  game_name <- c(game_name, html %>% html_nodes("#NewReleasesRows") %>% html_nodes(".tab_item_content") %>% html_nodes(".tab_item_name") %>% html_text())
  
} 

 
 
# ------------------------  Get Steam _ game _ name _ Genre (Indie)


# 첫 번째 방법 : 게임에 대한 정보를 포함하고 있는 <a> 태그 주소값들을 뽑는다 . 

game_Genre_link <- NULL 

for(url in urls)
  
{
  
  html <- read_html(url)
  
  #game_Genre <- c(game_Genre, html %>% html_nodes(".tab_item_top_tags") %>% html_nodes(".top_tag") %>% html_text())
  
  game_Genre_link <- c(game_Genre_link, html %>% html_nodes("#NewReleasesRows") %>% html_nodes("a") %>% html_attr("href"))
    
}

# 가져온 <a> 태그에 들어가서 html 코드를 가져오고 , 장르를 가져온다 . 

game_Genre <- NULL

# 게임의 이름 마다의 장르를 나눠준다.
game_Genre_Filter <- NULL

Filter_str <- function(str,sep=",")
{
  paste(str,collapse = sep)
}

a <- 0 



for(url in game_Genre_link)
  
{
  a <- a + 1 
  
  download.file(url, destfile = "scrapedpage.html", quiet = TRUE , encoding = "UTF-8")
  
  html <- read_html(url)
  
  game_Genre <- c(game_Genre, html %>% html_nodes(".responsive_page_content") %>% html_nodes(".glance_tags.popular_tags") %>% html_nodes("a") %>% html_text())
  
  game_Genre_Filter[a] <- Filter_str(game_Genre)
  
}

game_Genre_Filter <- gsub("\t","",game_Genre_Filter)

game_Genre_Filter <- gsub("\r","",game_Genre_Filter)

game_Genre_Filter <- gsub("\n","",game_Genre_Filter)

game_Genre_Filter <- gsub(" ","",game_Genre_Filter)

game_Genre_Filter

game_Genre_Filter_real <- game_Genre_Filter[1:30]

game_Genre_Filter_real

game_Genre_Filter_real <- NULL

i <- 0

for(i in 2:30)
{
  game_Genre_Filter_real[i] <- str_replace_all(game_Genre_Filter_real[i],game_Genre_Filter[i-1],"")
  #game_Genre_Filter_real[i] <- gsub(game_Genre_Filter[i-1],"", game_Genre_Filter_real[i],perl = FALSE,fixed = FALSE, useBytes = FALSE)
}

game_Genre_Filter_real


data1 <- data.frame(게임=game_name , 장르=game_Genre_Filter_real)


data1$게임 <- format(data1$게임, justify = 'left')
data1$장르 <- format(data1$장르, justify = 'right')




write.csv(game_Genre_Filter_real,"C:/Users/user/Desktop/졸작/Steam_Crolling_Indie/Indie_Genre.csv",append=F)

write.csv(data1,"C:/Users/user/Desktop/졸작/Steam_Crolling_Indie/Indie_data(1~2).csv",append=F)


news_path <- NULL
modi_txt <- NULL


news_path <- paste0(getwd(),"/Indie_Genre.csv")
modi_txt <- readLines(news_path)


last_number_of_lines <- length(readLines(news_path))
line_numbers <- seq(1,last_number_of_lines,1)

last_number_of_lines

doc_ids <- line_numbers
df <- data.frame(doc_id = doc_ids,text = modi_txt, stringsAsFactors = FALSE)

modi_data <- Corpus(DataframeSource(df))

tdm_modi <- TermDocumentMatrix(modi_data)


TDM1 <- as.matrix((tdm_modi)) 

v=sort(rowSums(TDM1), decreasing = TRUE)
profile = data.frame(word=names(v),freq=v)

head(profile,10)

word_path <- paste0(getwd(),"/Indie_Genre.csv")

write.csv(profile, word_path)

data <- read.csv(word_path)

data <- data[,-1]

data_pick <- subset(data, freq >= 0)

head(data_pick,10)

in_out_colors = "function(word,weight)
{return(weight > 5 ? '#4B088A':'#81F7F3')}"


data
wordcloud2(data_pick,shape = "diamond",size=0.7,color=htmlwidgets::JS(in_out_colors), backgroundColor = "black")  