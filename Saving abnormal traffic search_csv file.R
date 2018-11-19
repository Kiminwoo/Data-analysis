library(rvest)
library(dplyr)
library(XML)
First_url <- "https://www.google.co.kr/search?q=site:www.dbpia.co.kr+%EB%B9%84%EC%A0%95%EC%83%81+%ED%8A%B8%EB%9E%98%ED%94%BD&ei=2JbyW73FIIKy8QXfprGQDQ&start="


Second_url <- "&sa=N&ved=0ahUKEwi9kebxpeDeAhUCWbwKHV9TDNIQ8tMDCEI&biw=1218&bih=650&dpr=1.5"

urls <- NULL


for(num in 1:5)
{
  urls[num] <- paste0(First_url,as.character(num*10),Second_url)  
}

urls

links <- NULL

for(url in urls)
{
  html <- read_html(url)
  
links <-c(links, html %>% html_nodes('.r') %>% html_nodes('a')%>% html_attr('href')%>% unique() )
  
}

html
links[2]


links <- paste0("https://www.google.co.kr/",links[1:50])

Simlple_Contxt <- NULL 

for(link in links)
{
  html <- read_html(link)
  Simlple_Contxt <- c(Simlple_Contxt, html %>% html_nodes('.con_txt') %>% html_text())
}

Simlple_Contxt <- gsub("\r","",Simlple_Contxt)
Simlple_Contxt <- gsub("\n","",Simlple_Contxt)
Simlple_Contxt <- gsub(" ","",Simlple_Contxt)

Simlple_Contxt

write.csv(Simlple_Contxt,"C:/Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말고사/기말_HW2/비정상 트래픽/Simple_Contxt.csv")