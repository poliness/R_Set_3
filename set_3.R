library(rvest) 
library(xml2)
library(gtools)

#css-klxieh e1ia8j2v11
url1<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/","lublin","?areaMax=38&distanceRadius=0&page=1")
ileOgloszen<- read_html(url1)%>%html_node(".css-klxieh.e1ia8j2v11")%>%html_text()
ileOgloszen<-as.numeric(ileOgloszen)
wektorLinkow<-c()
#print(ileOgloszen)

for(i in 1: (ileOgloszen%/%35) ){
  url<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/","lublin","?areaMax=38&distanceRadius=0&page=",i)
  page<-read_html(url)
  temp<- page%>%html_nodes(".css-14cy79a")%>%html_children()%>%html_node("a")%>%xml_attr("href")
  wektorLinkow<-c(wektorLinkow,temp)
  print(temp)
  #print(url)
}





wektorLinkow<-wektorLinkow[!is.na(wektorLinkow)]
wektorLinkow<- unique(wektorLinkow)

#save(wektorLinkow,file="wektorLinkow.RData")
#load("wektorLinkow.RData")

zrobWierszRvest <- function(w,wektorLinkow,miasto){

  test_link <- paste0("https://www.otodom.pl",wektorLinkow[w])
  
  page <- read_html(test_link)
  cena <- read_html(test_link)%>%html_node(".css-8qi9av")%>%html_text()
  v<-page%>%html_nodes(".css-1qzszy5.estckra8")%>%html_text()
  
  v<-gsub('.*}','',v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<-v[indexy%%2==1]
  wartosci<-v[indexy%%2==0]
  
  
  df1 <- data.frame(matrix(wartosci,nrow=1,ncol=length(nazwyKolumn)))
  names(df1)<-nazwyKolumn
  df1<-cbind(df1,miasto=miasto)
  df1<-cbind(df1,link=test_link)
  df1
}

w<-1

miasto<-"lublin"
mieszkania <- NULL
liczbaLinkow<-length(wektorLinkow)
for(w in 1:liczbaLinkow){
  
  skip<-FALSE
  tryCatch(
    dm<-zrobWierszRvest(w,wektorLinkow,miasto),error=function(e){print(e);skip<<-TRUE}
    
  )
  
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-dm
  }
  else{
    mieszkania<-smartbind(mieszkania,dm)
  }
  
}
zrobWierszRvest(w,wektorLinkow = wektorLinkow,"lublin")

