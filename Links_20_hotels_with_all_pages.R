library(rvest)
library(dplyr)
library(stringr)
library(gdata)

#####################################################################################
############################# Finding 20 hotels in Armenia ##########################
#####################################################################################

URL <- "https://www.tripadvisor.com/Hotels-g293931-Armenia-Hotels.html"
(LINKS<-read_html(URL) %>% 
  html_nodes(css="a.linkText") %>% html_attr("href"))
LINKS<-paste("https://www.tripadvisor.com/", LINKS, sep="")

num_pages<-c()

for(i in 1:length(LINKS)){
  HTML<-read_html(LINKS[i])
  number_of_pages<-HTML %>% html_nodes(css=".pageNum") %>% html_text()
  number_of_pages<-as.numeric(rev(number_of_pages)[1])
  link_num_pages<-c(LINKS[i], number_of_pages)
  num_pages<-rbind(num_pages, link_num_pages)
}

num_pages<-data.frame(num_pages)
colnames(num_pages)<-c("Link", "N_Pages")
rownames(num_pages)<-NULL
num_pages$N_Pages<-as.numeric(as.character(num_pages$N_Pages))

keep(num_pages, sure = TRUE)

#####################################################################################
######################### Scraping all links in Armenia #############################
#####################################################################################

page_sequence<-list()

for(i in 1:length(num_pages$N_Pages)){
  page_sequence[[i]]<-list(seq(0, (num_pages[i,"N_Pages"]-1)*30, by=30), num_pages[i,"Link"])
}

all_links<-c()

for (i in 1:20) {
  for(j in 1:num_pages[i,"N_Pages"]){
    first_part_link<-paste("https://www.tripadvisor.com//Hotels",
                           str_extract_all(page_sequence[[i]][[2]], "-g+[0-9]{1,}", simplify = T), sep="")
    
    second_part_link<-paste("-oa", page_sequence[[i]][[1]][j], sep="")
    
    third_part_link<-str_remove_all(page_sequence[[i]][[2]], first_part_link)
    
    full_link<-paste(first_part_link, second_part_link, third_part_link, sep="")
    
    all_links<-rbind(all_links, full_link)
  }
}

all_links<-data.frame(all_links)
colnames(all_links)<-"Link"
rownames(all_links)<-NULL

#write.csv(all_links, "Links_20_hotels_with_all_pages.csv", row.names = F)
