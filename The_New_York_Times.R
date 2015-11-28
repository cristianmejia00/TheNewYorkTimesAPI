#This code retrieves news metadat from The New York Times
#And save the search results as a .csv file

#Created 20151109
#####################################################################
#Install the library...
#install.packages("rtimes") 

#to see all parameters in the library...
?as_search

#To see all parameters from The New York Times
#http://developer.nytimes.com/docs/read/article_search_api_v2

#To get to know the original interface:
#http://developer.nytimes.com/io-docs

#Plase use your own API. You can get one here:
#http://developer.nytimes.com/

####################################################################
#The New York Times API downloads 10 articles per page.
#By default each call to the API will retrieve results of page 1
#The maximum number of pages available is 100 pages (1000 articles) for a given query
#So that, if your query is too broad you wont be able to download all the information

#You need to limit the call of your query to have less than 100 pages (= 1000 news articles).
#To do so, In this code I manually adjust the begining and end date to always 
#get less than 100 pages each time I run the code. 

#Once you get the first 100 pages, manually change the begining and end dates 
#to get another bunch of pages, and so on, until you are satisfied.
###################################################################
#Call libraries
library(rtimes)
library(plyr)

#Set parameters
name = "Japan_NYT_2010.csv" #Write the name of the output file, always put .csv

key = "" #Insert your API personal key here, get one: http://developer.nytimes.com/
q = "Japan"  #Query
fq = 'glocations:("Japan") AND 
      section_name:("World" "Opinion" "Blogs" "Public Editor" "The Public Editor" 
                    "Times Topics" "Topics" "Front Page" "Business" "Food" "Health" 
                    "Technology" "Science" "Open")'
begin_date <- "20100101"
end_date <- "20101231"
sort <- "oldest"

#Helper function to search by number of page
search_by_page <- function(pag) { 
  as_search(key = key, 
            q = q,
            fq = fq, 
            begin_date = begin_date,  
            end_date = end_date,
            sort = sort,
            page = pag)#,
            #facet_field = 'section_name',
            #fl = "word_count","snippet")#,"abstract","document_type",
            #"news_Desk","type_of_material")
}

#First running to know the number of articles in your query
output1 <- search_by_page(1) 

#Calculate the number of pages
#If you get more than 100, limit the query!!
pages <- as.numeric(floor(output1$meta["hits"]/10))
pages

#Get data
articles <- lapply((1:pages), function(x) {
  search <- search_by_page(x)
  data <- search$data
  return(data)})

#Process data
#This convert the obtained values to data frame
rows <- list(0) #Initial empty value for a an empty list
for (i in 1:pages) {
  list_of_values <-  lapply((1:length(articles[[i]])), function(y) {
    n <- articles[[i]][[y]]
    keywords <- paste(sapply ((1:length(n$keyword)), function(x) {
      n$keywords[[x]]$value}), collapse = " | ")
    keywords_type <- paste(sapply ((1:length(n$keyword)), function(x) {
      n$keywords[[x]]$name}), collapse = " | ")
    return (c("id" = n$"_id",
              "headline_main" = n$headline$main,
              "headline_print" = n$headline$print_headline,
              "lead_paragraph" = n$lead_paragraph,
              "snippet" = n$snippet,
              "abstract" = n$abstract,
              "web_url" = n$web_url,
              #"print_page" = n$print_page,
              "word_count" = n$word_count,
              "source1" = n$"source",
              #"blog" = n$blog,
              "keywords" = keywords,
              "keywords_type" = keywords_type,
              "type_of_material" = n$type_of_material,
              "document_type" = n$document_type,
              "news_desk" = n$news_desk,
              "section_name" = n$section_name,
              "subsection_name" = n$subsection_name,
              "pub_date" = n$pub_date,
              "byline" = n$byline$original))})
  rows <- c(rows, list_of_values)
}
rows <- rows[-1] #Removes the initial empty value
dfs <- lapply(rows, function(x) data.frame(as.list(x),stringsAsFactors = F))
dfs2<- rbind.fill(dfs) #merge all together

#Write file
write.csv(dfs2, file = name, row.names = F)

getwd()

