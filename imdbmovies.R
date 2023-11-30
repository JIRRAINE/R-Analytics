############  Web Scraping IMDB TV SHOWS #################

# install.packages("rvest")
# install.packages("httr")
# install.packages("polite")

#loading of rvest, httr, and polite package into the environment
library(rvest)
library(httr)
library(dplyr) # use of pipeline %>%
library(polite)

#install.packages("kableExtra")
#kableExtra package is to create tables using the kable() function 

library(kableExtra)



polite::use_manners(save_as = 'polite_scrape.R')

#Specifying the url for desired website to be scraped
url <- 'https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250'

# asking permission to scrape
session <- bow("https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250",
               user_agent = "Educational")
session

#creating objects for the dataset
rank_title <- character(50)
links <- character(50)

# scraping in polite way using the h3 element
title_list <- scrape(session) %>%
  html_nodes('h3.ipc-title__text') %>% 
  html_text

# Extracting titles and simple data cleaning process
# we will use the title_list 
class(title_list)

# simple data cleaning and processing
# the tv shows list only contains 50 titles which is in index[2] to index[51]

title_list_sub <- as.data.frame(title_list[2:51])

head(title_list_sub)
tail(title_list_sub)
#changing column name to ranks
colnames(title_list_sub) <- "ranks"

#split the string(rank and title)
split_df <- strsplit(as.character(title_list_sub$ranks),".",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

#rename and delete columns
# deleting columns 3 and 4 since it duplicated the columns
split_df <- split_df[-c(3:4)] 

#renaming column 1 and 2
colnames(split_df) <- c("ranks","title","review","rate") 

# structure of splif_df
str(split_df) 
class(split_df)
head(split_df)

rank_title_ratings_episodes_year <- data.frame(
  rank_title_ratings_episodes_year = split_df)

write.csv(rank_title_ratings_episodes_year,file = "title.csv")

# extracting for link of every movies
link_list <- scrape(session) %>%
  html_nodes('a.ipc-title-link-wrapper') %>% 
  html_attr('href') 

head(link_list)

# simple cleaning and append 'https://imdb.com'

link_list[45:50]

link <- as.vector(link_list[1:50])
names(link) <- "links"

head(link)
tail(link)

# append https://imdb.com to each of the links extracted
for (i in 1:50) {
  link[i] <- paste0("https://imdb.com", class="ipc-metadata-list ipc-metadata-list--dividers-between sc-9d2f6de0-0 iMNUXk compact-list-view ipc-metadata-list--base"[i], sep = "")
}

#converting to a dataframe
links <- as.data.frame("h3.ipc-title__text")

rank_title <- data.frame(
  rank_title = split_df,"h3.ipc-title__text")

#combining the dataframe
scrape_df <- data.frame(rank_title,links,ratings,year,episodes)
names(scrape_df) <- c("Rank","Title","Link","Ratings","Year","Episodes")

head(scrape_df)

#saving the file into csv
write.csv(scrape_df,file = "data/top50.csv")

############ extracting the links of each movie ################

imdb_top_50 <- data.frame()

current_row <- 1

# for this example, we will get only the content for the 1st two rows
for (row in 1:2) {
  # Get the URL from the "href" column
  url <- links$link[current_row]
  
  # Read the HTML content of the webpage 
  
  session2 <- bow(url,
                  user_agent = "Educational")
  
  webpage <- scrape(session2)
  
  # Extract the rating using the appropriate CSS selector
  rating <- html_text(html_nodes(webpage, ".sc-e3e7b191-1 gjM"))
  rating <- rating[-2]
  
  #extracting votecount
  votecount <- html_text(html_nodes(webpage,
                                    'div.sc-bde20123-3.gPVQxL'))
  votecount <- votecount[-2]
  
  #extracting description
  movie_desc <- html_text(html_nodes(webpage, 
                                     '.sc-466bb6c-1.dWufeH'))
  movie_desc <- movie_desc[-2]
  
  
  #extracting metascore
  meta_score <- html_text(html_nodes(
    webpage,
    '.sc-b0901df4-0.bcQdDJ.metacritic-score-box'))
  meta_score <- meta_score[-2]
  
  # Print or save the extracted rating
  cat("Rating for", url, "is:", rating, "vote count is", votecount, 'and metascore is',meta_score, "\n")
  
  #store results
  # Store the results
  imdb_top_50[current_row,1] <- rating
  imdb_top_50[current_row,2] <- votecount
  imdb_top_50[current_row,3] <- movie_desc
  imdb_top_50[current_row,4] <- meta_score
  
  
  # Move to the next row
  current_row <- current_row + 1
  
  # Add some delay to avoid overloading the server (optional)
  Sys.sleep(3)
}

#changing column names
names(imdb_top_50) <- c("Rating","VoteCount","Description","MetaScore")

write.csv(imdb_top_50,file = "data/imdb_top_50.csv")

#combine with the previous dataframe

imdb_top_250 <- data.frame(
  scrape_df,imdb_top_50)

write.csv(imdb_top_250,file = "data/imdb_top_250.csv")


# displaying table using kableExtra
library(kableExtra)

df_d <- imdb_top_250[c(1:2),]

knitr::kable(df_d, caption = "IMDB Top 250 Movies") %>%
  kable_classic(full_width = T, html_font = "Arial Narrow") %>%
  kable_styling(font_size = 9)

################# End #####################################
             