# library(rvest)
# library(tidyverse)
# 
# 
# 
# link <- "http://www.fangraphs.com/leaders.aspx?pos=p&stats=pit&lg=all&qual=y&type=0&season=2015&month=0&season1=2015&ind=0&team=&rost=&age=&fitler=&players=&page=1_10000"
# 
# page <- read_html(link)
# header <- page %>% html_nodes(headernode) %>% html_text()
# 
# tmp <- page %>% html_nodes(tablenode) %>% html_nodes(lines) %>% html_text()
# data <- tmp[3:length(tmp)]
# 
# out <- matrix(data, ncol=length(header), byrow=T)
# colnames(out) <- header
# colnames(out) <- sub("%", "pct", header)
# out <- as.data.frame(out[,-1])
# 
# # for(v in names(out)[grep("pct", names(out))]){
# #   out[[v]] <- recodeToPct(out[[v]])
# # }
# 
# 
# 



source("./code/utilityInfielder.R")

# getting data for one season
link <- buildLinkFG(position = "p", 
                    stats_for = "pit", 
                    season = 2017, 
                    season1 = 2017)
fg_pitchers_2017 <- makeTableFG(link)

dfs = list()
for(yr in 2012:2019){
  
  link <- buildLinkFG(position = "p", 
                      stats_for = "pit", 
                      season = yr, 
                      season1 = yr)
  
  tmp <- makeTableFG(link)
  tmp$Year = yr
  
  dfs[[paste("y", yr)]] = tmp
}

fg_pitcher_2012_2019 <- bind_rows(dfs)

write_csv(x = fg_pitcher_2012_2019, 
          path = "./data/fg_pitcher.csv")



