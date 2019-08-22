require(rvest)
require(tidyverse)

recodeToPct <- function(list){
  list <- as.numeric(as.character(gsub("[[:space:]*%]", "", list)))
  list/100
}

teams = read.csv("./data/teams.csv", stringsAsFactors = F, strip.white=T)

getLeague <- function(abv="", name="", year=2016){
  
  lg = teams %>% filter((team==name | abr==abv) & (year >= Entered & year <= Left)) %>% select(league) %>% distinct()
  if(nrow(lg) != 1){
    print(paste("Error: league not found for team", name, abv))
    return(NA)
  } else {
    return(as.character(lg$league[1]))
  }
}

###--------------------------------------------------------###
### Fangraphs
###--------------------------------------------------------###
makeTableFG <- function(link, tablenode="#LeaderBoard1_dg1", headernode=".rgHeader", lines="td"){
  page <- read_html(link)
  header <- page %>% html_nodes(headernode) %>% html_text()
  
  tmp <- page %>% html_nodes(tablenode) %>% html_nodes(lines) %>% html_text()
  data <- tmp[3:length(tmp)]
  
  out <- matrix(data, ncol=length(header), byrow=T)
  colnames(out) <- header
  colnames(out) <- sub("%", "pct", header)
  out <- as.data.frame(out[,-1])
  
  for(v in names(out)[grep("pct", names(out))]){
    out[[v]] <- recodeToPct(out[[v]])
  }
  out
}

buildLinkFG <- function(base="http://www.fangraphs.com/leaders.aspx?", 
                        position="all",
                        stats_for="bat",
                        league="all",
                        qualified="y",
                        type="0", # stats
                        season=format(Sys.Date(), "%Y"),
                        month=0,
                        season1=format(Sys.Date(), "%Y"),
                        split_by_season="0", #1 = split by season if month=0 and season1!=season
                        team="", # if 0, include all teams
                        rost="",
                        age="", #age1,age2 if otherwise
                        players="", 
                        page="1_10000"){
  
  if(season < season1) { season1 <- season }
  
  paste0(base, "pos=", position, "&stats=", stats_for, "&lg=", league,
         "&qual=", qualified, "&type=", type, "&season=", season,
         "&month=", month, "&season1=", season1, "&ind=", split_by_season, 
         "&team=", team, "&rost=", rost, "&age=", age, "&fitler=",
         "&players=", players, "&page=", page )		
}

# useful objects
base <- "http://www.fangraphs.com/leaders.aspx?"
positionsFG <- c("all", "p", "1b", "2b", "SS", "3b", "c", "lf", "cf", "rf", "of", "dh", "np")
statsFG <- c("pit", "bat", "fld")
statTypeFG <- c("c",4,5,6,7,8,9,10,11,114,12,13,14,15,16,17,18,19,20,21,22,23,24,62,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224)
allStatTypes <- paste(statTypeFG, collapse=",")
#LeaderBoard1_dg1_ct

###--------------------------------------------------------###
### Baseball Reference
###--------------------------------------------------------###
getLeaderDataBR <- function(link="http://www.baseball-reference.com/awards/", league="Both", year=2015){
  #link1 <- "http://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fawards%2Fawards_2014.shtml%3Fsr%26utm_source%3Ddirect%26utm_medium%3DShare%26utm_campaign%3DShareTool&div=div_NL_Cy_Young_voting"
  link2 <- paste0(link, "awards_", year, ".shtml")
  page <- read_html(link2) 
  cytab <- page %>% html_table()#html_nodes("#div_AL_Cy_Young_voting, #csv_AL_Cy_Young_voting") %>% html_text()
  
  if(league=="AL"){
    out = cytab[[3]]
    out$League = "AL"
  } else if (league=="NL"){
    out = cytab[[4]]
    out$League = "NL"
  } else {
    outal = cytab[[3]]
    outnl = cytab[[4]]
    outal$League = "AL"
    outnl$League = "NL"
    out = rbind(outal, outnl)
  }
  
  names(out)[c(4:5, 10, 31) ] = c("VotePts", "FirstPlaceVotes", "WLpct", "ERAplus")
  out$Year = year
  
  return(out)
}
