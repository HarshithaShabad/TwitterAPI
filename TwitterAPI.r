
#------------------------------------------------
#Github User:
#Founder of NewsBlur and Turn Touch
#NewsBlur
#Cambridge, Massachusetts 
#samuel@ofbrooklyn.com
#Github URL: https://github.com/samuelclay
#-------------------------------------------------

library(gh)
library(curl)
library(purrr)
library(tidyverse)
library(kableExtra)
library(knitr)
library(ggplot2)
library(jsonlite)
library(repurrrsive)
library(lubridate)
library(stringr)

###################################################
# Github Token
###################################################

my_token = "c7a75416bc32642607ab72e883ec5a85f897d0c2"
Sys.setenv(GITHUB_TOKEN = my_token)

###################################################
#1) user's id, name, public_repos, followers
###################################################

samuelclay <- gh("/users/samuelclay")


user_details <- data.frame(id = samuelclay$id, name = samuelclay$name, public_Repositories = samuelclay$public_repos, followers = samuelclay$followers)
user_details

user_details1 <- kable(user_details) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
user_details1

#################################################
#2) followers' id, name, public_repos, followers
#################################################

samuelclay_followers <- gh("/users/samuelclay/followers", .limit = Inf)

# Details of followers of samuelclay
df1_samuelclay_followers = map_df(
  samuelclay_followers, magrittr::extract, names(samuelclay_followers[[1]])
)
head(df1_samuelclay_followers,3)


# Details of each follower
followers1 = 
  map(df1_samuelclay_followers$login, ~gh(paste0("/users/", .)))

head(followers1,1)

length(followers1)

#Replace null values in name

for(i in 1:length(followers1)) {
  if (is.null(followers1[[i]]$name)){
    followers1[[i]]$name <- 'NA'
  }
}

# displaying requited details of samuelclay followers
df_samuelclay_followers <- map_df(
  followers1, magrittr::extract, c("id","name","public_repos","followers")
)
head(df_samuelclay_followers,3)


#######################################################
#3) A table summarizing the repositories' name, language, size, forks_count,
#stargazers_count, watchers_count, open_issues_count
######################################################
samuelclay_repos <- gh("/users/samuelclay/repos", .limit = Inf)

#Replace null values in language

for(i in 1:length(samuelclay_repos)) {
  if (is.null(samuelclay_repos[[i]]$language)){
    samuelclay_repos[[i]]$language <- 'NA'
  }else{
    samuelclay_repos[[i]]$language <- samuelclay_repos[[i]]$language
    }
}

#displaying requited details of samuelclay repository
df_samuelclay_repos <- map_df(
  samuelclay_repos, magrittr::extract, c("name", "language", "size", "forks_count","stargazers_count", "watchers_count", "open_issues_count" )
)
head(df_samuelclay_repos,3)


####################################
#4) A table summarizing the issues. The table should include the following
#columns: repo name, the number of open issues, the number of closed
#issues, the average duration to close an issue
####################################
sam_repos <- gh("/users/samuelclay/repos", .limit = Inf)
length(sam_repos)

# Read issues to a dataframe
sam_repos_df <-
  data_frame(
    repo = sam_repos %>% map_chr("name"),
    issue = repo %>%
      map(~ gh(repo = .x, endpoint = "/repos/samuelclay/:repo/issues?state=all",
               .limit = Inf))
  )

# Find number of no of open, no of closed, average duration to close issue
df = NULL
for(i in 1:length(sam_repos_df$issue)){
  open = 0
  closed = 0
  duration = 0
  if(length(sam_repos_df$issue[[i]])>0){
    for(j in 1:length(sam_repos_df[[2]][[i]]))
      if(sam_repos_df[[2]][[i]][[j]][["state"]]=='open'){
        open = open+1
      }else{
        closed = closed+1
        duration = duration + ymd_hms(sam_repos_df[[2]][[i]][[j]][["closed_at"]]) - ymd_hms(sam_repos_df[[2]][[i]][[j]][["created_at"]])
      }
  }
  duration = duration / closed
  duration = as.duration(duration)
  repo <- sam_repos_df$repo[[i]]
  df = rbind(df, data.frame(repo,open,closed,duration))
}
df

############################################
#Visualization 1
#This graph plots size of repositories in each language

ggplot(data = df_samuelclay_repos, aes(x = as.character(language), y = size))+
  geom_bar(stat="identity")

#Visualization 2
#This graph plots number of issues open in each repository

ggplot(data = df, aes(x = repo, y = open))+
  geom_bar(stat="identity")














