#' Big four major sports from 2016
"bigfour_2016"

load("~/Dropbox/tourneyr/data/bigfour_2016.rda")
load("~/Dropbox/competitivenessGit/data/tidy_alphas.R1.rda")

bigfour_2016[1,]
tidy_alphas[1,]
bigfour_2016 <- merge(bigfour_2016,tidy_alphas,by.x=c("sport","team_id"),by.y=c("sport","team_id"),all.x=TRUE)
save(bigfour_2016,file="~/Dropbox/tourneyr/data/bigfour_2016.rda")
