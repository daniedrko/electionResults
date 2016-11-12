#prepping stuff for the app

# all of data in this cleaning script pulled from Federal Elections Commission website
# http://www.fec.gov/pubrec/electionresults.shtml

# 2012
res12 <- read.csv("~/federalelections2012.csv")

#we must reshape this data so it moves from wide format to long. here's how i do that

r12 <- reshape(res12,
                 varying = c("Obama..D.", "Romney..R.", "All.Others"),
               v.names = "votes",
               timevar = "choice", 
               times = c("Obama", "Romney", "others"), 
               direction = "long")

r12$votes <- as.numeric(as.character(r12$votes))
r12$Total.Vote <- as.numeric(as.character(r12$Total.Vote))
r12$percent <- (r12$votes/r12$Total.Vote)*100

# 2008
# pretty standard procedure on this, except for 2004
res08 <- read.csv("~/federalelections2008.csv")

r08 <- reshape(res08,
               varying = c("Obama..D.", "McCain..R.", "All.Others"),
               v.names = "votes",
               timevar = "choice", 
               times = c("Obama", "McCain", "others"), 
               direction = "long")

r08$votes <- as.numeric(as.character(r08$votes))
r08$Total.Vote <- as.numeric(as.character(r08$Total.Vote))
r08$percent <- (r08$votes/r08$Total.Vote)*100


# 2004
# format is slightly weird on this one, had to play with columns so i could easily combine them with all other years

res04 <- read.csv("~/federalelections2004.csv")
res04$X.3 <- res04$X.2 <- res04$X.1 <- res04$X <- res04$NOTES <- NULL
res04 <- subset(res04, TOTAL.VOTES != "Total State Votes:")
res04$GENERAL.RESULTS <- as.numeric(as.character(res04$GENERAL.RESULTS))
res04 <- subset(res04, !is.na(GENERAL.RESULTS))

res04$LAST.NAME...FIRST <- as.character(res04$LAST.NAME...FIRST)
test <- subset(res04, LAST.NAME...FIRST != "Bush, George W." & LAST.NAME...FIRST != "Kerry, John" & LAST.NAME...FIRST != "Kerry, John F." & LAST.NAME...FIRST != "Bush, George" & LAST.NAME...FIRST != "Bush, George Walker")

library(dplyr)
test <- test %>%
  group_by(STATE.ABBREVIATION) %>%
  mutate(TOTAL.VOTES.. = TOTAL.VOTES..,
            LAST.NAME...FIRST = "others",
            TOTAL.VOTES = " ",
            PARTY = " ",
            GENERAL.RESULTS = sum(GENERAL.RESULTS))
test <- subset(test, !duplicated(STATE.ABBREVIATION))

r04 <- rbind(res04, test)
r04$LAST.NAME...FIRST <- ifelse((r04$LAST.NAME...FIRST == "Bush, George W." | r04$LAST.NAME...FIRST == "Bush, George" | r04$LAST.NAME...FIRST == "Bush, George Walker"), "Bush", ifelse((r04$LAST.NAME...FIRST == "Kerry, John" | r04$LAST.NAME...FIRST == "Kerry, John F."), "Kerry", r04$LAST.NAME...FIRST))
r04 <- subset(r04, LAST.NAME...FIRST == "Bush" | LAST.NAME...FIRST == "Kerry" | LAST.NAME...FIRST == "others")

r04$percent <- (r04$GENERAL.RESULTS/r04$TOTAL.VOTES..)*100

r04$PARTY <- r04$TOTAL.VOTES <- NULL

colnames(r04)[1] = "State"
colnames(r04)[2] = "Total.Votes"
colnames(r04)[3] = "choice"
colnames(r04)[5] = "votes"
r04$year <- "2004"


r04t <- r04 %>%
  group_by(choice) %>%
  transmute(votes = sum(votes))
r04t <- subset(r04t, !duplicated(choice))
r04t$State <- "All"
r04t$Total.Votes <- sum(r04t$votes)
r04t$percent <- (r04t$votes/r04t$Total.Votes)*100
r04t$year <- "2004"
my.vars = c("State", "Total.Votes", "choice", "votes", "percent", "year")
r04t <- r04t[my.vars]

r04 <- rbind(r04, r04t)

# 2000

res00 <- read.csv("~/federalelections2000.csv")
res00$Total.Votes <- res00$Democratic.Candidates + res00$Republican.Candidates + res00$Other.Candidates

r00 <- reshape(res00,
               varying = c("Democratic.Candidates", "Republican.Candidates", "Other.Candidates"),
               v.names = "votes",
               timevar = "choice", 
               times = c("Gore", "Bush", "others"), 
               direction = "long")

r00$percent <- (r00$votes/r00$Total.Vote)*100
r00 <- subset(r00, !is.na(percent))





r04$PARTY <- NULL

r00$year <- "2000"
r04$year <- "2004"
r08$year <- "2008"
r12$year <- "2012"

colnames(r12)[1] = "State"



colnames(r08)[1] = "State"
colnames(r08)[2] = "Total.Votes"

colnames(r12)[2] = "Total.Votes"

r04$TOTAL.VOTES <- r00$id <- r08$id <- r12$id <- NULL

# added a party variable so we can compare the democrat or republican victory

r00$party <- ifelse(r00$choice == "Gore", "Democrat", ifelse(r00$choice == "Bush", "Republican", "Other"))
r04$party <- ifelse(r04$choice == "Kerry", "Democrat", ifelse(r04$choice == "Bush", "Republican", "Other"))
r08$party <- ifelse(r08$choice == "Obama", "Democrat", ifelse(r08$choice == "McCain", "Republican", "Other"))
r12$party <- ifelse(r12$choice == "Obama", "Democrat", ifelse(r12$choice == "Romney", "Republican", "Other"))

# finally, bind the data by rows to get a single database with all five years

res <- rbind(r00, r04, r08, r12)

# the main graph 

library(ggplot2)
ggplot(subset(res, State == "OH"), aes(year, percent)) +
  geom_point(stat = "identity", aes(colour = party)) +
  geom_line(aes(group = party, colour = party)) +
  scale_colour_manual(values = c("#0066cc", "#008000", "#cc0000")) +
  ylim(c(0,100)) +
  ggtitle("Presidential Candidate Votes By Year")

# creating a new dataframe that will work well with the data table (i'm not awesome with datatable manipulation, any easier mods welcome)

library(xtable)
res2 <- res
res2$xx <- paste(res2$State, res2$year)
res2 <- subset(res2, !duplicated(res2$xx))
my.vars = c("State", "year", "Total.Votes")

res2 <- res2[my.vars]

rownames(res) <- 1:nrow(res)
rownames(res2) <- 1:nrow(res2)

# update! i later figured out why all my id column was turning out weird after reshaping data frames. you can actually specify normal data row names when using the reshape() code. i just ignored that the first 48+ times i've used the code

#fixing calculation error in data
#replaced with data from http://www.fec.gov/pubrec/fe2000/prespop.htm
r00$Total.Votes <- ifelse(r00$State == "All", 105405100, r00$Total.Votes)
res$Total.Votes <- ifelse((res$State == "All" & res$year == "2000"), 105405100, res$Total.Votes)
res2$Total.Votes <- ifelse((res2$State == "All" & res2$year == "2000"), 105405100, res2$Total.Votes)

# aaaand save that data for the shiny app
save(r00, r04, r08, r12, res, res2, file = "~/pastpolls/pastResults.rdata")
