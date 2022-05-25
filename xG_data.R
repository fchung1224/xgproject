rm(list = ls())

### ------          00. Libraries               ------------

if(!require("jsonlite")){install.packages("jsonlite")}

library(jsonlite)
library(tidyverse)
library(janitor)

### ------          01. Data              ------------

# Players data
players <- fromJSON(txt = "dataset/players.json") %>% as.data.frame() %>% 
  clean_names()
glimpse(players)

# Events data

ev <- list.files("dataset/events")
ev <- ev[!str_detect(ev, c("World"))]
ev <- ev[!str_detect(ev, c("European"))]
events <- NULL

for(i in 1:length(ev)){
  file <- ev[i]
  league <- sub("*events_", "", file)
  league <- sub(".json*", "", league)
  print(str_c("Loading: ", league))
  df <- fromJSON(txt = str_c("dataset/events/", file)) %>% as.data.frame() %>% 
    clean_names() %>% 
    mutate(league = league)
  
  events <- rbind(events, df)
  print(str_c("League loaded: ", league))
  
}
#save(events, file = "events.RData")
rm(list = c("df", "league", "i", "ev", "file"))

events %>% select(event_id, event_name) %>% distinct() %>% 
  arrange(event_id)



## Events glossary
# 1. Duel - A challenge between two players to gain control of the ball, progress with the ball or change its direction.
#           it is always a paired event (can be offensive, defensive, open play)
# 2. Foul - An offence committed by a player according to law 12 (1, 3) of the IFAB Laws of the Game.
# 3. Free kick 
# 4. Goalkeeper leaving line - An attempt from the part of the goalkeeper to actively play a high Cross or a 
#                             long aerial pass in the air, either to claim or to punch the ball.
# 5. Interruption - 
# 6. Offside - An offside as described in the law 11 of the IFAB Laws of the Game.
# 7. Others on the ball
# 8. Pass


# For each goal, we are going to get the type of pass it was preceded from 
# as well as the position were the pass took place

events %>% 
  filter(event_id == 8) %>% 
  select(sub_event_id, sub_event_name) %>% 
  distinct() %>% 
  arrange(sub_event_id)

events %>% 
  filter(event_id == 8) %>% 
  pull(tags) %>% 
  unlist() %>% unique() %>% 
  sort()

# Tags on passes: (https://support.wyscout.com/matches-wyid-events)
# 101: goal
# 102: own goal
# 201:
# 301: assist
# 302: keyPass
# 401: left foot
# 402: right foot
# 403: head/body
# 502: 
# 504:
# 801: high
# 802 : low
# 901: through
# 1001: direct
# 1401: interception
# 1801: successful
# 1802 : not successful
# 2001: dangerous_ball_lost
# 2101: blocked


# We are going to filter every shot from the events dataset
# and unnest its tags (which will now have the column name "id)
# as well as its positions
shots <- events %>% 
  filter(event_id == 10)
names(shots)[12] <- "id_unique"
nrow(shots)

shots2 <- shots %>% unnest(positions)%>% mutate(pos = str_c(y, x, sep = "\t"))
npass <- rep(c("or","fin"), nrow(shots2)/2)
shots2 <- cbind(shots2, npass)
shots2 <- shots2 %>% select(-x, -y) %>% 
  spread(key = npass, value = pos) %>% 
  separate(or, into = c("y1", "x1"), sep = "\t") %>% 
  separate(fin, into = c("y2", "x2"), sep = "\t")

shots2 <- shots2 %>% unnest(tags)

# Now we will create an indicator function for goal
goals_id <- shots2 %>% filter(id == 101) %>% pull(id_unique) %>% unique()


# We will create another indicator to indicate whether or not the shot
# was taken from inside the are 
shots2 <- shots2 %>% 
  mutate(goal = ifelse(id_unique %in% goals_id, 1, 0),
         shot_area = case_when(
           x1 >= 84 & y1 >= 19 & y1 <= 81 ~ 1,
           TRUE ~ 0
         ))



# Finally, we will add the position from where the pass previous to that
# shot was made, as well as the type of pass

pass <- events %>% 
  filter(event_id == 8)
names(pass)[12] <- "id_unique"
nrow(pass)

pass2 <- pass %>% unnest(positions)%>% mutate(pos = str_c(y, x, sep = "\t"))
npass <- rep(c("or","fin"), nrow(pass2)/2)
pass2 <- cbind(pass2, npass)
pass2 <- pass2 %>% select(-x, -y) %>% 
  spread(key = npass, value = pos) %>% 
  separate(or, into = c("y1", "x1"), sep = "\t") %>% 
  separate(fin, into = c("y2", "x2"), sep = "\t")

pass2 <- unnest(pass2, tags)
save(pass2, file = "pass2.RData")

# We will select only the columns that work for us

pass3 <- pass2 %>% 
  select(id, match_id, event_name, 
         team_id, match_period, event_sec, sub_event_name, y1, x1, y2, x2)
names(pass3) <- str_c("pass_", names(pass3))

# We join the pass and shots tables
shots_final <- shots2 %>% 
  left_join(pass3, 
            by = c("match_id" = "pass_match_id",
                   "team_id" = "pass_team_id", 
                   "match_period" = "pass_match_period",
                   "y1" = "pass_y2",
                   "x1" = "pass_x2")) %>% 
  mutate(diff_time = event_sec - pass_event_sec) %>% 
  replace_na(list(diff_time = 0)) %>% 
  filter(abs(diff_time) < 6)

summary(shots_final$diff_time)
hist(shots_final$diff_time)

shots_final %>% filter(pass_id == 301) %>% 
  group_by(goal) %>% tally()

shots_final %>% filter(pass_id == 301, goal == 0) %>% View()

assist_id <- shots_final %>% filter(pass_id == 301) %>% pull(id_unique)

shots_final <- shots_final %>% 
  mutate(goal = if_else(id_unique %in% assist_id, 1, goal))
shots_final %>% filter(pass_id == 301) %>% 
  group_by(goal) %>% tally()

# There should be 4947 goals in total (might be closed play goals)
shots_final %>% filter(goal == 1) %>% pull(id_unique) %>% n_distinct()

save(shots_final, file = "shots_final.RData")
