
get_shots <- function(file_path, name_detail, save_files = F){
  
  players <- jsonlite::fromJSON("dataset/players.json")
  
  shots <- jsonlite::fromJSON(file_path) %>%
    filter(subEventName == "Shot")
  
  tags <- tibble(tags = shots$tags) %>%
    hoist(tags, 
          tags_id = "id") %>%
    unnest_wider(tags_id, names_sep = "")
  
  tags2 <- tags %>%
    mutate(is_goal = ifelse(rowSums(. == "101", na.rm = T) > 0, 1, 0),
           is_blocked = ifelse(rowSums(. == "2101", na.rm = T) > 0, 1, 0),
           is_CA = ifelse(rowSums(. == "1901", na.rm = T) > 0, 1, 0),
           body_part = ifelse(rowSums(. == "401", na.rm = T) > 0, "left", 
                              ifelse(rowSums(. == "402", na.rm = T) > 0, "right", 
                                     ifelse(rowSums(. == "403", na.rm = T) > 0, "head/body", "NA"))))
  
  pos <- tibble(positions = shots$positions) %>%
    hoist(positions, 
          y = "y",
          x = "x") %>%
    unnest_wider(y, names_sep = "") %>%
    unnest_wider(x, names_sep = "") %>%
    dplyr::select(-c(x2, y2))
  
  shots_ok <- shots %>%
    dplyr::select(matchId, teamId, playerId, eventSec, matchPeriod, id) %>%
    bind_cols(pos, tags2) %>%
    filter(is_blocked == 0) %>%
    dplyr::select(-c(9:14)) %>%
    left_join(players %>%
                dplyr::select(c("wyId", "foot")), by = c("playerId" = "wyId")) %>%
    mutate(league = name_detail)
  
  if(save_files){
    dir.create("processed_data/shots")
    write_rds(shots, paste0("processed_data/shots/shots", name_detail, ".rds"))
    write_rds(tags2, paste0("processed_data/shots/tags2", name_detail, ".rds"))
    write_rds(pos, paste0("processed_data/shots/pos", name_detail, ".rds"))
    write_rds(shots_ok, paste0("processed_data/shots/unblocked_shots", name_detail, ".rds"))
  }
  
  shots_ok
}

get_passes <- function(file_path, name_detail, save_files = F){
  
  players <- jsonlite::fromJSON("dataset/players.json")
  
  passes <- jsonlite::fromJSON(file_path) %>%
    filter(eventName == "Pass")
  
  tags <- tibble(tags = passes$tags) %>%
    hoist(tags, 
          tags_id = "id") %>%
    unnest_wider(tags_id, names_sep = "")
  
  tags2 <- tags %>%
    mutate(
      is_own_goal = ifelse(rowSums(. == 102, na.rm = T) > 0, 1, 0),
      is_assist = ifelse(rowSums(. == 301, na.rm = T) > 0, 1, 0),
      is_key_Pass = ifelse(rowSums(. == 302, na.rm = T) > 0, 1, 0),
      is_own_goal = ifelse(rowSums(. == 102, na.rm = T) > 0, 1, 0),
      height_pass = ifelse(rowSums(. == 801, na.rm = T) > 0, "high", 
                           ifelse(rowSums(. == 802, na.rm = T) > 0, "low", "NA")),
      is_through = ifelse(rowSums(. == 901, na.rm = T) > 0, 1, 0),
      is_intercepted = ifelse(rowSums(. == 1401, na.rm = T) > 0, 1, 0),
      is_accurate = ifelse(rowSums(. == 1801, na.rm = T) > 0, 1,
                           ifelse(rowSums(. == 1802, na.rm = T) > 0, 0, "NA")),
      is_blocked = ifelse(rowSums(. == 2101, na.rm = T) > 0, 1, 0),
      is_CA = ifelse(rowSums(. == 1901, na.rm = T) > 0, 1, 0),
      body_part = ifelse(rowSums(. == 401, na.rm = T) > 0, "left", 
                         ifelse(rowSums(. == 402, na.rm = T) > 0, "right", 
                                ifelse(rowSums(. == 403, na.rm = T) > 0, "head/body", "NA"))))
  
  pos <- tibble(positions = passes$positions) %>%
    hoist(positions, 
          y = "y",
          x = "x") %>%
    unnest_wider(y, names_sep = "") %>%
    unnest_wider(x, names_sep = "")
  
  passes_ok <- passes %>%
    dplyr::select(matchId, teamId, playerId, eventSec, matchPeriod, id, subEventName) %>%
    bind_cols(pos, tags2) %>%
    filter(is_blocked == 0,
           is_own_goal == 0, 
           is_intercepted == 0) %>%
    dplyr::select(-starts_with("tags_"), -is_blocked, -is_own_goal, -is_intercepted,
                  - is_accurate, -height_pass) %>%
    left_join(players %>%
                dplyr::select(c("wyId", "foot")), by = c("playerId" = "wyId")) %>%
    mutate(league = name_detail)
  
  
  
  if(save_files){
    dir.create("processed_data/passes/")
    write_rds(shots, paste0("processed_data/passes/passes", name_detail, ".rds"))
    write_rds(tags2, paste0("processed_data/passes/tags2", name_detail, ".rds"))
    write_rds(pos, paste0("processed_data/passes/pos", name_detail, ".rds"))
    write_rds(passes_ok, paste0("processed_data/passes/successful_passes", name_detail, ".rds"))
  }
  
  passes_ok
}


get_shots_and_passes <- function(file_path, name_detail, save_filestop = F){

  
  shots <- get_shots(file_path, name_detail)
  passes <- get_passes(file_path, name_detail)
  
  
  shotsB <- shots %>%
    arrange(matchId, matchPeriod, teamId, eventSec) %>%
    mutate(eventSec2 = ifelse(matchPeriod == "2H", eventSec + 2700, eventSec),
           time_prev = ifelse(matchId == lag(matchId) & matchPeriod == lag(matchPeriod) & teamId == lag(teamId), eventSec - lag(eventSec), -1),
           time_prev = ifelse(is.na(time_prev), -1, time_prev),
           skilled_foot = 
             case_when(
               body_part == "head/body" ~ body_part,
               body_part == foot ~ "Yes",
               foot == "both" ~ "Yes", 
               TRUE ~ "No"
             ),
           x_meter = x1 * 105/100,
           y_meter = y1 * 68/100,
           distance_to_goal_line = sqrt((105 - x_meter)^2 + (32.5 - y_meter)^2),
           angle_to_goal = atan( (7.32 * (105 - x_meter) ) / ( (105 - x_meter)^2 + (32.5 - y_meter)^2 - (7.32/2)^2 )) * 180/pi) %>%
    filter(!is.na(skilled_foot))
  
  passesB <- passes %>%
    arrange(matchId, matchPeriod, teamId, eventSec) %>%
    mutate(eventSec2 = ifelse(matchPeriod == "2H", eventSec + 2700, eventSec),
           time_prev = ifelse(matchId == lag(matchId) & matchPeriod == lag(matchPeriod) & teamId == lag(teamId), eventSec - lag(eventSec), -1),
           time_prev = ifelse(is.na(time_prev), -1, time_prev),
           skilled_foot = 
             case_when(
               body_part == "head/body" ~ body_part,
               body_part == foot ~ "Yes",
               foot == "both" ~ "Yes", 
               TRUE ~ "No"
             ),
           x1_meter = x1 * 105/100,
           y1_meter = y1 * 68/100,
           x2_meter = x2 * 105/100,
           y2_meter = y2 * 68/100) %>%
    filter(!is.na(skilled_foot)) %>% 
    select(matchId, teamId, matchPeriod,subEventName,  y2, x2, x1, y1, eventSec, starts_with("is"),
           foot, eventSec2, skilled_foot,x2_meter, y2_meter, x1_meter, y1_meter)
  
  
  names(passesB)[7:ncol(passesB)] <- str_c("passes_",names(passesB)[7:ncol(passesB)])
  
  shots_final <- shotsB %>% 
    left_join(passesB, 
              by = c(
                "matchId" = "matchId", 
                "teamId" = "teamId",
                "x1" = "x2",
                "y1" = "y2",
                "matchPeriod" = "matchPeriod"
              )) %>% 
    group_by(id) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    mutate(keep = case_when(
           n == 1 ~ 1, 
           n > 1 & eventSec - passes_eventSec < 5 & eventSec - passes_eventSec >= 0 ~ 1,
           TRUE ~ 0)
           ) %>% filter(keep == 1) %>% 
    group_by(id) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n == 1) %>% 
    select(- passes_x2_meter, - passes_y2_meter, -n, -keep, - passes_foot, - foot, 
           -is_blocked, - body_part, - time_prev, -contains("Id"), -id, -passes_x1, -passes_y1, 
           -passes_eventSec, -n, -keep, -eventSec, -x1, -y1, -passes_eventSec2) %>% 
    mutate(passes_distance = sqrt((x_meter - passes_x1_meter)^2 + (y_meter - passes_y1_meter)^2),
           is_goal = case_when(
             is_goal == 0 & passes_is_assist == 1 ~ 1,
             TRUE ~ is_goal
           )) %>% 
    replace_na(
      list(
        subEventName = "No Pass",
        passes_is_assist = "No Pass",
        passes_is_key_Pass = "No Pass",
        passes_is_through = "No Pass",
        passes_is_CA = "No Pass",
        passes_skilled_foot = "No Pass",
        passes_x1_meter = "No Pass",
        passes_y1_meter = "No Pass", 
        passes_distance = "No Pass"
      )
    ) %>% select(-passes_is_assist)
  
  
  
  
    file <- str_c("processed_data/shots_final_", name_detail, ".RData")
  if(save_filestop){save(shots_final, file = file)}
  return(shots_final)
}


shotsEN <- get_shots_and_passes("dataset/events/events_England.json", "EN", T)
shotsFR <- get_shots_and_passes("dataset/events/events_France.json", "FR", T)
shotsGE <- get_shots_and_passes("dataset/events/events_Germany.json", "GE", T)
shotsIT <- get_shots_and_passes("dataset/events/events_Italy.json", "IT", T)
shotsSP <- get_shots_and_passes("dataset/events/events_Spain.json", "SP", T)


final <- rbind(shotsEN, shotsFR, shotsGE, shotsIT, shotsSP)





save(final, file = "processed_data/FINAL_DATA.RData")


sum(final$is_goal)

unique(final$skilled_foot)
unique(final$passes_skilled_foot)
