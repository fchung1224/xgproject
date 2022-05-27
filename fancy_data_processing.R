
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
    unnest_wider(x, names_sep = "") %>%
    dplyr::select(-c(x1, y1))
  
  passes_ok <- passes %>%
    dplyr::select(matchId, teamId, playerId, eventSec, matchPeriod, id) %>%
    bind_cols(pos, tags2) %>%
    filter(is_blocked == 0,
           is_own_goal == 0, 
           is_intercepted == 0) %>%
    dplyr::select(-starts_with("tags_")) %>%
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
           skilled_foot = ifelse(body_part == "head/body", body_part,
                                 ifelse(body_part == foot, "Yes", "No")),
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
           skilled_foot = ifelse(body_part == "head/body", body_part,
                                 ifelse(body_part == foot, "Yes", "No")),
           x_meter = x2 * 105/100,
           y_meter = y2 * 68/100) %>%
    filter(!is.na(skilled_foot)) %>% 
    select(matchId, teamId, matchPeriod,  y2, x2, eventSec, starts_with("is"),
           foot, eventSec2, skilled_foot, x_meter, y_meter)
  
  
  names(passesB)[6:ncol(passesB)] <- str_c("passes_",names(passesB)[6:ncol(passesB)])
  
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
    ungroup() 
  
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


final %>% 
  group_by(is_goal, passes_is_assist) %>% tally()


apply(final, 2, function(x)sum(is.na(x)))

# We will make an indicator to express when the shot has a previous pass
final$passes_na <- apply(final %>% select(starts_with("passes")), 1, function(x)sum(is.na(x)))


save(final, file = "processed_data/FINAL_DATA.RData")


sum(final$is_goal)


