# datacleaning.R -----------------------------------

# prep_shooting_data <- function(df, shot_type){
#   # returns a formatted df for a single shot type
#   made_name <- paste(shot_type,"Made")
#   att_name <- paste(shot_type,"Attempted")
#   
#   num_made <- df
#   
# }

make_shooting_data <- function(main_df){
  # returns a data frame with a row for every single shot
  all_shots <- data.frame(Player = c(), Shot_Type = c(), Result = c())
  
  
  for(i in 1:nrow(main_df)){
    # field goals
    att <- main_df$`FG Attempted`[i]
    made <- main_df$`FG Made`[i]
    name_vec <- rep(main_df$Player[i], att)
    shot_vec <- rep('FG', att)
    # make_miss <- c(rep("Made",made),rep("Missed",att - made))
    make_miss <- c(rep("Missed",att - made),rep("Made",made))
    
    
    temp_df <- data.frame(Player = name_vec,
                          Shot_Type = shot_vec,
                          Result = make_miss)
    all_shots <- rbind(all_shots, temp_df)
    
    # free throws
    att <- main_df$`FT Attempted`[i]
    made <- main_df$`FT Made`[i]
    name_vec <- rep(main_df$Player[i], att)
    shot_vec <- rep('FT', att)
    make_miss <- c(rep("Missed",att - made),rep("Made",made))
    
    temp_df <- data.frame(Player = name_vec,
                          Shot_Type = shot_vec,
                          Result = make_miss)
    all_shots <- rbind(all_shots, temp_df)
    
    # three pointers
    att <- main_df$`3PT Attempted`[i]
    made <- main_df$`3PT Made`[i]
    name_vec <- rep(main_df$Player[i], att)
    shot_vec <- rep('3PT', att)
    make_miss <- c(rep("Missed",att - made),rep("Made",made))
    
    temp_df <- data.frame(Player = name_vec,
                          Shot_Type = shot_vec,
                          Result = make_miss)
    all_shots <- rbind(all_shots, temp_df)
    
    
    
    
    
    
  }
  all_shots
  
}


