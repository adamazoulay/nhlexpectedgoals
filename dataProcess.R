library(nhlscrape)

processed_data <- function(team_ids, gids) {
  # Start by creating the final data frame
  data <- data.frame(Goal=logical(),
                     ShotType=character(),
                     ShotDistance=double(),
                     ShotAngle=double(),
                     Rebound=logical(),
                     Rush=logical(),
                     #TODO PlayerAdvantage=logical(),
                     #TODO GoaliePct=double(),
                     #TODO EmptyNet=logical(),
                     PreviousShots=integer(),
                     stringsAsFactors = FALSE)

  for (i in 1:length(team_ids)) {
    team_id <- team_ids[[i]]
    print(team_id)
    for (game_id in gids[[i]]) {
      query <- paste("SELECT * FROM events WHERE game_id=", game_id, " AND team_id=", team_id,
                     " GROUP BY result_eventCode ORDER BY about_period, about_periodTime ASC", sep="")
      results <- QueryDb(query)

      # We also must transform the x coordinates of the shots so they are all pointing left
      query <- paste("SELECT coordinates_x FROM events WHERE game_id=", game_id, " AND team_id=", team_id,
                     " AND (result_eventTypeId='SHOT' OR result_eventTypeId='GOAL') AND (about_period=1 OR about_period=3)", sep = "")
      row <- QueryDb(query)
      avg = mean(row$coordinates_x)

      if (avg < 0) {
        flip <- TRUE
      } else {
        flip <- FALSE
      }

      for (i in 1:nrow(results)) {
        if (flip && results[i,]$about_period %% 2 == 0) {
          # Flip second period
          results[i,]$coordinates_x <- -1 * results[i,]$coordinates_x
        } else if (!flip && results[i,]$about_period %% 2 == 1) {
          # Flip first and last period
          results[i,]$coordinates_x <- -1 * results[i,]$coordinates_x
        }
      }



      total_shots = 0
      for (rownum in 1:length(results[,1])){
        cur <- results[rownum, ]
        # Make sure we are at a shot/goal event
        if (!(cur$result_eventTypeId == "SHOT" || cur$result_eventTypeId == "GOAL")) {
          next
        }

        # Goal
        Goal <- (cur$result_eventTypeId == "GOAL")

        # Shot type
        ShotType <- cur$result_secondaryType

        # Shot distance
        # Net is at (-89, 0), so find the distance from the middle of the net
        x <- cur$coordinates_x
        y <- cur$coordinates_y
        dx <- (x + 89)
        dy <- y
        ShotDistance <- sqrt(dx^2 + dy^2)

        # Shot angle
        # Reference the diagram, theta off the line perpendicular to the goal mouth
        ShotAngle <- (atan(dy/dx) * 180)/pi #Degrees

        # Rebound?
        # Let's define this as a shot following another shot in the last 7 seconds
        rebound_time <- 5 #s
        if (rownum == 1) {
          Rebound <- FALSE
        } else {
          previous <- results[rownum - 1,]
          prev_shot <- (previous$result_eventTypeId == "SHOT")

          time_prev <- as.POSIXct(previous$about_dateTime, format = '%Y-%m-%dT%H:%M:%SZ')
          time_cur <- as.POSIXct(cur$about_dateTime, format = '%Y-%m-%dT%H:%M:%SZ')
          diff <- as.integer(difftime(time_cur, time_prev, units="secs"))
          time_bool <- (diff <= rebound_time)

          if (time_bool && prev_shot) {
            Rebound <- TRUE
          } else {
            Rebound <- FALSE
          }
        }

        # Rush
        # Let's say ~10s from previous event to shot/goal
        # We must also start from d-zone side of center
        rush_speed <- 10 #s
        if (rownum == 1) {
          Rush <- FALSE
        } else {
          previous <- results[rownum - 1,]
          prev_x <- previous$coordinates_x
          cur_x <- cur$coordinates_x

          time_prev <- as.POSIXct(previous$about_dateTime, format = '%Y-%m-%dT%H:%M:%SZ')
          time_cur <- as.POSIXct(cur$about_dateTime, format = '%Y-%m-%dT%H:%M:%SZ')
          diff <- as.integer(difftime(time_cur, time_prev, units="secs"))

          if (diff < rush_speed) {
            Rush <- TRUE
          } else {
            Rush <- FALSE
          }
          # On d-zone side, shot from inside the o-zone
          if (is.na(prev_x) || is.na(cur_x) ||  prev_x < 0 || cur_x > -50) {
            Rush <- FALSE
          }
        }

        # Previous shots
        PreviousShots <-  total_shots
        total_shots <- total_shots + 1

        data <- rbind(data, data.frame(Goal, ShotType, ShotDistance, ShotAngle, Rebound, Rush, PreviousShots, stringsAsFactors = FALSE))
      }
    }
  }
  return(data)
}
