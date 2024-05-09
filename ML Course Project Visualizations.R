#---------------------------------------------------
#---------------------------------------------------
#----- ML Final Project - William Forman
#---------------------------------------------------
#---------------------------------------------------


##-------------------------------------
##----- Step 1: prepare the environment
##-------------------------------------

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console

# Prepare needed libraries and load data
packages <- c("ISLR" # Datasets that are used in our textbook
              , "stargazer" # Nice summary tables
              , "ggplot2"   # Best plotting
              , "dplyr"
              )
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only=TRUE)
}
rm(packages)

load(file = '/Users/williamforman/Desktop/Desktop/School/College/Senior/ML/Final Proj/nba.full.RData')
View(nba)

#---------------------------------------------------
#----- Step 2: Basic visualizations
#---------------------------------------------------

# Histogram

####### I THINK THIS IS WRONG - JUST THE DISTRIBUTION OF NBA PLAYERS BY AGE
# ggplot(nba, aes(x = age)) +
#   geom_histogram(bins = 50
#                  , color = "darkblue"
#                  , fill = "lightblue"
#   ) +
#   # Adjust y-axis ticks
#   scale_y_continuous(breaks = seq(from = 0        
#                                   , to = 40000000
#                                   , by = 500000
#   )
#   ) +
#   # Adjust x-axis ticks
#   scale_x_continuous(breaks = seq(from = 15        
#                                   , to = 45
#                                   , by = 5
#   )
#   ) +
#   labs(title = "Salary vs. Age of NBA Players, 2000-2019"
#        , subtitle = ""
#        , x = "Age of NBA player"
#        , y = "Salary of NBA player"
#   ) +
#   # Apply black-and-white theme
#   theme_bw()

#----- Scatter Plots for Salary

# Age
ggplot(nba, aes(x = age, y = salary)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "lightblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  labs(title = "Salary vs. Age of NBA Players, 2000-2019"
       , subtitle = ""
       , x = "Age (Years)"
       , y = "Salary (Dollars)"
  ) +
  theme_bw()

# Year (year in which season ends since seasons are Oct-Jun)
ggplot(nba, aes(x = yrend, y = salary/1000000)) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , shape = 21
             , size = 1
             , alpha = 1
  ) +
  labs(title = "Salary vs. Year of Season of NBA Players, 2000-2019"
       , subtitle = ""
       , x = "Points Per Game"
       , y = "Salary (Millions of Dollars)"
  ) +
  theme_bw()

# PPG
ggplot(nba, aes(x = pts, y = salary/1000000)) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , shape = 21
             , size = 1
             , alpha = 1
  ) +
  labs(title = "Salary vs. PPG of NBA Players, 2000-2019"
       , subtitle = ""
       , x = "Points Per Game"
       , y = "Salary (Millions of Dollars)"
  ) +
  theme_bw()

#----- Regressions for Salary

# Total Team Salaries by Year End
slr <- lm(formula = salary.team.total/1000000 ~ yrend, data = nba)

ggplot(data = nba, aes(x = yrend, y = salary.team.total/1000000)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "lightblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  geom_abline(intercept = coefficients(slr)[1]
              , slope = coefficients(slr)[2]
              , color = "lightblue", size = 1.1
  ) + 
  labs(title = "Team NBA Team Salary Increase Over Time, 2000-2019"
       , subtitle = ""
       , x = "Year of Season End"
       , y = "Total Team Salary (Million Dollars)"
  ) +
  theme_bw()

# Player Salaries by Minutes Played
slr <- lm(formula = salary ~ mp, data = nba)

ggplot(data = nba, aes(x = mp, y = salary)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "lightblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  geom_abline(intercept = coefficients(slr)[1]
              , slope = coefficients(slr)[2]
              , color = "lightblue", size = 1.1
  ) + 
  labs(title = "Player Salaries by Minutes Played, 2000-2019"
       , subtitle = ""
       , x = "Minutes Played"
       , y = "Player Salary"
  ) +
  theme_bw()

# Player Salaries by Minutes Played, 2019
nba.2019 <- nba[nba$yrend == 2019,]

slr <- lm(formula = salary ~ mp, data = nba.2019)

ggplot(data = nba.2019, aes(x = mp, y = salary)) +
  geom_point(color = "darkgreen"  # Color of points' outline
             , fill = "lightblue"       # Color of points' fill
             , shape = 21         # Shape of points
             , size = 2           # Size of points
             , alpha = 1          # Transparency of points
  ) +
  geom_abline(intercept = coefficients(slr)[1]
              , slope = coefficients(slr)[2]
              , color = "lightblue", size = 1.1
  ) + 
  labs(title = "Player Salaries by Minutes Played, 2019"
       , subtitle = ""
       , x = "Minutes Played"
       , y = "Player Salary"
  ) +
  theme_bw()

