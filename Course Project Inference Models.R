# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 5) # Remove scientific notation for numbers
# Prepare needed libraries
packages <- c("ggplot2"     # Best plotting
              , "gridExtra" # Arrange multiple plots in a grid
              , "leaps"     # Best subset selection
              , "ISLR"      # Textbook datasets
              , "stargazer" # Nice output tables
              , "glmnet"    # Ridge and Lasso
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only = TRUE)
}
rm(packages)

load("/Users/williamforman/Desktop/Desktop/School/College/Senior/ML/Final Proj/nba.full.RData")
nba <- subset(nba, select = -c(salary.lead, playerid, 
                               teamfullsal, teamid, player, season))

##-------------------------------------
##----- Inference models
##-------------------------------------

nba$salary.pct <- (nba$salary / nba$salary.team.total)
nba$age.sq <- nba$age^2
nba$pts.sq <- nba$pts^2
nba$gs.sq <- nba$gs^2

mlr.fg3.mp <- lm(nba$salary.pct ~ pts + age + fg3.pct + gs + mp
           , data = nba
) 

mlr1 <- lm(nba$salary.pct ~ pts + age + gs
           , data = nba
) 

mlr2 <- lm(nba$salary.pct ~ pts + age.sq + gs
           , data = nba
) 

mlr3 <- lm(nba$salary.pct ~ pts.sq + age + gs
           , data = nba
) 

mlr4 <- lm(nba$salary.pct ~ pts + age + gs.sq
           , data = nba
)

regs <- list(mlr.fg3.mp, mlr1, mlr2, mlr3, mlr4)
stargazer(mlr4, type = "html", style = "default", digits = 4
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE,
          out = "mlrs.html"
)

summary(mlr1)
summary(mlr2)
summary(mlr3)
summary(mlr4)
summary(mlr.fg3.mp)