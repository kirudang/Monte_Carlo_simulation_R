# Monte Carlo Simulation ----
############# Lottery Winner #############

# Step 1 - List all possible outcomes.
# Step 2 - Determine the probabilities.
# Step 3 - Set up a correspondence between the random numbers and the outcome.
# Step 4 – Conduct experiment.
# Create a function to get result
Random_Number_List <- c() # Create an empty vector to append the result
Lottery <- function() 
{     b <- FALSE # Set default False for all outcome
      i <- FALSE # Set default False for all outcome
      g <- FALSE # Set default False for all outcome
      Total_Pick <- 0 # Number of Pick is 0
      # While loop ( Condition is TRUE) {Command}
      while(!b | !i | !g) # Only stop when temporary values of b, i, g holing TRUE at the same time
          {
            Total_Pick <- Total_Pick + 1 # Count the number of Pick
            Random_Number <- sample(0:9, 1) # Get random number from 0 to 9 with 1 unit space
            Random_Number_List <- append(Random_Number_List, Random_Number) #  Record the random number
            Random_Number_List <- paste(Random_Number_List, collapse = ", ") # Convert to character to illustrate the consequence of number
            if(Random_Number <= 5){b <- TRUE} # Assign value to random number for b
            else if(Random_Number <= 8){i <- TRUE} # Assign value to random number for i
            else {g <- TRUE} # Assign value to random number for g
            Result <- data.frame(Random_Number_List = Random_Number_List,
                                 Total_Pick = Total_Pick) # Data frame the result
          }
      return(Result)
}

Lottery()


df <- data.frame(Random_Number_List = character(),
                 Total_Pick = integer(),
                 stringsAsFactors = FALSE) # Empty data frame to record result of each trial
Experiment <- function(n)                  # Function with input n
{trial <- c(1:n)                           # Vector of trial from 1 to n
    for( k in trial)                     
    {if(k<=n)                              # Repeat calling function n times
      {Random_Number_List <- c()           # Clear the list from last record
       Result <- Lottery()                 # Run Lottery function
      }
    df <- rbind(df,Result)                 # Append result for each trial
    Sum_Trial <- sum(df$Total_Pick)        # Sum of picks of total trial
    Average_Pick <- Sum_Trial/n            # Average pick
    }
return( list( data = df,                   # Create a list of return
              Sum_Trial = Sum_Trial,
              Average_Pick = Average_Pick))
}

# Experiment with 30 times
set.seed(333)
exp1 <- Experiment(30)
exp1$data
exp1$Average_Pick
# Visualization
hist(exp1$data$Total_Pick, 
     main = "Histogram for number of pick in the experiment",
     xlab = "Pick per trial")
psych::describe(exp1$data$Total_Pick)

# Experiments
exp2 <- Experiment(1000)
exp2$Average_Pick
exp3 <- Experiment(2000)
exp3$Average_Pick
exp4 <- Experiment(3000)
exp4$Average_Pick
exp5 <- Experiment(5000)
exp5$Average_Pick


