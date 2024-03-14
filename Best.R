# Question 2
##finding the best hospital in state

library(tidyverse)


best <- function(state, outcome) {
        ## Reading the outcome data
        data_outcome <- read.csv("~/School/WNTR 2024/R Programming/Week 4/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        head(outcome)
        #Checking to see if variables are valid
        ## Create a unique list of STATE in this dataset.
        states_outcome <- base::unique(data_outcome$State)
        
        ## List of outcomes
        list_disease <- c("heart attack", "heart failure", "pneumonia")
        
        ## Valid STATE and Valid OUTCOME
        if ((state %in% states_outcome) & (outcome %in% list_disease)) {
                # Subsetting the raw data.
                data_tidy <- data_outcome %>% select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
                
                # Renaming columns to be readable.
                colnames(data_tidy) <- c("hospital_name", "hospital_state","heart_attack", "heart_failure", "pneumonia")
                
                # Uniforming the names with underscore to match during the select.
                outcome_ <- sub(pattern = " ", replacement = "_", x = outcome)
                
                # Converting character into numeric.
                data_tidy$heart_attack <- as.numeric(data_tidy$heart_attack)
                data_tidy$heart_failure <- as.numeric(data_tidy$heart_failure)
                data_tidy$pneumonia <- as.numeric(data_tidy$pneumonia)
                
                data_tidy %>%
                        select(hospital_name,hospital_state,all_of(outcome_)) %>%
                        filter(hospital_state == state) %>%
                        na.omit() %>% select(-hospital_state) %>%
                        arrange(across(all_of(outcome_)), hospital_name) %>%
                        head(1) %>%
                        select(hospital_name) %>%
                        as.character() -> message
        }
        
        # Something is wrong with the inputs
        # The state input is not a valid one
        if (!(state %in% states_outcome) & (outcome %in% list_disease)) {
                return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid state', state, outcome)))
        }
        
        # The outcome is not valid.
        if ((state %in% states_outcome) & !(outcome %in% list_disease)) {
                return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid outcome', state, outcome)))
        }
        
        # Both state and outcome has typos
        if (!(state %in% states_outcome) & !(outcome %in% list_disease)) {
                return(base::cat(base::sprintf('Error in best("%s", "%s") : invalid state and outcome', state, outcome)))
        }
        
        return(message)
        
}

# Testing function 
best("AL", "heart attack")
best("DC", "pneumonia")

