counts <- 0
num_runs <- 100000
num_years <- (2050-2022) #time span within which event can happend
prob <- 1*(.004) #independent probability of event
prob_to_int <- round(1/prob,digits = 0)

for (i in 1:num_runs) {
  event_vector <- sample(1:prob_to_int,num_years,replace = T)
  event_number <- sample(1:prob_to_int,1)
  
  
  if (event_number %in% event_vector) {
    counts <- counts +1
  }
  
}

counts/num_runs


