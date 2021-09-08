library(tidyverse)
rep <- 500
n <- 2
  

sim_tcl <- function(rep,n){
  sim <- map(1:rep, function(x){
    mean(sample(1:6, n, replace = T))    
  })

  sim %>% unlist() %>% data.frame() %>% set_names("val") %>% 
    ggplot(aes(x = val))+
    geom_bar()+
    theme_bw()
}

sim_tcl(50000, 25)


all_sims <- function(rep, n){
  sim <- map(1:rep, function(x){
    mean(sample(1:6, n, replace = T))    
  })
  
  sample(c(1,2,3), 3, replace = T, prob = c(.1,.3,.6))  
}







