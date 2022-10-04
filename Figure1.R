

require(tidyverse)
require(cowplot)

#####FIXED AND EQUAL RESPIRATION#######

respiration.ratio <- seq(0,0.02, by = 0.0005)
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(respiration.ratio)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio[r]
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio[r]
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
  
}


colony <- data.frame(respiration.ratio, net.change.colony,"Colony")
trichome <- data.frame(respiration.ratio, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")
plotdf <- rbind(colony,trichome)

plota <- ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.7,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = expression(paste("Respiration rate (mol C ","hr"^-1," mol C"^-1,")")))+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))


#####FIXED AND UNEQUAL RESPIRATION#######

colony.success <- vector()

respiration.ratio <- runif(n = 500, min = 0, max = 0.02)
theta <- seq(0.01,1, by = 0.01)
net.change.trichome <- vector()
net.change.colony <- vector()

for(th in 1:length(theta)){
  for(r in 1:length(respiration.ratio)){
  
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio[r]
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio[r]*theta[th]
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
    
  } 
  
  colony.success[th] <- 100*(length(which(net.change.colony > net.change.trichome))/length(net.change.colony))
  
  print(th/length(theta))
}



plotdf <- data.frame(theta,colony.success)
colnames(plotdf) <- c("Theta","Success")

plotb <- ggplot(plotdf, aes(x = Theta, y = Success)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.7,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(y = "% Colony Success", x = expression(paste(theta, " (F"[resCol],"/","F"[resTri],")")))


#####PLOT FIGURE 1#####
plot_grid(plota,plotb, ncol = 1, labels = c("(a)","(b)"), label_x = 0.9, label_y = 0.95)
