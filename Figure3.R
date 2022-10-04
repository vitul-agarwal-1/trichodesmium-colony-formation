

require(tidyverse)
require(cowplot)

######CHANGING INTERACTIONS######

interaction.strength <- seq(-2,2, by = 0.01)
respiration.ratio <- 0
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
  
}


colony <- data.frame(interaction.strength, net.change.colony,"Colony")
trichome <- data.frame(interaction.strength, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")
plotdf <- rbind(colony,trichome)

plot1 <- ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.3,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(title = "Respiration ratio = 0%",y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = expression(paste("Interaction strength ","(i"["p"],")")))+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))



interaction.strength <- seq(-2,2, by = 0.01)
respiration.ratio <- 0.3
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
  
}


colony <- data.frame(interaction.strength, net.change.colony,"Colony")
trichome <- data.frame(interaction.strength, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")
plotdf <- rbind(colony,trichome)

plot2 <- ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.3,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(title = "Respiration ratio = 30%",y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = expression(paste("Interaction strength ","(i"["p"],")")))+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))


interaction.strength <- seq(-2,2, by = 0.01)
respiration.ratio <- 0.6
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
  
}


colony <- data.frame(interaction.strength, net.change.colony,"Colony")
trichome <- data.frame(interaction.strength, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")
plotdf <- rbind(colony,trichome)

plot3 <- ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.3,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(title = "Respiration ratio = 60%",y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = expression(paste("Interaction strength ","(i"["p"],")")))+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))


interaction.strength <- seq(-2,2, by = 0.01)
respiration.ratio <- 0.9
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  interaction.rate <- interaction.strength[r]*(carbon.fixation.rate - nitrogen.fixation.rate)
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate +
    start.carbon
  
  
  for(i in t){
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.colony[r] <-  carbon.storage[720] - carbon.storage[1]
  
}


colony <- data.frame(interaction.strength, net.change.colony,"Colony")
trichome <- data.frame(interaction.strength, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")
plotdf <- rbind(colony,trichome)

plot4 <- ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.3,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(title = "Respiration ratio = 90%",y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = expression(paste("Interaction strength ","(i"["p"],")")))+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))


#####PLOT FIGURE 2#####
plot_grid(plot1,plot2,plot3,plot4, labels = c("(a)","(b)","(c)","(d)"), ncol = 2)
