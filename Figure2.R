

require(tidyverse)
require(cowplot)

#####CHANGING RESPIRATION#######

respiration.ratio <- seq(0,1, by = 0.001)
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(respiration.ratio)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio[r]*carbon.fixation.rate
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
  respiration.rate <- respiration.ratio[r]*carbon.fixation.rate
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


colony <- data.frame(respiration.ratio*100, net.change.colony,"Colony")
trichome <- data.frame(respiration.ratio*100, net.change.trichome,"Free trichome")
colnames(colony) <- c("Ratio","Change","Group")
colnames(trichome) <- c("Ratio","Change","Group")

plotdf <- rbind(colony,trichome)

ggplot(plotdf, aes(x = Ratio, y = Change, color = Group)) +
  geom_line(size = 2, linetype = "solid") +
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.7,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(y = expression(paste(Delta,"C"["sto"], "(mol C" ," mol C"^"-1",")")), x = "Respiration ratio (% C-fixation)")+
  scale_y_continuous(expand = c(0.1, 0.1)) + scale_colour_discrete(labels=c("Colony", "Free trichome"),type = c("darkgreen","grey"))

