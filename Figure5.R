

require(tidyverse)
require(cowplot)

######RATIOS WITH CHANGING RESPIRATION#####

results <- vector()
ms <- vector()
ks <- vector()

ratios <- seq(1,5,by = 0.1)
score <- vector()

respiration.ratio <- seq(0,1, by = 0.01)
net.change.trichome <- vector()
net.change.colony <- vector()

for(m in 1:length(ratios)){
  for(k in 1:length(ratios)){
    for(r in 1:length(respiration.ratio)){
      ######SINGLE TRICHOME
      
      carbon.fixation.rate <- 0.015
      nitrogen.fixation.rate  <- carbon.fixation.rate/ratios[m]
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
      nitrogen.fixation.rate  <- carbon.fixation.rate/ratios[k]
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
    
    
    
    score <- 100*(length(which(net.change.colony > net.change.trichome))/length(net.change.colony))
    
    results <- c(results,score)
    ms <- c(ms,ratios[m])
    ks <- c(ks,ratios[k])
    
  }
  
  print(100*m/length(ratios))
}


plotdf <- as.data.frame(cbind(ms/ks,results))

plotdf <- plotdf %>% group_by(V1) %>% summarise_all(funs(mean(.,na.rm=TRUE)))

ggplot(plotdf, aes(x = V1, y = results)) +
  geom_hline(yintercept = 0)+
  geom_point(size = 2) +
  geom_vline(size = 2, xintercept = 0.8, color = "blue")+
  theme(panel.background = element_blank(),plot.title = element_text(size=16),
        axis.text = element_text(size=14), axis.title = element_text(size=16),
        panel.border = element_rect(fill = NA, linetype = "solid")) + 
  labs(y = "% Colony Success", x = expression(paste(tau[trichome],"/",tau[colony]))) +
  scale_y_continuous(expand = c(0.1, 0.1))

