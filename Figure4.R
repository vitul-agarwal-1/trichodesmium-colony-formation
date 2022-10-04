


require(tidyverse)
require(cowplot)

######CHANGING INTERACTIONS######

interaction.strength <- seq(-0.002,0.002, by = 0.0001)
respiration.ratio <- 0
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
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
plotdf <- data.frame(interaction.strength, trichome$Change - colony$Change, "Respiration ratio = 0%")
colnames(plotdf) <- c("Ratio","Change", "Model")

df1 <- plotdf


interaction.strength <- seq(-0.002,0.002, by = 0.0001)
respiration.ratio <- 0.3
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
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
plotdf <- data.frame(interaction.strength, trichome$Change - colony$Change, "Respiration ratio = 30%")
colnames(plotdf) <- c("Ratio","Change", "Model")

df2 <- plotdf


interaction.strength <- seq(-0.002,0.002, by = 0.0001)
respiration.ratio <- 0.6
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
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
plotdf <- data.frame(interaction.strength, trichome$Change - colony$Change, "Respiration ratio = 60%")
colnames(plotdf) <- c("Ratio","Change", "Model")

df3 <- plotdf



interaction.strength <- seq(-0.002,0.002, by = 0.0001)
respiration.ratio <- 0.9
net.change.trichome <- vector()
net.change.colony <- vector()

for(r in 1:length(interaction.strength)){
  ######SINGLE TRICHOME
  
  carbon.fixation.rate <- 0.015
  nitrogen.fixation.rate  <- 0.0075
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
    carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                            (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) + interaction.rate + 
      carbon.storage[i-1]
    
  }
  
  net.change.trichome[r] <- carbon.storage[720] - carbon.storage[1]
  
  
  ######COLONY
  
  carbon.fixation.rate <- 0.010
  nitrogen.fixation.rate  <- 0.004
  respiration.rate <- respiration.ratio*carbon.fixation.rate
  carbon.nitrogen.conversion <- 1
  n_to_c <- 0.159
  t <- 2:720
  start.carbon <- 100
  carbon.storage <- vector()
  carbon.storage[1] <- start.carbon
  
  i = 1
  
  carbon.storage[i] <- (carbon.fixation.rate  - respiration.rate - 
                          (nitrogen.fixation.rate*carbon.nitrogen.conversion*n_to_c)) +
    start.carbon
  
  
  for(i in t){
    
    interaction.rate <- interaction.strength[r]*(carbon.storage[i-1])
    
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
plotdf <- data.frame(interaction.strength, trichome$Change - colony$Change, "Respiration ratio = 90%")
colnames(plotdf) <- c("Ratio","Change", "Model")

df4 <- plotdf

plotdf <- rbind(df1,df2,df3,df4)

ggplot(plotdf, aes(x = Ratio, y = log10(Change+1), color = Model)) +
  geom_line(size = 1.5, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(panel.background = element_blank(),legend.background = element_blank(), legend.position = c(0.2,0.8), plot.title = element_text(size=16),
        legend.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=14),
        panel.border = element_rect(fill = NA, linetype = "solid"), legend.title = element_blank()) + 
  labs(y = expression(paste("log"["10"],"(Trichome ",Delta,"C"["sto"]," -  Colony ", Delta,"C"["sto"],")")), x = expression(paste("Interaction strength ","(i"["t"],")")))+
  scale_y_continuous(expand = c(0.05, 0.05)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_colour_discrete(type = c("red","orange", "gold", "skyblue"))

