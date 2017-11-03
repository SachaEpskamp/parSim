# Load packages:
library("ggplot2")
library("tidyr")

# Set working directory, in my case:
# setwd("~/GitHub/parSim/examples/LisaExample")

# Read data:
files <- list.files("outputV1/",pattern="Simulation_v1_", full.names = TRUE)
Results <- do.call(rbind, lapply(files, read.table, header = TRUE))

# We want to plot bias and R-squared. Let's first make it long format:
Long <- gather(Results,metric,value,bias:Rsquared)

# Make factors with nice labels:
Long$sigmaFac <- factor(Long$sigma,levels = c(0.25,0.5,1), labels = c("Sigma: 0.025", "Sigma: 0.5", "Sigma: 1"))

# Now let's plot:
g <- ggplot(Long, aes(x = factor(sampleSize), y = value, fill = factor(beta)))  +
  facet_grid(metric ~ sigmaFac, scales = "free") + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("Sample size") + 
  ylab("") + 
  scale_fill_discrete("Beta")
print(g)