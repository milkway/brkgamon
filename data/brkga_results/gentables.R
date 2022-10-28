
library(tidyverse)
library(kableExtra)

results <- read_rds("data/brkga_results/resultado_brkga.rds") %>% mutate(
  Type = str_sub(Instancia, 6,8), 
  SubType = str_sub(Instancia, 10,10), 
  File = as.integer(str_remove(str_sub(Instancia, 12,13), "_")), 
  n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
  m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
  Name = paste(SubType, File, sep = "-"))


#brito <- brito %>% rename(Instancia = X1, Target = X2, BRKGAPDM  = X3, BRKGAM = X4, GRASPM = X5)
#write_rds(brito, "brito.rds")

results %>% filter(n == 2000) %>% ggplot() + 
  geom_boxplot(aes(y = LSEr, x = reorder(Name, -File))) + coord_flip() +
  geom_jitter(aes(y = LSEr, x = reorder(Name, -File)),  color = "darkgreen", alpha = .5) 


read_rds("data/brkga_results/resultado_brkga.rds")
