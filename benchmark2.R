library(tidyverse)

read_rds("../brkga/inst/extdata/MDG.10.a.n500m50.rds")

benchmark <- read_rds("data/brkga_results/resultado_brkga.rds") %>% mutate(
  Type = str_sub(Instancia, 6,8), 
  SubType = str_sub(Instancia, 10,10), 
  File = as.integer(str_remove(str_sub(Instancia, 12,13), "_")), 
  n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
  m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
  Name = paste(SubType, File, sep = "-"),
  Instancia = str_remove(str_remove(Instancia, pattern = "conv_"), pattern = ".txt"))


brito <- read_rds("data/brito.rds")  %>% mutate(
  Type = str_sub(Instancia, 1,3), 
  SubType = str_sub(Instancia, 5,5), 
  File = as.integer(str_remove(str_sub(Instancia, 7,8), "_")), 
  n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
  m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
  Name = paste(SubType, File, sep = "-"))

db <- benchmark  %>% select(-Target) %>% 
  left_join(brito, by = c("Instancia", "Type", "SubType", "File", "Name", "n", "m")) %>% 
  mutate(LSEr = 100*(round(-LS,2) - Target)/Target,
         BKEr = 100*(round(-BRKGA,2) - Target)/Target)



db %>% 
  filter(n == 500, m == 50) %>% 
  mutate(Method = paste(SubType, File, '-')) %>% 
  select(Method, LSEr) %>% 
  ggplot() + 
  geom_boxplot(aes(x = reorder(Method, LSEr, median), y = LSEr, fill = Method)) + 
  coord_flip() + labs(x = "Method", y = "Error in %")


db %>% ggplot() + 
  geom_boxplot(aes(x = reorder(Method, N_gen, median), y = N_gen, fill = Method)) + 
  coord_flip() + labs(x = "Method", y = "Number of Generations")


db %>% ggplot() + 
  geom_boxplot(aes(x = reorder(Method, N_bst, median), y = N_bst, fill = Method)) + 
  coord_flip() + labs(x = "Method", y = "Best Generation")


db %>% ggplot() + 
  geom_boxplot(aes(x = reorder(Method, LSEr, median), y = LSEr, fill = Method)) + 
  coord_flip() + labs(x = "Method", y = "Error in %") + facet_wrap(. ~ Name)
