library(tidyverse)

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



base <- db %>% 
  filter(Type == "MDG", m == 50, n == 500) %>% 
  mutate(nA = is.na(LSEr),
         target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
         draw = (!target)&(abs(LSEr + BRKGAPDM) <= .Machine$double.eps),
         best = (!target)&(LSEr > -BRKGAPDM),
         lost = (!target)&(LSEr < -BRKGAPDM)) %>% 
  gather(Relation, Value, -Order:-GRASPM) %>% 
  filter(Value) %>% 
  mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost"))) %>% 
  mutate(BRKGA = -BRKGA,
         LS = -LS) 


table1 <- base %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(Name) %>% 
  slice_max(LSEr, n = 1, with_ties = FALSE) %>% 
  arrange(File) %>% 
  mutate(ResultLS = if_else(abs(LSEr) <= BRKGAPDM, 'Win', 'Lost'),
         BRKGALS = -LSEr) %>% 
  select(Order, Name, File, BRKGALS, BRKGAM, BRKGAPDM, ResultLS)


base2 <- db %>% 
  filter(Type == "MDG", m == 200, n == 2000) %>% 
  mutate(nA = is.na(LSEr),
         target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
         draw = (!target)&(abs(LSEr + BRKGAPDM) <= .Machine$double.eps),
         best = (!target)&(LSEr > -BRKGAPDM),
         lost = (!target)&(LSEr < -BRKGAPDM)) %>% 
  gather(Relation, Value, -Order:-GRASPM) %>% 
  filter(Value) %>% 
  mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost"))) %>% 
  mutate(BRKGA = -BRKGA,
         LS = -LS) 


#table2 <- 
  base2 %>% filter(Type == "MDG", SubType == "a", n == 2000) %>% 
  group_by(Name) %>% 
  slice_max(LSEr, n = 1, with_ties = FALSE) %>% 
  arrange(File) %>% 
  mutate(ResultLS = if_else(abs(LSEr) <= BRKGAPDM, 'Win', 'Lost'),
         BRKGALS = -LSEr) %>% 
  select(Order, Name, File, BRKGALS, BRKGAM, BRKGAPDM, ResultLS)

base %>% 
  filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(File) %>% 
  mutate(BRKGALS = -LSEr,
         Best  = min(abs(LSEr))) %>% 
  ungroup() %>% 
  mutate(ResultLS = if_else(Best <= BRKGAPDM, 'Win', 'Lost')) %>% 
  select(Name, File, BRKGALS, BRKGAPDM, BRKGAM, ResultLS) %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(Name, -File), 
                   y = BRKGALS, 
                   fill = ResultLS)) +
  geom_point(aes(x = reorder(Name, -File), y = BRKGAM, color = 'BRKGAM')) +
  geom_point(aes(x = reorder(Name, -File), y = BRKGAPDM, color = 'BRKGAPDM')) +
  coord_flip() + theme_light() +
  labs(x = "Deviation (%)",
       y = "Instance MDG-a (n = 500)",
       fill = 'BRKGALS',
       color = 'Methods') +
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom') +
  scale_fill_brewer(palette = "BrBG") +
  scale_color_brewer(palette = "Dark2")



base2 <- db %>% 
  filter(Type == "MDG", m == 200, n == 2000) %>% 
  mutate(nA = is.na(LSEr),
         target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
         draw = (!target)&(abs(LSEr + BRKGAPDM) <= .Machine$double.eps),
         best = (!target)&(LSEr > -BRKGAPDM),
         lost = (!target)&(LSEr < -BRKGAPDM)) %>% 
  gather(Relation, Value, -Order:-GRASPM) %>% 
  filter(Value) %>% 
  mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost"))) %>% 
  mutate(BRKGA = -BRKGA,
         LS = -LS) 


read_rds("data/brkga_results/resultado_type_a_geiza_40_38.rds")





benchmark2 <- read_rds("data/brkga_results/resultado_type_a_geiza_37_38.rds") %>% 
  mutate(
    Type = str_sub(Instancia, 6,8), 
    SubType = str_sub(Instancia, 10,10), 
    File = as.integer(str_remove(str_sub(Instancia, 12,13), "_")), 
    n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
    m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
    Name = paste(SubType, File, sep = "-"),
    Instancia = str_remove(str_remove(Instancia, pattern = "conv_"), pattern = ".txt"))

db2 <- benchmark2  %>% select(-Target) %>% 
  left_join(brito, by = c("Instancia", "Type", "SubType", "File", "Name", "n", "m")) %>% 
  mutate(LSEr = 100*(round(LS,2) - Target)/Target,
         BKEr = 100*(round(BRKGA,2) - Target)/Target) 








benchmark3 <- read_rds("data/brkga_results/resultado_type_a_geiza_40_38.rds") %>% 
  mutate(
    Type = str_sub(Instancia, 6,8), 
    SubType = str_sub(Instancia, 10,10), 
    File = as.integer(str_remove(str_sub(Instancia, 12,13), "_")), 
    n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
    m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
    Name = paste(SubType, File, sep = "-"),
    Instancia = str_remove(str_remove(Instancia, pattern = "conv_"), pattern = ".txt"))

db3 <- benchmark3  %>% 
  select(-Target) %>% 
  left_join(brito, by = c("Instancia", "Type", "SubType", "File", "Name", "n", "m")) %>% 
  mutate(LSEr = 100*(round(LS,2) - Target)/Target,
         BKEr = 100*(round(BRKGA,2) - Target)/Target) 


db %>% count(File) %>% print(n = 50)
db2 %>% count(File) %>% print(n = 50)
db3 %>% count(File) %>% print(n = 50)
db3 %>% filter(File == 38)


db4 <- 
  db %>% 
  bind_rows(db2) %>% 
  bind_rows(db3 %>% filter(File != 38))



base4 <- db4 %>% 
  filter(Type == "MDG", m == 200, n == 2000) %>% 
  mutate(nA = is.na(LSEr),
         target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
         draw = (!target)&(abs(LSEr + BRKGAPDM) <= .Machine$double.eps),
         best = (!target)&(LSEr > -BRKGAPDM),
         lost = (!target)&(LSEr < -BRKGAPDM)) %>% 
  gather(Relation, Value, -Order:-GRASPM) %>% 
  filter(Value) %>% 
  mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost"))) %>% 
  mutate(BRKGA = -BRKGA,
         LS = -LS) 


base4 %>% 
  filter(Type == "MDG", SubType == "a", n == 2000) %>% 
  group_by(File) %>% 
  mutate(BRKGALS = -LSEr,
         Best  = min(abs(LSEr))) %>% 
  ungroup() %>% 
  mutate(ResultLS = if_else(Best <= BRKGAPDM, 'Win', 'Lost')) %>% 
  select(Name, File, BRKGALS, BRKGAPDM, BRKGAM, ResultLS) %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(Name, -File), 
                   y = BRKGALS, 
                   fill = ResultLS)) +
  geom_point(aes(x = reorder(Name, -File), y = BRKGAM, color = 'BRKGAM', shape = 'BRKGAM'), size = 2) +
  geom_point(aes(x = reorder(Name, -File), y = BRKGAPDM, color = 'BRKGAPDM', shape = 'BRKGAM'), size = 2) +
  coord_flip() + theme_light() +
  labs(x = "Deviation (%)",
       y = "Instance MDG-a (n = 2000)",
       fill = 'BRKGALS',
       color = 'Methods',
       shape = 'Methods') +
  guides(shape = 'none') +
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom') +
  scale_fill_brewer(palette = "BrBG") +
  scale_color_brewer(palette = "Dark2")
