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
  filter(Type == "MDG") %>% 
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


base %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(Name) %>% 
  slice_max(LSEr, n = 1, with_ties = FALSE) %>% 
  arrange(File) %>% 
  mutate(ResultLS = if_else(LSEr >= 0, 'Win', 'Lost'),
         BRKGALS = -LSEr) %>% 
  select(Order, Name, File, LSEr, ResultLS)

base %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  #group_by(Name) %>% 
  #slice_max(LSEr, n = 1, with_ties = FALSE) %>% 
  #arrange(File) %>% 
  mutate(ResultLS = if_else(max(LSEr) <= 0, 'Win', 'Lost'),
         BRKGALS = -LSEr) %>% 
  select(Name, File, Target, BRKGALS, BRKGAPDM, BRKGAM, ResultLS) %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(Name, -File), y = BRKGALS, fill = ResultLS)) +
  coord_flip()



