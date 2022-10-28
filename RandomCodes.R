# base <- db %>% 
#   filter(Type == "MDG") %>% 
#   mutate(Relation = factor(if_else(is.na(LSEr), NA_character_,
#                                    if_else(isTRUE(all.equal(round(LSEr, 2), 0.0)), "target",
#                                            if_else(isTRUE(all.equal(abs(LSEr), abs(BRKGAPDM))),
#                                                    if_else(LSEr > -BRKGAPDM, "best", "lost")))), 
#                            levels = c("target", "best", "lost")))


base <- db %>% 
  filter(Type == "MDG") %>% 
  mutate(nA = is.na(LSEr),
         target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
         draw = (!target)&(abs(LSEr + BRKGAPDM) <= .Machine$double.eps),
         best = (!target)&(LSEr > -BRKGAPDM),
         lost = (!target)&(LSEr < -BRKGAPDM)) %>% 
  gather(Relation, Value, -Order:-GRASPM) %>% 
  filter(Value) %>% 
  mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost")))
base %>% 
  ggplot() + 
  geom_bar(aes(x = as.factor(File), fill = Relation), position = "stack", na.rm = TRUE) + 
  facet_wrap(. ~ SubType, nrow = 3, strip.position = "right") + #xlim(1, 40) +
  scale_y_continuous(name ="Replications", breaks = seq(0,10,by = 2)) +
  scale_x_discrete(name ="Instance", breaks = paste0(seq(1,max(db$File),by = 2))) +
  scale_fill_manual(breaks = c("target", "best",   "draw", "lost"), 
                    values = rev(RColorBrewer::brewer.pal(4, "RdYlGn")), drop = FALSE) +
  theme(legend.position="top") + labs(fill = " ")



base %>% 
  filter(n == 500, m == 50) %>% 
  select(BRKGA, LS, BRKGAPDM, BRKGAM, Target, Name, LSEr, BKEr, GRASPM, Relation) %>% 
  
  count(Relation)



library(tidyverse)
library(networkD3)
library(scales)


simulation <- read_rds("data/brkga.rds")

read_rds("data/brkga_results/resultado_brkga.rds")

# control colours with a JS ordinal scale
# edited 20 May 2017 with updated code from Renal Chesak's answer:
pal <- RColorBrewer::brewer.pal(3, "Set1")
ColourScale <- sprintf('d3.scaleOrdinal()
                       .domain(["node", "tour"])
                       .range(["%s", "%s"]);', pal[1], pal[3])

Tour <- simulation$Tour[1] %>% unlist(use.names = FALSE)

dist_matrix <- read_rds("../brkga/inst/extdata/MDG.1.a.n500m50.rds") 

n = ncol(dist_matrix)
m <- length(Tour)
Nds <- rep("node", n)
Nds[Tour] <- "tour"

Links <- data.frame((dist_matrix)*upper.tri(dist_matrix),   stringasfactors = FALSE) %>%  
  mutate(source = 0:(n-1)) %>% 
  gather(target, value, -source) %>% 
  filter(value != 0) %>% 
  mutate(
    color = grey(value/max(value),.5),
    target = -1 + as.integer(str_remove(target, "V")),
    color = if_else(
      (source %in% (Tour-1)) & (target %in% (Tour-1)),
      pal[2], 
      color)
  )


Nodes <- data.frame(
  name = paste0("V", 1:n), 
  group = Nds,
  size = .5*(dist_matrix %*% matrix(Nds == "tour", ncol = 1))) %>% 
  mutate_at(.vars = "size", function(x){rescale(x, to = c(1, 100))})


forceNetwork(Links = Links, Nodes = Nodes, Nodesize = "size",
             NodeID = "name", Group = "group", opacity = 1, 
             Source = "source", Target = "target", Value = "value", 
             zoom = TRUE,
             linkColour = Links$color,
             charge = -1000,
             colourScale = networkD3::JS(ColourScale), 
             linkWidth = 2, opacityNoHover = 1, 
             linkDistance = networkD3::JS("function(d) { return 10*d.value; }")
             )


Nodes2 <- 
  Nodes %>% 
  mutate(Order = row_number(),
         Seq = if_else(group == "tour", 0.0, 1.0)) %>% 
  arrange(Seq, -size) %>% 
  slice(1:(2*m))

Links2 <- Links %>% 
  filter((source %in% Nodes2$Order) & (target %in% Nodes2$Order)) %>% 
  mutate(source = source  - min(source), 
         target = target  - min(target))
  



forceNetwork(Links = Links2, Nodes = Nodes2, Nodesize = "size",
             NodeID = "name", Group = "group", opacity = 1,
             Source = "source", Target = "target", Value = "value", 
             bounded = FALSE, zoom = TRUE,
             linkColour = Links2$color,
             charge = -1000,
             colourScale = networkD3::JS(ColourScale), 
             linkWidth = 2,
             linkDistance = networkD3::JS("function(d) { return 10*d.value; }")
)

simpleNetwork(Links2)
forceNetwork(Links = Links2, Nodes = Nodes2, Nodesize = "size",
             NodeID = "name", Group = "group", opacity = 1,
             Source = "source", Target = "target",
)
# Plot tour only ///////////////////////




library(tidyverse)
library(networkD3)
library(scales)


simulation <- read_rds("~/data/brkga.rds")

# control colours with a JS ordinal scale
# edited 20 May 2017 with updated code from Renal Chesak's answer:
pal <- RColorBrewer::brewer.pal(3, "Set1")
ColourScale <- sprintf('d3.scaleOrdinal()
                       .domain(["node", "tour"])
                       .range(["%s", "%s"]);', pal[1], pal[3])

Tour <- simulation$Tour[1] %>% unlist(use.names = FALSE) + 1
m <- length(Tour)

dist_matrix <- read_rds("../brkga/inst/extdata/MDG.1.a.n500m50.rds") 

Alpha <- matrix(rep(0,  ncol(dist_matrix), ncol = 1))
Alpha[Tour] <- 1
Alpha = (dist_matrix %*% Alpha)/2

Nodes <- 
  as.data.frame(sort(Alpha, decreasing = TRUE, index.return = TRUE)) %>%
  rename(size = x, index = ix) %>% 
  top_n(3*m, size) %>% 
  mutate(name = paste0("V", index),
         order = 0:(3*m - 1),
         group = if_else(index %in% Tour, "node", "tour"))

graph_matrix <- dist_matrix[Nodes$index, Nodes$index]
n <- ncol(graph_matrix)
colnames(graph_matrix) <- paste0("V", 1:n)

Links <- data.frame((graph_matrix)*upper.tri(graph_matrix)) %>%  
  mutate(source = 0:(n-1)) %>% 
  gather(target, value, -source) %>% 
  filter(value != 0) %>% 
  mutate(
    color = grey(value/max(value),.5),
    target = -1 + as.integer(str_remove(target, "V"))
  )


forceNetwork(Links = Links, Nodes = Nodes, Nodesize = "size",
             NodeID = "name", Group = "group", opacity = 1, bounded = FALSE,
             Source = "source", Target = "target", Value = "value", 
             zoom = TRUE,
             linkColour = Links$color,
             charge = -1000,
             colourScale = networkD3::JS(ColourScale), 
             linkWidth = 1, opacityNoHover = 1, 
             linkDistance = networkD3::JS("function(d) { return 10*d.value; }")
)

#/////////////////////////

library(tidyverse)

simulation %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(File, LSEr) %>% count() %>% arrange(File, LSEr)


simulation %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(File, LSEr) %>% count() %>% 
  arrange(File, -LSEr) %>% ungroup %>% group_by(File) %>% slice(1)


base %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(File, LSEr) %>% count() %>% 
  arrange(File, -LSEr) %>% ungroup %>% group_by(File) %>% slice(1)


base %>% filter(Type == "MDG", SubType == "a", n == 500) %>% 
  group_by(Name) %>% 
  slice_min(LSEr) %>% 
  arrange(File)
