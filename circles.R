vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  tabyl(religpew_2017) %>% 
  summarise(sum = sum(n))
  
vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  tabyl(religpew_baseline) %>% 
  summarise(sum = sum(n))


vp <- vp %>% 
  mutate(religbase = to_factor(religpew_baseline, relig17 = to_factor(religpew_2017)))


vp <- vp %>% 
  mutate(religbase = as.numeric(religpew_baseline)) %>% 
  mutate(religbase = recode(religbase, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


vp <- vp %>% 
  mutate(relig17 = as.numeric(religpew_2017)) %>% 
  mutate(relig17 = recode(relig17, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


circle <- vp %>% 
  select(religbase, relig17) %>% 
  filter(religbase !=12) %>% 
  filter(relig17 !=12) %>% 
  na.omit() %>% 
  graph_from_data_frame(directed = TRUE)

V(circle)$node_label <- unname(ifelse(degree(circle)[V(circle)] > 1, names(V(circle)), "")) 
V(circle)$node_size <- unname(ifelse(degree(circle)[V(circle)] > 1, degree(circle), 0)) 

ggraph(circle, layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(alpha =..index..)) +
  geom_node_label(aes(label=node_label),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, family="Product Sans", fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted screen names labeled. Darkers edges == more retweets. Node size == larger degree") +
  theme_graph(base_family="Product Sans") +
  theme(legend.position="none")


vp %>% group_by(religbase, relig17) %>% count()

  