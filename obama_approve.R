ba <- vp %>% 
  filter(pew_bornagain_2017 ==1) %>% 
  mutate(trump = as.numeric(obamaapp_2016)) %>% 
  mutate(trump = recode(trump, "1:2 = 'Approve'; 3:4= 'Disapprove'; 5 = 'Do Not Know'; else =99")) %>% 
  count(trump) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(trump != 99) %>% 
  mutate(group = c("Always Born Again"))


switch <- vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  mutate(trump = as.numeric(obamaapp_2016)) %>% 
  mutate(trump = recode(trump, "1:2 = 'Approve'; 3:4= 'Disapprove'; 5 = 'Do Not Know'; else =99")) %>% 
  count(trump) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(trump != 99) %>% 
  mutate(group = c("BA to Not BA"))

never <- vp %>% 
  filter(pew_bornagain_baseline ==2) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  mutate(trump = as.numeric(obamaapp_2016)) %>% 
  mutate(trump = recode(trump, "1:2 = 'Approve'; 3:4= 'Disapprove'; 5 = 'Do Not Know'; else =99")) %>% 
  count(trump) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(trump != 99) %>% 
  mutate(group = c("Never Born Again"))


gr <- bind_rows(ba, switch, never)

gr$trump <- factor(gr$trump, levels = c("Approve", "Do Not Know", "Disapprove"))

gr %>% rename(obama = trump)

gr %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= trump), colour = "black") + 
  facet_grid(group ~ .) + 
  coord_flip() +
  flip_bar_rb() +
  scale_fill_manual(values=c("firebrick1", "azure3", "dodgerblue3" )) +
  theme(axis.title.y = element_blank()) + 
  ylab("") + xlab("") +
  labs(title = "Approval of Pres. Obama in December 2016", caption = "Data: Democracy Fund Voter Study Group", subtitle = "Born Again Status Was Asked in 2011 and 2017") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(face="bold", size = 44)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggsave(file="D://voter_project/obama_approve.png", type = "cairo-png", width = 15, height = 12)







