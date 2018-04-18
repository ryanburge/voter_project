all <- vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  tabyl(pew_bornagain_2017) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  mutate(c= ("Entire Sample"))

trump <- vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(presvote16post_2016 ==2) %>% 
  tabyl(pew_bornagain_2017) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  mutate(c= ("Trump Voters"))

clinton <- vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(presvote16post_2016 ==1) %>% 
  tabyl(pew_bornagain_2017) %>% 
  filter(pew_bornagain_2017 ==2) %>% 
  mutate(c= ("Clinton Voters"))


com <- bind_rows(all, trump, clinton) %>% select(c, valid_percent) %>% 
  rename(pct = valid_percent)

com <- com %>% mutate(pct = round(pct,3))


com %>% 
  ggplot(., aes(x = c, y = pct, fill = c)) + geom_col(color = "black") +
  bar_rb() +
  theme(axis.title.y = element_text(size =18)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Were Not Born-Again in 2017", title = "Did Politics Lead Some Evangelicals to Defect?", subtitle = "Among Those Who Said They Were Born-Again in 2011", caption = "Data: Democracy Fund Voter Study Group") +
  theme(plot.title = element_text(size=36)) +
  theme(legend.position="none")  +
  scale_fill_manual(values = c("Trump Voters" = "firebrick3", "Clinton Voters" = "dodgerblue3", "Entire Sample" = "azure3")) 

ggsave(file="D://voter_project/defect_by_vote.png", type = "cairo-png", width = 12, height = 8)

