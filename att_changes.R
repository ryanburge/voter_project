attend <- vp %>%
  filter(pew_bornagain_baseline ==1) %>%
  group_by(presvote16post_2016) %>%
  mutate(attend11 = recode(pew_churatd_baseline, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =99")) %>%
  mutate(attend17 = recode(pew_churatd_2017, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =99"))  %>%
  filter(attend11 != 99) %>%
  filter(attend17 != 99) %>%
  summarise(base = mean(attend11), end = mean(attend17))

attend <- attend %>% 
  mutate(presvote16post_2016 = to_factor(presvote16post_2016)) %>% na.omit() %>%  
  rename(candidate = presvote16post_2016)


attend1 <- vp %>%
  filter(pew_bornagain_baseline ==1) %>%
  mutate(attend11 = recode(pew_churatd_baseline, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =99")) %>%
  mutate(attend17 = recode(pew_churatd_2017, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =99"))  %>%
  filter(attend11 != 99) %>%
  filter(attend17 != 99) %>%
  summarise(base = mean(attend11), end = mean(attend17)) %>% 
  mutate(candidate = c("Entire Sample"))

all_att <- bind_rows(attend, attend1)

all_att$candidate <- factor(all_att$candidate, levels = c("Gary Johnson",  "Hillary Clinton", "Donald Trump", "Entire Sample"))


all_att %>% 
  filter(candidate == "Donald Trump" | candidate == "Hillary Clinton" | candidate == "Entire Sample" | candidate == "Gary Johnson") %>% 
  ggplot(., aes(x=base, xend=end, y=candidate, group=candidate)) + 
  geom_dumbbell(size_x = 3, size_xend = 3, size = 1, color="azure3", 
                colour_x = "forestgreen", colour_xend = "darkorchid",
                dot_guide=FALSE, dot_guide_size=0.05)  + mean_rb() +
  theme(plot.title = element_text(size=34)) +
  scale_x_continuous(limits = c(2.5,5.5), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  geom_text(data=filter(all_att, candidate == "Entire Sample"), aes(x=base, y=candidate, label="2011"), color="black", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  geom_text(data=filter(all_att, candidate == "Entire Sample"), aes(x=end, y=candidate, label="2017"), color="black", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  labs(x = "Average Church Attendance", y = "Candidate Voted For", subtitle = "Among Those Who Said They Were Born-Again in 2011", caption = "Data: Democracy Fund Voter Study Group", title = "Did Politics Drive People Out of Churches?")

ggsave(file="D://voter_project/att_dumbbell.png", type = "cairo-png", width = 12, height = 8)

