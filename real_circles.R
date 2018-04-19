vp <- vp %>% 
  mutate(religbase = as.numeric(religpew_baseline)) %>% 
  mutate(religbase = recode(religbase, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


vp <- vp %>% 
  mutate(relig17 = as.numeric(religpew_2017)) %>% 
  mutate(relig17 = recode(relig17, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


circle<- vp %>% 
  group_by(religbase, relig17) %>% 
  count() %>% 
  filter(religbase !=12) %>% 
  filter(relig17 !=12) %>% 
  rename(from = religbase, to = relig17, value =n)


png("circle_final.png", width = 7, height = 7, units = 'in', res = 300)
grid.col = c(Protestant = "#F97F51", Catholic = "#1B9CFC", Orthodox = "black",
             Mormon = "#58B19F", Jewish = "#2C3A47", Buddhist = "#6D214F", Nothing = "#182C61", Muslim = "#FC427B", Agnostic = "#BDC581", Hindu = "gray", Atheist = "#82589F")
par(family = 'Product Sans')
chordDiagram(circle, order = c("Protestant", "Orthodox", "Catholic",  "Mormon", "Jewish", "Buddhist", "Nothing", "Muslim","Agnostic", "Hindu", "Atheist"), grid.col = grid.col)
title(main ="The Shifting Religious Landscape from 2011 to 2017")
text(.7,-1.05, "Data: Democracy Fund Voter Study Group", cex = .65)
# legend("bottomright", legend = "This is the legend")


dev.off()
circos.clear()




circos.clear()