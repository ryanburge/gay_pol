
cces16 <- cces16 %>% 
  mutate(white = recode(race, "1=1; else =0")) %>% 
  mutate(abort = recode(CC16_301b, "1=5; 2=4; 3=3; 4=2; 5=1; else =99")) %>% 
  mutate(pid = recode(pid7, "8:99 = 99"))



evan <- cces16 %>% filter(evangelical == 1) %>% filter(pid != 99) %>% filter(abort != 99)
not_evan <- cces16 %>% filter(evangelical == 0)%>% filter(pid != 99) %>% filter(abort != 99)

reg1 <- lm(pid ~ abort*white, data = evan)

reg2 <- lm(pid ~ abort*white, data = not_evan)


b <- predict(reg1, newdata = evan,  interval =  "confidence")
l1 <- cbind(evan, b) %>% mutate(group = c("Evangelical"))


b <- predict(reg2, newdata = not_evan,  interval =  "confidence")
l2 <- cbind(not_evan, b) %>% mutate(group = c("Not Evangelical"))


margin <- bind_rows(l1, l2)

margin %>% 
  mutate(white = recode(white, "1 = 'White'; 0 = 'Not White'")) %>% 
  mutate(new = paste(white, group, sep = " - ")) %>% 
  ggplot(., aes(abort, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = as.factor(new), fill = as.factor(new)), alpha = .2, show.legend = FALSE) + 
  labs(x ="Importance of Abortion", y = "Prediction of Political Partisanship", subtitle = "", title = "Interaction of Race and Abortion Importance on Political Partisanship", caption = "Data: CCES 2016") + 
  theme_minimal() +
  scale_x_continuous(limits = c(.95,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_y_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.2)) + 
  theme(text=element_text(size=45, family="font")) +
  theme(plot.title = element_text(size = 36)) +
  scale_fill_d3() +
  scale_color_d3() +
  ggsave("evan_abortion_interact2.png")