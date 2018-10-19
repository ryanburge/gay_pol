library(prediction)
library(broom)


cces16 <- cces16 %>% 
  mutate(ed2 = recode(educ, "5:6 =1; 1:4 =0")) %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1= 'LGBT'; else = 'Not LGBT'")) %>% 
  mutate(gaym = recode(CC16_301n, "1=5; 2=4; 3=3; 4=2; 5=1; else =99")) %>% 
  mutate(pid = recode(pid7, "8:99 = 99"))



lgbt <- cces16 %>% filter(lgbt == "LGBT") %>% filter(pid != 99) %>% filter(gaym != 99)
not_lgbt <- cces16 %>% filter(lgbt == "Not LGBT")%>% filter(pid != 99) %>% filter(gaym != 99)
 
reg1 <- lm(pid ~ gaym*ed2, data = lgbt)
reg2 <- lm(pid ~ gaym*ed2, data = not_lgbt)

b <- predict(reg1, newdata = lgbt,  interval =  "confidence")
l1 <- cbind(lgbt, b) %>% mutate(group = c("LGBT"))


b <- predict(reg2, newdata = not_lgbt,  interval =  "confidence")
l2 <- cbind(not_lgbt, b) %>% mutate(group = c("Not LGBT"))


margin <- bind_rows(l1, l2)

margin %>% 
  mutate(ed2 = recode(ed2, "1 = 'College Degree'; 0 = 'Less than College'")) %>% 
  mutate(new = paste(ed2, group, sep = " - ")) %>% 
  ggplot(., aes(gaym, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = as.factor(new), fill = as.factor(new)), alpha = .2, show.legend = FALSE) + 
  labs(x ="Importance of Gay Marriage", y = "Prediction of Political Partisanship", subtitle = "", title = "Interaction of Education and Sexuality on Political Partisanship", fill = "") + 
  theme_minimal() +
  scale_x_continuous(limits = c(.95,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_y_continuous(limits = c(1,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(legend.title=element_blank())


## Gay Marriage ####

cces16 <- cces16 %>% 
  mutate(ed2 = recode(educ, "5:6 =1; 1:4 =0")) %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1= 'LGBT'; else = 'Not LGBT'")) %>% 
  mutate(abort = recode(CC16_301b, "1=5; 2=4; 3=3; 4=2; 5=1; else =99")) %>% 
  mutate(pid = recode(pid7, "8:99 = 99"))



lgbt <- cces16 %>% filter(lgbt == "LGBT") %>% filter(pid != 99) %>% filter(abort != 99)
not_lgbt <- cces16 %>% filter(lgbt == "Not LGBT")%>% filter(pid != 99) %>% filter(abort != 99)

reg1 <- lm(pid ~ abort*ed2, data = lgbt)
reg2 <- lm(pid ~ abort*ed2, data = not_lgbt)


b <- predict(reg1, newdata = lgbt,  interval =  "confidence")
l1 <- cbind(lgbt, b) %>% mutate(group = c("LGBT"))


b <- predict(reg2, newdata = not_lgbt,  interval =  "confidence")
l2 <- cbind(not_lgbt, b) %>% mutate(group = c("Not LGBT"))


margin <- bind_rows(l1, l2)

margin %>% 
  mutate(ed2 = recode(ed2, "1 = 'College Degree'; 0 = 'Less than College'")) %>% 
  mutate(new = paste(ed2, group, sep = " - ")) %>% 
  ggplot(., aes(abort, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = as.factor(new), fill = as.factor(new)), alpha = .2, show.legend = FALSE) + 
  labs(x ="Importance of Abortion", y = "Prediction of Political Partisanship", subtitle = "", title = "Interaction of Education and Sexuality on Political Partisanship", fill = "") + 
  theme_minimal() +
  scale_x_continuous(limits = c(.95,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_y_continuous(limits = c(1,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(legend.title=element_blank())
