cces16 <- cces16 %>%
  mutate(ed2 = recode(educ, "5:6 =1; 1:4 =0")) %>%
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>%
  mutate(trans2 = recode(trans, "1=1; else =0")) %>%
  mutate(lgbt = gay + trans2) %>%
  mutate(lgbt = recode(lgbt, "1= 'LGBT'; else = 'Not LGBT'")) %>%
  mutate(ab1 = recode(CC16_332a, "2=1; 1=0; else = NA")) %>%
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA")) %>%
  mutate(abort = ab1 + ab2 + ab3 + ab4 + ab5) %>%
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
  labs(x ="<-- More Pro Choice: More Pro Life -->", y = "Prediction of Political Partisanship", subtitle = "", title = "Interaction of Education and Abortion Opinion on Political Partisanship", caption = "Data: CCES 2016") +
  theme_minimal() +
  # scale_x_continuous(limits = c(.95,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_y_continuous(limits = c(1,5.75), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.5, 0.2)) +
  theme(text=element_text(size=45, family="font")) +
  theme(plot.title = element_text(size = 36)) +
  scale_fill_d3() +
  scale_color_d3() +
  ggsave("/gay_pol/images/interaction_abortion.png")