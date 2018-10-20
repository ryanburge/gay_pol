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
  mutate(pid = recode(pid7, "8:99 = 99")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  mutate(trump = recode(CC16_410a, "1=1; 2:4=0; else=NA"))

ggg1 <- cces16 %>% 
  filter(abort != "NA") %>% 
  group_by(pid3, abort) %>% 
  mean_ci(trump)

ggg2 <- cces16 %>% 
  filter(abort != "NA") %>% 
  filter(lgbt == "LGBT") %>% 
  group_by(abort) %>% 
  mean_ci(trump) %>% 
  mutate(pid3 = "LGBT")

graph <- bind_df("ggg")

pd <- position_dodge(0.2)


graph %>% 
  filter(pid3 != "NA") %>% 
  ggplot(., aes(x= abort, y = mean, group = pid3, color = pid3)) +
  geom_point() + 
  geom_line(size = 1) +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = pid3, fill = pid3), alpha = .4, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, .955), labels = percent) + 
  scale_color_manual(values=c("dodgerblue3", "azure4", "forestgreen", "firebrick1")) +
  scale_fill_manual(values=c("dodgerblue3", "azure4", "forestgreen", "firebrick1")) +
  labs(x ="<-- More Pro Choice: More Pro Life -->", y = "Vote Share for Donald Trump", subtitle = "", title = "Impact of Abortion on Vote Choice in 2016", caption = "Data: CCES 2016") +
  theme(legend.position = c(0.8, 0.1)) +
  theme(legend.title=element_blank()) +
  ggsave("/gay_pol/images/abort_trump_ribbons.png") 



