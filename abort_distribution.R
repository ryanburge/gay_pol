abb <- cces16 %>% 
  filter(lgbt == "LGBT") %>% 
  ct(abort, wt = commonweight_vv_lgbt) %>% 
  mutate(group = "LGBT Voters")

abb2 <- cces16 %>% 
  filter(pid3 == "Democrats")  %>% 
  ct(abort, wt = commonweight_vv) %>% 
  mutate(group = "Democrats")

graph <- bind_df("abb")

graph %>% 
  filter(abort != "NA") %>% 
  ggplot(., aes(x= abort, y = pct, group = group, fill = group)) +
  geom_col(position = "dodge", color = "black") +
  theme_minimal() +
  scale_fill_manual(values=c("dodgerblue3", "forestgreen")) +
  labs(x ="<-- More Pro Choice: More Pro Life -->", y = "Percent", subtitle = "", title = "Distribution of Abortion Opinion in 2016", caption = "Data: CCES 2016") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(0.8, .8)) +
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=45, family="font")) + 
  ggsave("/gay_pol/images/abort_distribute.png") 


abb3 <- cces16 %>% 
  mutate(gender = recode(gender, "1 =  'Male'; 2 = 'Female'")) %>% 
  filter(lgbt == "LGBT") %>% 
  group_by(gender) %>% 
  ct(abort, wt = commonweight_vv_lgbt) %>% 
  mutate(group = "LGBT Voters")



abb3 %>% 
  filter(abort != "NA") %>% 
  ggplot(., aes(x= abort, y = pct, group = gender, fill = gender)) +
  geom_col(position = "dodge", color = "black") +
  theme_minimal() +
  scale_fill_manual(values=c("darkorchid", "dodgerblue3")) +
  labs(x ="<-- More Pro Choice: More Pro Life -->", y = "Percent", subtitle = "Among LGBT Respondents", title = "Distribution of Abortion Opinion in 2016", caption = "Data: CCES 2016") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(0.8, .8)) +
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=45, family="font")) + 
  ggsave("/gay_pol/images/abort_distribute_gender.png") 

