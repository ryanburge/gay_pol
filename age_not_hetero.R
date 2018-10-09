by_age <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = NA")) %>% 
  mutate(age = 2016 - birthyr) %>% 
  group_by(age) %>% 
  mean_ci(gay, wt = commonweight_vv_lgbt) 

pd <- position_dodge(0.2)

by_age %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(x= age, y = mean)) +
  geom_point() + 
  geom_line(size = 1) +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  geom_errorbar(aes(ymin=lower, ymax = upper), width = .1, position = pd, size = .75) +
  scale_y_continuous(limits = c(0, .355), labels = percent) +
  labs(x = "Respondent's Age", y = "Percent Not Heterosexual", title = "Sexuality at Different Ages", caption = "Data: CCES 2016") +
  annotate("text", x=36, y = .355, label = "Percent Indicating They Are: Lesbian, Gay, Bisexual, or Other", size = 12, family = "font") +
  ggsave("/gay_pol/images/age_percent.png") 
 