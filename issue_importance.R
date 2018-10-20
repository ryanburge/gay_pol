
compare <- function(df, var, ques){
  
  var <- enquo(var)
  ques <- enquo(ques)
  
  df <- df %>% mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  dd1 <- df %>% 
    mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
    mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
    filter(gay ==1 | trans2 ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "LGBT Individuals")
  
  dd2 <- df %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "Entire Sample")
  
  
  all <- bind_rows(dd1, dd2)
  all
  
  
}

ee1 <- cces16 %>% compare(CC16_301a, "Gun Control")
ee2 <- cces16 %>% compare(CC16_301b, "Abortion")
ee3 <- cces16 %>% compare(CC16_301c, "Taxes")
ee4 <- cces16 %>% compare(CC16_301d, "Immigration")
ee5 <- cces16 %>% compare(CC16_301e, "Budget Deficit")
ee6 <- cces16 %>% compare(CC16_301f, "Defense Spending")
ee7 <- cces16 %>% compare(CC16_301g, "Social Security")
ee8 <- cces16 %>% compare(CC16_301h, "Environment")
ee9 <- cces16 %>% compare(CC16_301i, "Jobs")
ee10 <- cces16 %>% compare(CC16_301j, "Crime")
ee11 <- cces16 %>% compare(CC16_301k, "National Security")
ee12 <- cces16 %>% compare(CC16_301l, "Race Relations")
ee13 <- cces16 %>% compare(CC16_301m, "Health Care")
ee14 <- cces16 %>% compare(CC16_301n, "Gay Marriage")
ee15 <- cces16 %>% compare(CC16_301o, "Govt. Corruption")

graph <- bind_df("ee")


font_add_google("Playfair Display", "font")
showtext_auto()


graph %>% 
  ggplot(., aes(y=mean, x= fct_reorder(topic, mean), color = group)) +
  geom_point(position=position_dodge(width=0.5), size =3) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Issue Importance in 2016", x = "Issue Area", y = "Level of Importance", caption = "Data: CCES 2016") +
  scale_y_continuous(limits = c(2.55,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_color_npg() + 
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=64, family="font")) +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  geom_vline(xintercept = 2.5, linetype = "dotdash") +
  geom_vline(xintercept = 3.5, linetype = "dotdash") +
  geom_vline(xintercept = 4.5, linetype = "dotdash") +
  geom_vline(xintercept = 5.5, linetype = "dotdash") +
  geom_vline(xintercept = 6.5, linetype = "dotdash") +
  geom_vline(xintercept = 7.5, linetype = "dotdash") +
  geom_vline(xintercept = 8.5, linetype = "dotdash") +
  geom_vline(xintercept = 9.5, linetype = "dotdash") +
  geom_vline(xintercept = 10.5, linetype = "dotdash") +
  geom_vline(xintercept = 11.5, linetype = "dotdash") +
  geom_vline(xintercept = 12.5, linetype = "dotdash") +
  geom_vline(xintercept = 13.5, linetype = "dotdash") +
  geom_vline(xintercept = 14.5, linetype = "dotdash") +
  ggsave("/gay_pol/images/issue_importance.png", width =12) 

## Broken down by PID

compare <- function(df, var, ques){
  
  var <- enquo(var)
  ques <- enquo(ques)
  
  df <- df %>% mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  dd1 <- df %>% 
    mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
    mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
    filter(gay ==1 | trans2 ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "LGBT Individuals")
  
  dd2 <- df %>% 
    filter(newvar != 99) %>% 
    filter(pid7 <=3) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "Democrats")
  
  dd3 <- df %>% 
    filter(newvar != 99) %>% 
    filter(pid7 >= 5) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "Republicans")
  
  dd4 <- df %>% 
    filter(newvar != 99) %>% 
    filter(pid7 == 4) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "Independents")
  
  
  all <- bind_rows(dd1, dd2, dd3, dd4)
  all
  
  
}

ee1 <- cces16 %>% compare(CC16_301a, "Gun Control")
ee2 <- cces16 %>% compare(CC16_301b, "Abortion")
ee3 <- cces16 %>% compare(CC16_301c, "Taxes")
ee4 <- cces16 %>% compare(CC16_301d, "Immigration")
ee5 <- cces16 %>% compare(CC16_301e, "Budget Deficit")
ee6 <- cces16 %>% compare(CC16_301f, "Defense Spending")
ee7 <- cces16 %>% compare(CC16_301g, "Social Security")
ee8 <- cces16 %>% compare(CC16_301h, "Environment")
ee9 <- cces16 %>% compare(CC16_301i, "Jobs")
ee10 <- cces16 %>% compare(CC16_301j, "Crime")
ee11 <- cces16 %>% compare(CC16_301k, "National Security")
ee12 <- cces16 %>% compare(CC16_301l, "Race Relations")
ee13 <- cces16 %>% compare(CC16_301m, "Health Care")
ee14 <- cces16 %>% compare(CC16_301n, "Gay Marriage")
ee15 <- cces16 %>% compare(CC16_301o, "Govt. Corruption")

graph <- bind_df("ee")


font_add_google("Playfair Display", "font")
showtext_auto()


my.cols <- c("dodgerblue3", "azure4", "forestgreen", "firebrick3")


graph %>% 
  ggplot(., aes(y=mean, x= fct_reorder(topic, mean), color = group)) +
  geom_point(position=position_dodge(width=0.5), size =3) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Issue Importance in 2016", x = "Issue Area", y = "Level of Importance", caption = "Data: CCES 2016") +
  scale_y_continuous(limits = c(2,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_color_manual(values =my.cols) +
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=64, family="font")) +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  geom_vline(xintercept = 2.5, linetype = "dotdash") +
  geom_vline(xintercept = 3.5, linetype = "dotdash") +
  geom_vline(xintercept = 4.5, linetype = "dotdash") +
  geom_vline(xintercept = 5.5, linetype = "dotdash") +
  geom_vline(xintercept = 6.5, linetype = "dotdash") +
  geom_vline(xintercept = 7.5, linetype = "dotdash") +
  geom_vline(xintercept = 8.5, linetype = "dotdash") +
  geom_vline(xintercept = 9.5, linetype = "dotdash") +
  geom_vline(xintercept = 10.5, linetype = "dotdash") +
  geom_vline(xintercept = 11.5, linetype = "dotdash") +
  geom_vline(xintercept = 12.5, linetype = "dotdash") +
  geom_vline(xintercept = 13.5, linetype = "dotdash") +
  geom_vline(xintercept = 14.5, linetype = "dotdash") +
  ggsave("/gay_pol/images/issue_importance_pid.png", width =12) 








