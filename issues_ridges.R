library(ggridges)

compare <- function(df, var, var2) {
  var <- enquo(var)
  var2 <- enquo(var2)
  
  df <- df %>% 
    mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  df %>% 
    rename(ques = newvar) %>% 
    select(ques) %>%
    mutate(issue = !! var2) %>% 
    mutate(group = "Entire Sample")
  
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


graph <- bind_df("ee") %>% as.tibble()



compare <- function(df, var, var2) {
  var <- enquo(var)
  var2 <- enquo(var2)
  
  df <- df %>% 
    mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
    mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
    filter(gay ==1 | trans2 ==1) %>% 
    mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  df %>% 
    rename(ques = newvar) %>% 
    select(ques) %>%
    mutate(issue = !! var2) %>% 
    mutate(group = "LGBT Sample")
  
  
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


graph1 <- bind_df("ee") %>% as.tibble()

full <- bind_rows(graph, graph1)


full %>% 
  filter(ques != 99) %>% 
  ggplot(., aes(x = ques, y = issue, fill = group)) +
  geom_density_ridges2(alpha = .4) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  labs(fill = "", x = "Issue Importance", y = "Issue", title = "Distribution of Issue Importance Responses", caption = "Data: CCES 2016") +
  theme(text=element_text(size=64, family="font")) +
  ggsave("/gay_pol/images/issue_import_ridges.png", width = 12) 
