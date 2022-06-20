watchedVideos %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

watchedVideos %>%
  ggplot(aes(x = time, fill = category)) + 
  labs(x = "Year", y = "Count") +
  ggtitle("How much have your genre tastes changed over time?", "Most played categories")+
  geom_area(stat = "bin") +
  theme_economist_white()