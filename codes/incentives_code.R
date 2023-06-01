# Incentive Analysis

# List of dataframes for incentives
dataframes <- list(BrokenBow = BrokenBow[c(141:183), c(3:5)],
                   Hickman = Hickman[c(151:212),c(3:5)],
                   Lincoln = Lincoln[c(69:128), c(3:5)],
                   Mead = Mead[c(128:189),c(3:5)],
                   Norflok = Norflok[c(147:209),c(3:5)],
                   York = York[c(107:173),c(3:5)])

# Removing unnecessary columns
BR2 <- Bridgeport_2020_Incentive_Ratings[,-c(4:6)]
BR2$Poll.Option <- as.factor(BR2$Poll.Option)

# Joining the full data_frame
CD3 <- Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = c("Poll.Question", "Poll.Option")), dataframes)
CD3 <- full_join(CD3, BR2, by = c("Poll.Question", "Poll.Option"))

# changing the column names
names(CD3) <- c("Poll.Question", "Poll.Option", "Broken Bow", "Hickman", "Lincoln", "Mead", "Norfolk", "York", "Bridgeport")

# Reshaping data from wide to long
CD4 <- gather(CD3, "Broken Bow", "Hickman", "Lincoln", "Mead", "Norfolk", "York", "Bridgeport",key = "Location", value = "Count")

# Reordering factor levels
question_levels <- c("1.  If the change makes my operation more dollars", "2.  I want to leave my land better for the next generation","3.  Improve my soil organic matter and soil carbon","4.  Because my landlord wants me to","5.  Discounted crop insurance rates","6.  Improve my crop yields","7.  Increase the value of my land for a future sale","8.  Increase my soil water infiltration","9.  Decrease nitrogen leaching into groundwater", "10. Decrease topsoil erosion ", "11. Decrease pesticide and nutrient loss in water runoff from my fields", "12. Decrease weather risk and increase weather resiliency of my crops", "13. Increase weed control", "14. Reduce herbicide use")
CD4$Poll.Question <- factor(CD4$Poll.Question, levels = question_levels)

# Plotting
# Plotting
CD4 %>% 
  ggplot(aes(Poll.Option, Count)) + 
  geom_bar(aes(fill = Location), stat = "identity") + 
  facet_wrap(~Poll.Question, ncol = 7, labeller = label_wrap_gen()) + 
  scale_fill_brewer(palette = "Set3") + 
  labs(x = "Ratings", y = "Number of Response") + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13)) + 
  theme(legend.position = c(0.8,0.95), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.direction = "horizontal") + 
  theme(axis.text.x = element_text(size = 15, face = "bold"), 
        axis.text.y = element_text(size = 15, face = "bold"), 
        axis.text = element_text(size = 17))

                                                                                         