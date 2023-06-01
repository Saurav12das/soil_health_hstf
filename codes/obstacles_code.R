library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(forcats)


library(dplyr)
library(ggplot2)

# Data frames for obstacle ratings
BB <- BrokenBow[92:134, 3:5]
HK <- Hickman[80:129, 3:5]
LK <- Lincoln[28:64, 3:5]
MD <- Mead[67:115, 3:5]
NF <- Norflok[84:133, 3:5]
YK <- York[47:96, 3:5]
BR <- Bridgeport_2020_Obstacle.Ratings[-c(51, 52), -c(4:6)]

# Challenges and Obstacles - Full join of data frames
CD1 <- full_join(BB, HK, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(., LK, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(., MD, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(., NF, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(., YK, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(., BR, by = c("Poll.Question", "Poll.Option"))

# Change column names
CD1 <- CD1 %>%
  setnames(old = c("Count.x", "Count.y", "Count.x.x", "Count.y.y", "Count.x.x.x", "Count.y.y.y", "Count"),
           new = c("Broken Bow", "Hickman", "Lincoln", "Mead", "Norfolk", "York", "Bridgeport")) %>%
  rename_all(~gsub("\\.", "_", .))

# Gather the data with gather function from dplyr
CD2 <- CD1 %>%
  gather(key = "Location", value = "Count", -Poll_Question, -Poll_Option)

# Stacked Bar plot
CD2$Poll_Question <- factor(CD2$Poll_Question, levels = c(
  "1. Input costs including seed cost",
  "2. Weather issues",
  "3. Farm machinery and equipment",
  "4. Farm labor",
  "5. Potential of a crop yield lag",
  "6. The window and later crop harvest",
  "7. Allowing livestock to graze cover crops",
  "8. Termination of cover crop issues",
  "9. Pest issues",
  "10. It limits my herbicide options"
))

# Perform chi-square test
test <- chisq.test(table(CD2$Poll_Question, CD2$Count))
test

# Plot the obstacles
CD2 %>%
  ggplot(aes(Poll_Option, Count)) +
  geom_bar(aes(fill = Location), stat = "identity") +
  facet_wrap(~Poll_Question, ncol = 5, labeller = label_wrap_gen()) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Ratings", y = "Number of Response") +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = c(0.8, 0.95),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.direction = "horizontal",
    axis.text = element_text(size = 15, face = "bold")
  )

    