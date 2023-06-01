#Nitrogen ----
#Data-frames for nitrogen ratings 
BB <- BrokenBow[c(186:223), c(3:5)]
HK <- Hickman[c(222:266),c(3:5)]
LK <- Lincoln[c(131:170), c(3:5)]
MD <- Mead[c(194:238),c(3:5)]
NF <- Norflok[c(217:261),c(3:5)]
YK <- York[c(177:221),c(3:5)]
BR <- Bridgeport_NPK[c(1:45), c(3:5)]


BR$Poll.Question <- as.character(BR$Poll.Question)
BR$Count <- as.numeric(BR$Count)
BR$Poll.Option <- as.character(BR$Poll.Option)
str(BR)

CD_N <- full_join(BB, HK, by = c("Poll.Question", "Poll.Option")) %>% full_join(.,LK, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(.,MD, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(.,NF, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(.,YK, by = c("Poll.Question", "Poll.Option")) %>%
  full_join(.,BR, by = c("Poll.Question", "Poll.Option"))

setnames(CD_N, old = c("Count.x", "Count.y","Count.x.x", "Count.y.y", "Count.x.x.x", "Count.y.y.y", "Count"), new = c("Broken Bow", "Hickman", "Lincoln", "Mead", "Norfolk", "York", "Bridgeport"))
names(CD1) <- gsub("\\.", "_", names(CD_N))

CD_N2 <- gather(CD_N, key = "Location", value = "Count", -Poll.Question, -Poll.Option)


#Stacked Bar plot
CD_N2$Poll.Question <- factor(CD_N2$Poll.Question, 
                              levels = c("1. The cost of the fertilizer", 
                                         "2. Improved crop genetics and/or crop hybrids",
                                         "3. I use tissue tests or other tests to sidedress ",
                                         "4. I adjust my N rate as based on my soil tests",
                                         "5. Past experience plus advice from my agronomy team",
                                         "6. Government mandates require reduced rates",
                                         "7. I use in-season climate models to base my nutrient rates",
                                         "8. I prefer to fertigate or chemigate in-season vs applying all N in the fall or spring ",
                                         "9. I reduce my N rate based on the previous legume crop (soybeans, alfalfa) or manure and/or Irrigation water usage
"))

View(CD2)
test <- chisq.test(table(CD2$Poll.Question,CD2$Count))
test
#plot for the nitrogen data 
CD_N2 %>%
  ggplot(aes(Poll.Option, Count, fill = Location)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Poll.Question, ncol = 5, labeller = label_wrap_gen()) +
  labs(x = "Ratings", y = "Number of Responses") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 13),
    legend.position = c(0.95, 0.3),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.direction = "vertical"
  )
