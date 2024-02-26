library(ggsomestat)
HEC <- as.data.frame(HairEyeColor)
ggplot(HEC, aes(x=Freq, colour=Sex))+
  stat_countgrid(shape=15)+
  facet_grid(Hair~Eye, labeller = label_both)

ggsave("HEC.png")

# Titanic <- as.data.frame(Titanic)
# head(Titanic)
# ggplot(Titanic, aes(x=Freq, colour=Survived))+
#   stat_countgrid(size=0.25, n_col = 20, shape=15)+
#   facet_grid(Sex~Class, labeller = label_both)
