dataM <- function_data_analysis(Sexv="M")
dataW <- function_data_analysis(Sexv="W")

c1 <- ggplot(data= dataM, aes(x = Age_cat, y = visceral.adipose.tissue.value)) +
  geom_boxplot() +
  xlab("") +
  ylab("Visceral adipose tissue (kg)") +
  ggtitle("Men")+
  ylim(0,10)
             
             
c2 <- ggplot(data= dataW,  aes(x = Age_cat, y = visceral.adipose.tissue.value)) +
  geom_boxplot() +
  xlab("") +
  ylab("") +
  ggtitle("Women")+
  ylim(0,10)

c3 <- ggplot(data= dataM, aes(x = Age_cat, y = Relative.fat.mass.value)) +
  geom_boxplot() +
  xlab("") +
  ylab("Relative fat mass (%)") +
  ylim(0,50)


c4 <- ggplot(data= dataW, aes(x = Age_cat, y = Relative.fat.mass.value)) +
  geom_boxplot() +
  xlab("") +
  ylab("") +
  ylim(0,50)

c5 <- ggplot(data= dataM, aes(x = Age_cat, y = SMI)) +
  geom_boxplot() +
  xlab("Men age category") +
  ylab("Skeletal muscle mass index (kg/m2)") +
  ylim(0,15)

c6 <- ggplot(data= dataW, aes(x = Age_cat, y = SMI)) +
  geom_boxplot() +
  xlab("Women age category") +
  ylab("") +
  ylim(0,15)


pdf("body_composition.pdf", width=8, height=10)
ggarrange(c1, c2, c3, c4, c5, c6, ncol = 2, nrow = 3, font.label = list(size = 30))
dev.off()
