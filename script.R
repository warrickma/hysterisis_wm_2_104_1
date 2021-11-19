library(ggplot2)
library(scales)
library(svglite)
data = read.csv("cycle.csv")
str(data)
data = data[-(1:3)]
colnames(data) = c("Strain", "Stress")
data$Stress = data$Stress/1000
plot(data$Strain, data$Stress)
data$Cycle = NA
processed = data
for (i in 1:nrow(data))
{
  if (i/70 < 1)
  {
    data[i,3] = "Cycle 1"
  }
}
for (i in 1:nrow(data))
{
  if (i/70 > 9 & i/70 < 10 )
  {
    data[i,3] = "Cycle 10"
  }
}
for (i in 1:nrow(data))
{
  if (i/70 < 25 & i/70 > 24)
  {
    data[i,3] = " Cycle 25"
  }
}
for (i in 1:nrow(data))
{
  if (i/70 < 49 & i/70 > 48)
  {
    data[i,3] = "Cycle 50"
  }
}
processed = data[complete.cases(data), ]
processed$Strain = processed$Strain + 0.0137 
plot(processed$Strain, processed$Stress)
ggplot(processed) +
  geom_path(aes(x = Strain, y = Stress, color = Cycle), size = 1) +
  scale_color_manual(name = "Cycle",
                     labels = c("Cycle 1", "Cycle 10", "Cycle 25", "Cycle 50"),
                     values = c("#006699", "#6EB43F", "#F8981D", "#073949")) +
  labs(x = "Strain", y = "Stress (kPa)") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(limits = c(0,0.12), breaks=extended_breaks(n=4), labels = scales::percent) + 
  theme_classic() +
  theme(
    axis.text.x = element_text(size=10, color = "black", face = "bold"),
    axis.text.y = element_text(size=10, color = "black", face = "bold"),
    axis.title = element_text(size=10, color = "black", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=10, color = "black", face = "bold"))
ggsave("cycle.svg", width = 80, height = 80, units = "mm")
levels(processed$Cycle)
