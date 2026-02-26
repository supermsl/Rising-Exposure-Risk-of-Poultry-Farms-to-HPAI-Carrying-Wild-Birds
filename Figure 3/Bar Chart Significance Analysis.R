#
rm(list = ls()) 
library(tidyverse)
library(reshape2)
library(multcomp)
library(ggplot2)

#
df <- read.csv("2099.csv")

#
df_long <- melt(df,
                id.vars = c("Longitude", "Latitude", "Poultry"),
                measure.vars = c("X2099A1B45", "X2099A245", "X2099B145", "X2099B245",
                                 "X2099A1B85", "X2099A285", "X2099B185", "X2099B285"),
                variable.name = "Scenario", value.name = "Value")

#
df_long$Scenario <- gsub("^X2099", "", df_long$Scenario)
df_long <- df_long %>%
  mutate(
    Landuse = case_when(
      grepl("A1B", Scenario) ~ "A1B",
      grepl("A2", Scenario) ~ "A2",
      grepl("B1", Scenario) ~ "B1",
      grepl("B2", Scenario) ~ "B2"
    ),
    Climate = case_when(
      grepl("45", Scenario) ~ "45",
      grepl("85", Scenario) ~ "85"
    )
  )


df_long$Landuse <- factor(df_long$Landuse, levels = c("A1B", "A2", "B1", "B2"))
df_long$Climate <- factor(df_long$Climate, levels = c("45", "85"))

#
df_long$Interaction <- interaction(df_long$Landuse, df_long$Climate)

#
model_spatial <- lm(Value ~ Landuse * Climate + Longitude + Latitude,
                    data = df_long,
                    weights = Poultry)

#
cat("=== Model Summary ===\n")
print(summary(model_spatial))

cat("\n=== ANOVA Result ===\n")
print(anova(model_spatial))

#
model_inter <- lm(Value ~ Interaction + Longitude + Latitude,
                  data = df_long,
                  weights = Poultry)

tukey_result <- glht(model_inter, linfct = mcp(Interaction = "Tukey"))
cat("\n=== Tukey HSD Results ===\n")
print(summary(tukey_result))

#
ggplot(df_long, aes(x = Interaction, y = Value, fill = Landuse)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "2033 Avian Prevalence by Scenario",
       x = "Scenario (Landuse × Climate)",
       y = "Prevalence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 
ggplot(df_long, aes(x = Longitude, y = Latitude)) +
  geom_point(aes(color = Value, size = Poultry)) +
  facet_wrap(~ Interaction) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Distribution of Prevalence by Scenario",
       x = "Longitude", y = "Latitude")

