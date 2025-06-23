library(ggplot2)
library(readxl)
library(reshape2)
library(scales)
library(RColorBrewer)
library(patchwork)
library(tidyr)
library(dplyr)
yields <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/OTC_model(2).xlsx", 
                           sheet = "figure data", range = "A48:M68")
prices_yields <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/OTC_model(2).xlsx", 
                                            sheet = "figure data", range = "A71:AK91")
average_prices_OTC_ACP <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx", 
                           sheet = "Figure Data", range = "A55:Y75")
high_prices_OTC_ACP <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx", 
                           sheet = "Figure Data", range = "A80:Y100") 
original_spraying<-read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/original_spraying_model.xlsx",
                              sheet = "Figure Data", range = "A2:D22")
yields_spraying<-read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/original_spraying_model.xlsx",
                              sheet = "Figure Data", range = "F2:J22")
hlb_severity<-read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/original_spraying_model.xlsx",
                         sheet = "Figure Data", range = "K2:N22")
yields_OTC_ACP<-read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx",
                           sheet = "Figure Data", range = "A104:F124")
average_prices_ACP <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx", 
                                     sheet = "Figure Data", range = "A126:M146")
high_prices_ACP <- read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx", 
                                  sheet = "Figure Data", range = "A148:M168") 
otc_yields<-read_excel("~/JOBS/RESEARCH ASSISTANT, ECONOMICS DEPARTMENT AT SAC STATE/Citrus ACP & HLB Risk Project/Literature/OTC/Data/spraying_otc_budget_model.xlsx",
                       sheet = "Figure Data", range = "A172:F192")
save(yields, prices_yields, average_prices_OTC_ACP,
     high_prices_OTC_ACP, original_spraying,yields_spraying,hlb_severity, yields_OTC_ACP,
     average_prices_ACP, high_prices_ACP,otc_yields,
     file = "OTCdata.RData")
load("OTCdata.RData")

# Visualizations in the effects of the conjunction of Oxytetracycline and ACP
# spraying. 

#Average Prices

long_average_prices_OTC_ACP <- average_prices_OTC_ACP %>%
  pivot_longer(
    cols = -year, # All columns except 'year'
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Branch = case_when(
      Variable %in% c("Avgprice_lYield_NA", "Avgprice_Lyield_LOTC", 
                      "Avgprice_Lyield_AvgOTC", "Avgprice_Lyield_HighOTC",
                      "Avgprice_LowYields_LowOTC", 
                      "Avgprice_LowYields_AvgOTC", 
                      "Avgprice_LowYields_HighOTC",
                      "lowyield_healthy") ~ "Low Yields",
      Variable %in% c("Avgprice_AvgYield_NA", 
                      "Avgprice_AvgYield_LOTC", 
                      "Avgprice_AvgYield_AvgOTC", 
                      "Avgprice_AvgYield_HOTC", 
                      "Avgprice_AvgYields_LowOTC", 
                      "Avgprice_AvgYields_AvgOTC", 
                      "Avgprice_AvgYields_HighOTC",
                      "avgyield_healthy") ~ "Average Yields",
      Variable %in% c("Avgprice_HYield_NA", 
                      "Avgprice_HYield_LOTC", 
                      "Avgprice_HYield_AvgOTC", 
                      "Avgprice_HYield_HOTC", 
                      "Avgprice_HighYields_LowOTC", 
                      "Avgprice_HighYields_AvgOTC", 
                      "Avgprice_HighYields_HighOTC",
                      "highyield_healthy") ~ "High Yields",
      TRUE ~ "Unknown"
    )
  )


#FINAL VERSION OF THIS VISUALIZATION FOR AVERAGE PRICES
long_average_prices_OTC_ACP <- long_average_prices_OTC_ACP %>%
  mutate(Branch = factor(Branch, levels = c("Low Yields", "Average Yields", "High Yields")))  # Custom order

long_average_prices_OTC_ACP$Treatment <- factor(long_average_prices_OTC_ACP$Variable, 
                                                levels = c("Avgprice_AvgYield_NA", "Avgprice_Lyield_NA", "Avgprice_HYield_NA",
                                                           "Avgprice_AvgYield_LOTC", "Avgprice_Lyield_LOTC", "Avgprice_HYield_LOTC",
                                                           "Avgprice_AvgYield_AvgOTC", "Avgprice_Lyield_AvgOTC", "Avgprice_HYield_AvgOTC",
                                                           "Avgprice_AvgYield_HOTC", "Avgprice_Lyield_HighOTC", "Avgprice_HYield_HOTC",
                                                           "Avgprice_AvgYields_LowOTC", "Avgprice_LowYields_LowOTC", "Avgprice_HighYields_LowOTC",
                                                           "Avgprice_AvgYields_AvgOTC", "Avgprice_LowYields_AvgOTC", "Avgprice_HighYields_AvgOTC",
                                                           "Avgprice_AvgYields_HighOTC", "Avgprice_LowYields_HighOTC", "Avgprice_HighYields_HighOTC",
                                                           "lowyield_healthy","avgyield_healthy","highyield_healthy"),
                                                labels = c("No Action", "No Action", "No Action",
                                                           "Low OTC", "Low OTC", "Low OTC",
                                                           "Average OTC", "Average OTC", "Average OTC",
                                                           "High OTC", "High OTC", "High OTC",
                                                           "Low OTC and Spraying", "Low OTC and Spraying", "Low OTC and Spraying",
                                                           "Average OTC and Spraying", "Average OTC and Spraying", "Average OTC and Spraying",
                                                           "High OTC and Spraying", "High OTC and Spraying", "High OTC and Spraying",
                                                           "Healthy","Healthy","Healthy"))


ggplot(long_average_prices_OTC_ACP, aes(x = year, y = Value, color = Treatment)) +
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.75) +
  facet_wrap(~Branch, nrow = 2, ncol = 2, scales = "free_y") +
  labs(
    title = NULL,
    x = "Year",
    y = "Cumulative discounted profits ($)",
    color = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = scales::pretty_breaks(6)) +
  scale_color_manual(
    values = c(
      "No Action" = "red",
      "Low OTC" = "#00CED1",
      "Average OTC" = "black",
      "High OTC" = "darkgreen",
      "Low OTC and Spraying" = "orange",
      "Average OTC and Spraying" = "purple",
      "High OTC and Spraying" = "green",
      "Healthy" = "blue"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "serif"),
    legend.position = c(0.8, 0.2),
    strip.text = element_text(face = "bold"))

#High Prices

long_high_prices_OTC_ACP <- high_prices_OTC_ACP %>%
  pivot_longer(
    cols = -year, # All columns except 'year'
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Branch = case_when(
      Variable %in% c("Highprice_lYield_NA",
                      "Hprice_Lyield_LOTC",
                      "Hprice_Lyield_AvgOTC",
                      "Hprice_Lyield_HOTC",
                      "Highprice_LowYields_LowOTC",
                      "Highprice_LowYields_AvgOTC",
                      "Highprice_LowYields_HighOTC",
                      "lowyield_healthy") ~ "Low Yields",
      Variable %in% c("Highprice_AvgYield_NA",
                      "Hprice_AvgYield_LOTC",
                      "Hprice_AvgYield_AvgOTC",
                      "Hprice_AvgYield_HOTC",
                      "Highprice_AvgYields_LowOTC",
                      "Highprice_AvgYields_AvgOTC",
                      "Highprice_AvgYields_HighOTC",
                      "avgyield_healthy") ~ "Average Yields",
      Variable %in% c("Highprice_HYield_NA",
                      "Hprice_HYield_LOTC",
                      "Hprice_HYield_AvgOTC",
                      "Hprice_HYield_HOTC",
                      "Highprice_HighYields_LowOTC",
                      "Highprice_HighYields_AvgOTC",
                      "Highprice_HighYields_HighOTC",
                      "highyield_healthy") ~ "High Yields",
      TRUE ~ "Unknown"
    )
  )


long_high_prices_OTC_ACP <- long_high_prices_OTC_ACP %>%
  mutate(Branch = factor(Branch, levels = c("Low Yields", "Average Yields", "High Yields")))

long_high_prices_OTC_ACP$Treatment <- factor(long_high_prices_OTC_ACP$Variable, 
                                                levels = c("Highprice_lYield_NA", "Highprice_AvgYield_NA", "Highprice_HYield_NA",
                                                           "Hprice_Lyield_LOTC", "Hprice_AvgYield_LOTC", "Hprice_HYield_LOTC",
                                                           "Hprice_Lyield_AvgOTC", "Hprice_AvgYield_AvgOTC", "Hprice_HYield_AvgOTC",
                                                           "Hprice_Lyield_HOTC", "Hprice_AvgYield_HOTC", "Hprice_HYield_HOTC",
                                                           "Highprice_LowYields_LowOTC", "Highprice_AvgYields_LowOTC", "Highprice_HighYields_LowOTC",
                                                           "Highprice_LowYields_AvgOTC", "Highprice_AvgYields_AvgOTC", "Highprice_HighYields_AvgOTC",
                                                           "Highprice_LowYields_HighOTC", "Highprice_AvgYields_HighOTC", "Highprice_HighYields_HighOTC",
                                                           "lowyield_healthy","avgyield_healthy","highyield_healthy"),
                                                labels = c("No Action", "No Action", "No Action",
                                                           "Low OTC", "Low OTC", "Low OTC",
                                                           "Average OTC", "Average OTC", "Average OTC",
                                                           "High OTC", "High OTC", "High OTC",
                                                           "Low OTC and Spraying", "Low OTC and Spraying", "Low OTC and Spraying",
                                                           "Average OTC and Spraying", "Average OTC and Spraying", "Average OTC and Spraying",
                                                           "High OTC and Spraying", "High OTC and Spraying", "High OTC and Spraying",
                                                           "Healthy","Healthy","Healthy"))
#FINAL VERSION OF THIS VISUALIZATION: HIGH PRICES
ggplot(long_high_prices_OTC_ACP, aes(x = year, y = Value, color = Treatment)) +
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.75) +
  facet_wrap(~Branch, nrow = 2, ncol = 2, scales = "free_y") +
  labs(
    title = NULL,
    x = "Year",
    y = "Cumulative discounted profits ($)",
    color = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = scales::pretty_breaks(5)) +
  scale_color_manual(
    values = c(
      "No Action" = "red",
      "Low OTC" = "#00CED1",
      "Average OTC" = "black",
      "High OTC" = "darkgreen",
      "Low OTC and Spraying" = "orange",
      "Average OTC and Spraying" = "purple",
      "High OTC and Spraying" = "green",
      "Healthy" = "blue"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.8, 0.2),
        strip.text = element_text(face = "bold"))


#Original spraying model with no otc

original_spraying_long<-melt(
  original_spraying, id.vars = "year",
  variable.name = "efficacy",
  value.name = "profits"
)

ggplot(original_spraying_long, aes(x = year, y = profits, color = efficacy)) +
  geom_line(data = subset(original_spraying_long, efficacy != "Healthy"), linewidth = 0.7) +
  geom_point(data = subset(original_spraying_long, efficacy == "Healthy"), size = 1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  labs(title = NULL, x = "Year", y = "Cumulative Discounted Profits ($)",
       color = NULL) +
  scale_y_continuous(labels = dollar_format(), 
                     breaks = pretty_breaks(10)) +
  scale_color_manual(values = c("Healthy" = "darkgreen",
                                "0.9" = "green",
                                "0.8" = "#00F5FF",
                                "0.7" = "red"),
                     labels = c("Healthy" = "Healthy",
                                "0.9" = "Spray (0.9)",
                                "0.8" = "Spray (0.8)",
                                "0.7" = "Spray (0.7)")) +  
  theme_classic(base_size = 12) + theme(text = element_text(family = "serif"),
                                       legend.position = "bottom")
yields_spraying_long<-melt(
  yields_spraying, id.vars = "year",
  variable.name = "efficacy",
  value.name = "yield"
)

ggplot(yields_spraying_long, aes(x = year, y = yield, color = efficacy)) +
  geom_line(linewidth = 0.7) +
  labs(title = NULL, x = "Year", y = "Yield per acre (boxes)",
       color = NULL) +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(20)) +
  scale_color_manual(values = c("Healthy" = "blue",
                                "0.8" = "purple",
                                "0.7" = "green",
                                "no_action" = "red"),
                     labels = c("Healthy" = "Healthy",
                                "0.8" = "Spray (0.8)",
                                "0.7" = "Spray (0.7)",
                                "no_action" = "No Action")) +  
  theme_classic(base_size = 12) + theme(text = element_text(family = "serif"),
                                        legend.position = "bottom")
# Hlb severity
hlb_severity_long<-melt(
  hlb_severity, id.vars = "year",
  variable.name = "efficacy",
  value.name = "severity"
)
`
ggplot(hlb_severity_long, aes(x = year, y = severity, color = efficacy)) +
  geom_line(data = dplyr::filter(hlb_severity_long, efficacy != "Healthy"), linewidth = 0.7) +
  geom_point(data = dplyr::filter(hlb_severity_long, efficacy == "Healthy"), size = 1) +
  labs(title = NULL, x = "Year", y = "HLB %", color = NULL) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks(10)) +
  scale_x_continuous(breaks = unique(hlb_severity_long$year))+
  scale_color_manual(values = c("0.8" = "purple",
                                "0.7" = "green",
                                "no_action" = "red"),
                     labels = c("0.8" = "Spray (0.8)",
                                "0.7" = "Spray (0.7)",
                                "no_action" = "No Action")) +  
  theme_classic(base_size = 12) + 
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")

#Yields for OTC and spraying
yields_OTC_ACP_long<-melt(
  yields_OTC_ACP, id.vars = "Year",
  variable.name = "efficacy",
  value.name = "yield"
)

ggplot(yields_OTC_ACP_long, aes(x = Year, y = yield, color = efficacy)) +
  geom_line(linewidth = 0.75) +
  labs(title = NULL, x = "Year", y = "Yield per acre (boxes)",
       color = NULL) +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(20)) + 
  scale_color_manual(values = c("low_OTC_spray" = "orange",
                                "avg_OTC_spray" = "purple",
                                "high_OTC_spray" = "green",
                                "healthy" = "blue",
                                "yield_NA" = "red"),
                     labels = c("low_OTC_spray" = "Spray + Low OTC",
                                "avg_OTC_spray" = "Spray + Average OTC",
                                "high_OTC_spray" = "Spray + High OTC",
                                "healthy" = "Healthy",
                                "yield_NA" = "No Action")) +  
  theme_classic(base_size = 12) + theme(text = element_text(family = "serif"),
                                        legend.position = "bottom")


#Cumulative profits for just spraying at average prices
long_average_prices_ACP <- average_prices_ACP %>%
  pivot_longer(
    cols = -Year, # All columns except 'year'
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Branch = case_when(
      Variable %in% c("low_spray_lowyield", "avg_spray_lowyield", 
                      "NA_lowyield","lowyield_healthy") ~ "Low Yields",
      Variable %in% c("low_spray_avgyield", "avg_spray_avgyield",
                      "NA_avgyield","avgyield_healthy") ~ "Average Yields",
      Variable %in% c("low_spray_highyield", "avg_spray_highyield", 
                      "NA_highyield","highyield_healthy") ~ "High Yields",
      TRUE ~ "Unknown"
    )
  )

long_average_prices_ACP <- long_average_prices_ACP %>%
  mutate(Branch = factor(Branch, levels = c("Low Yields", "Average Yields", "High Yields")))  # Custom order

long_average_prices_ACP$Treatment <- factor(long_average_prices_ACP$Variable, 
                                                levels = c("low_spray_lowyield","low_spray_avgyield","low_spray_highyield",
                                                           "avg_spray_lowyield","avg_spray_avgyield","avg_spray_highyield",
                                                           "NA_lowyield","NA_avgyield","NA_highyield",
                                                           "lowyield_healthy","avgyield_healthy","highyield_healthy"),
                                                labels = c("Spray (0.7)", "Spray (0.7)", "Spray (0.7)",
                                                           "Spray (0.8)","Spray (0.8)","Spray (0.8)",
                                                           "No Action","No Action", "No Action",
                                                           "Healthy","Healthy","Healthy"))

ggplot(long_average_prices_ACP, aes(x = Year, y = Value, color = Treatment)) +
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.75) +
  facet_wrap(~Branch, nrow = 2, ncol = 2, scales = "free_y") +
  labs(
    title = NULL,
    x = "Year",
    y = "Cumulative discounted profits ($)",
    color = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format())+
  scale_color_manual(
    values = c(
      "Spray (0.7)" = "green",
      "Spray (0.8)" = "purple",
      "No Action" = "red",
      "Healthy" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.8, 0.2),
        strip.text = element_text(face = "bold"))

#Cumulative profits for just spraying at high prices
long_high_prices_ACP <- high_prices_ACP %>%
  pivot_longer(
    cols = -Year, # All columns except 'year'
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Branch = case_when(
      Variable %in% c("low_spray_lowyield", "avg_spray_lowyield", 
                      "NA_lowyield","lowyield_healthy") ~ "Low Yields",
      Variable %in% c("low_spray_avgyield", "avg_spray_avgyield",
                      "NA_avgyield","avgyield_healthy") ~ "Average Yields",
      Variable %in% c("low_spray_highyield", "avg_spray_highyield", 
                      "NA_highyield","highyield_healthy") ~ "High Yields",
      TRUE ~ "Unknown"
    )
  )

long_high_prices_ACP <- long_high_prices_ACP %>%
  mutate(Branch = factor(Branch, levels = c("Low Yields", "Average Yields", "High Yields")))  # Custom order

long_high_prices_ACP$Treatment <- factor(long_high_prices_ACP$Variable, 
                                            levels = c("low_spray_lowyield","low_spray_avgyield","low_spray_highyield",
                                                       "avg_spray_lowyield","avg_spray_avgyield","avg_spray_highyield",
                                                       "NA_lowyield","NA_avgyield","NA_highyield",
                                                       "lowyield_healthy","avgyield_healthy","highyield_healthy"),
                                            labels = c("Spray (0.7)", "Spray (0.7)", "Spray (0.7)",
                                                       "Spray (0.8)","Spray (0.8)","Spray (0.8)",
                                                       "No Action","No Action","No Action",
                                                       "Healthy","Healthy","Healthy"))
ggplot(long_high_prices_ACP, aes(x = Year, y = Value, color = Treatment)) +
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.75) +
  facet_wrap(~Branch, nrow = 2, ncol = 2, scales = "free_y") +
  labs(
    title = NULL,
    x = "Year",
    y = "Cumulative discounted profits ($)",
    color = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format())+
  scale_color_manual(
    values = c(
      "Spray (0.7)" = "green",
      "Spray (0.8)" = "purple",
      "No Action" = "red",
      "Healthy" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.8, 0.2),
        strip.text = element_text(face = "bold"))

#Effect of OTC in yields without spraying
otc_yields_long<-melt(
  otc_yields, id.vars = "Year",
  variable.name = "efficacy",
  value.name = "yield"
)

ggplot(otc_yields_long, aes(x = Year, y = yield, color = efficacy)) +
  geom_line(linewidth = 0.7) +
  labs(title = NULL, x = "Year", y = "Yield per acre (boxes)",
       color = NULL) +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(20))+
  scale_color_manual(values = c("healthy" = "blue",
                                "low_otc" = "orange",
                                "avg_otc" = "purple",
                                "high_otc" = "green",
                                "no_action" = "red"),
                     labels = c("healthy" = "Healthy",
                                "low_otc" = "Low OTC",
                                "avg_otc" = "Average OTC",
                                "high_otc" = "High OTC",
                                "no_action" = "No Action")) +  
  theme_classic(base_size = 12) + theme(text = element_text(family = "serif"),
                                        legend.position = "bottom")
