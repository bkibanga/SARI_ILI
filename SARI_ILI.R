pacman::p_load(
  rio,          # for importing data
  here,         # for file paths
  janitor,      # for data cleaning
  lubridate,    # for working with dates
  flextable,    # for making pretty tables
  tidyverse,     # for data management and visualisation
  skimr,         # create good summary
  dplyr,
  epikit,        # creating age category
  gtsummary,     # summarizing/create data
  ggExtra,       # 
  scales,        
  tsibble,      
  viridis,      
  apyramid,      
  kableExtra,     
  knitr,
  splitstackshape,
  gt,
  palmerpenguins
)
wk38<- wk38%>% clean_names()

#Remove 2023 from weekly column
wk38$weeks <- gsub(2023, "", wk38$weeks)

wk38$sites <- factor(wk38$sites)

# Create the scatter plot with lines
ggplot(wk38, aes(weeks, y = ili_cases_enrolled, color = "ILI", group = sites)) +
  geom_line(linetype = "solid", linewidth = 1) +
  geom_point(aes(color = "ILI"), shape = 16) +
  geom_point(aes(y = sari_cases_enrolled, color = "SARI"), shape = 16) +
  facet_wrap(~ sites) +
  scale_y_continuous(
    breaks = c(2, 4, 6, 8, 10, 12),
    labels = c("2", "4", "6", "8", "10", "12"),
    limits = c(0, 12)
  ) +
  theme(
    strip.background = element_rect(fill = "seagreen"),
    strip.text = element_text(color = "white", size = 12)
  ) +
  labs(
    title = "SARI/ILI Cases Enrolled Wk38(18th - 24th Sept 23)",
    x = "Epi Weeks",
    y = "Enrolled cases"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(
    values = c(ILI = "blue", SARI = "red"),  
    name = "Cases"
  ) +
  theme(legend.position = "top")




