pacman::p_load(
  readxl,      # load function for reading excel file
  tidyverse,   # load functions for data manipulation and visualization
  ggthemes,    # load pretty themes
  imputeTS,     # imputation of missing values
  esquisse,
  here,
  knitr,
  readr,
  geojsonio,
  lubridate,
  data.table, 
  fixest, 
  DescTools, 
  magrittr,
  plotly,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  httr,
  dotenv,
  jsonlite,
  broom
)

setwd("/Users/alexmirugwe/dr_emmy_recency")

df_recency <- read.csv("Recency.csv")

df_recency <- df_recency %>%
  mutate(UVRI.RITA = ifelse(nzchar(UVRI.RITA) == FALSE & RTRI.Field.Result == "LT", RTRI.Field.Result, UVRI.RITA))

df_recency <- df_recency %>%
  filter(UVRI.RITA %in% c("LT","Recent"))

table(df_recency$UVRI.RITA)

df_recency <- df_recency%>%
  mutate(age_group = age_categories(Age,
                                    breakers = c(15,20,25,30,35,40,45,50)))

write.xlsx(df_recency, "Recency_data_modified.xlsx")

map_df <- df_recency %>% 
  group_by(District, UVRI.RITA) %>% 
  summarise(count = n(),.groups = "drop") 

merge_df <- map_df %>% 
  group_by(District) %>% 
  summarise(num = sum(count)) 

recency_df <- map_df %>% 
  filter(UVRI.RITA=="Recent")%>%
  arrange(desc(count))

sf_df_merge <- merge( x=recency_df, y=merge_df, all = TRUE)  

sf_df_merge$count[is.na(sf_df_merge$count)] <- 0

sf_df_merge <- sf_df_merge %>% 
  mutate(prop = round(count/num*100,2)) %>% 
  select(District,prop) %>% 
  arrange(desc(prop))


long_df <- map_df %>% 
  filter(UVRI.RITA=="LT")%>%
  select(-UVRI.RITA) %>% 
  arrange(desc(count))

long_df_merge <- merge( x=long_df, y=merge_df, all = TRUE)  

long_df_merge$count[is.na(long_df_merge$count)] <- 0

long_df_merge <- long_df_merge %>% 
  mutate(prop = round(count/num*100,2)) %>% 
  select(District,prop) %>% 
  arrange(desc(prop))

uganda_districts$District <- replace(uganda_districts$District, uganda_districts$District == 'NAMUTUNMBA','NAMUTUMBA' )
uganda_districts$District <- replace(uganda_districts$District, uganda_districts$District == 'LUWERO', 'LUWEERO')
uganda_districts$District <- replace(uganda_districts$District, uganda_districts$District == 'KASSNDA','KASSANDA' )


uganda_districts$District <- replace(uganda_districts$District, uganda_districts$District == 'SSEMBABULE','SEMBABULE')

sf_df_merge$District <- toupper(sf_df_merge$District)

long_df_merge$District <- toupper(long_df_merge$District)

uganda_districts <- st_read("/Users/alexmirugwe/Library/CloudStorage/OneDrive-MonitoringandEvaluationTechnicalSupport/Recency/Recency/uganda_districts.shp")

water <- st_read("/Users/alexmirugwe/Jinja_workshop/code/analytics/UGA_wat/UGA_water_areas_dcw.shp")

sf_df <- merge( x=uganda_districts, y=sf_df_merge, all = TRUE)

pdf('recency_map_2022.pdf')
ggplot() +
  ggtitle("HIV Recenct Infections, per 100 clients") +
  geom_sf(data = sf_df, aes(fill = cut(prop, 
                                       breaks = c(0,9, 24, 49,Inf),
                                                 labels = c("0-9","10-24", "25-49","50-100"), 
                                                 right = FALSE))) +
  geom_sf(data = water, fill = "lightblue")+
  scale_fill_manual(values = c("darkgreen","#1f77b4","yellow","red" ),
                    name = "# Recent Infections",
                    labels = c("0-9","10-24", "25-49","50-100","Not Implementing")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
dev.off()


sf_df_long <- merge( x=uganda_districts, y=long_df_merge, all = TRUE)

pdf('long_map_2022.pdf')
ggplot() +
  ggtitle("HIV Long-Term Infections,per 100 clients") +
  geom_sf(data = sf_df_long, aes(fill = cut(prop, 
                                       breaks = c(0, 49, 99, Inf),
                                       labels = c("0-49", "50-99","100"), 
                                       right = FALSE))) +
  geom_sf(data = water, fill = "lightblue")+
  scale_fill_manual(values = c("darkgreen","#1f77b4", "red" ),
                    name = "# LT Infections",
                    labels = c("0-49", "50-99","100","Not Implementing")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
dev.off()

df_recency <- df_recency %>% 
  mutate(Sex = ifelse(nzchar(Sex) == FALSE , NA, Sex))%>%
  drop_na(Sex,age_group) 

df_recency%>%
  group_by(age_group) %>% 
  summarise(count = n(),.groups = "drop")

sex_recency <- df_recency %>% 
  filter(UVRI.RITA == "LT") %>% 
  mutate(Sex = ifelse(nzchar(Sex) == FALSE, NA, Sex)) %>%
  drop_na(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%  
  mutate(proportion = count / sum(count) * 100) 


age_recency <- df_recency %>% 
  filter(UVRI.RITA=="LT") %>% 
  #mutate(age_group = ifelse(nzchar(age_group) == FALSE , NA, age_group))%>%
  drop_na(age_group) %>% 
  group_by(age_group) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%  
  mutate(proportion = count / sum(count) * 100) 

sex_age <- df_recency %>% 
  filter(UVRI.RITA=="Recent") %>% 
  mutate(Sex = ifelse(nzchar(Sex) == FALSE , NA, Sex))%>%
  drop_na(Sex,age_group) %>%
  group_by(Sex, age_group) %>% 
  summarise(count = n(),.groups = "drop")


total_counts <- sex_age %>%
  group_by(Sex) %>%
  summarize(total_count = sum(count))

sex_age_with_totals <- sex_age %>%
  left_join(total_counts, by = "Sex") %>%
  mutate(proportion = count / total_count*100)
  


# Calculate standard error
#Standard Errors: These values indicate the standard error associated with the estimated proportions in each 
#age group. It's a measure of how much the sample proportions are likely to vary from the true population 
#proportions.
sex_age_with_totals$standard_errors <- sqrt(sex_age_with_totals$proportion * (100 - sex_age_with_totals$proportion) / total_count)

# Calculate confidence interval
critical_value <- qnorm(0.975)  # For 95% confidence interval
sex_age_with_totals$confidence_intervals_lower <- sex_age_with_totals$proportion - critical_value * sex_age_with_totals$standard_errors
sex_age_with_totals$confidence_intervals_upper <- sex_age_with_totals$proportion + critical_value * sex_age_with_totals$standard_errors 

# Print the result
print(age_recency)


sex <- df_recency %>% 
  filter(UVRI.RITA=="Recent") %>% 
  mutate(Sex = ifelse(nzchar(Sex) == FALSE , NA, Sex))%>%
  drop_na(Sex) %>%
  group_by(Sex) %>% 
  summarise(count = n(),.groups = "drop")

total_counts <- sex %>%
  mutate(total_count = sum(count),proportion = count / total_count*100)

total_counts$standard_errors <- sqrt(total_counts$proportion * (100 - total_counts$proportion) / total_count)


# Calculate confidence interval
critical_value <- qnorm(0.975)  # For 95% confidence interval
total_counts$confidence_intervals_lower <- total_counts$proportion - critical_value * total_counts$standard_errors
total_counts$confidence_intervals_upper <- total_counts$proportion + critical_value * total_counts$standard_errors 


df_recency %>% 
  filter(UVRI.RITA == "Recent",Sex == "M") %>% 
  #mutate(Sex = ifelse(nzchar(Sex) == FALSE, NA, Sex)) %>%
  #drop_na(Sex) %>% 
  group_by(age_group) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%  
  mutate(proportion = count / sum(count) * 100) 


df_recency %>% 
  filter(UVRI.RITA == "Recent",Sex == "F") %>% 
  #mutate(Sex = ifelse(nzchar(Sex) == FALSE, NA, Sex)) %>%
  #drop_na(Sex) %>% 
  group_by(age_group) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%  
  mutate(proportion = count / sum(count) * 100) 





