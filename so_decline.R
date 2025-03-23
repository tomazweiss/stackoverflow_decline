
library(tidyverse)
library(janitor)


# data import -------------------------------------------------------------

data <- read_csv('data/QueryResults_month.csv') |> clean_names()


# data preparation --------------------------------------------------------

data <- data |> mutate(month = ymd(month))


starting_values <- 
  data |> 
  filter(month == as.Date("2022-11-01")) |> 
  group_by(tag_name) |> 
  summarise(n_questions_start = sum(n_questions))

ending_values <- 
  data |> 
  filter(month == as.Date("2024-11-01")) |> 
  group_by(tag_name) |> 
  summarise(n_questions_end = sum(n_questions))

data_plot <- 
  starting_values |> 
  left_join(ending_values, by = 'tag_name') |> 
  mutate(n_questions_end = replace_na(n_questions_end, 0)) |> 
  mutate(ratio = n_questions_end / n_questions_start) |> 
  mutate(decline_pct = -100*(1 - ratio)) |> 
  arrange(decline_pct)


data_plot <- data_plot |> 
  mutate(tag_name = paste0(tag_name, ' (', n_questions_start, ')')) 


data_plot$tag_name <- factor(data_plot$tag_name, levels = data_plot$tag_name)



# plot --------------------------------------------------------------------

fig <- 
  data_plot |> 
  ggplot(aes(x = decline_pct, y = tag_name)) +
  geom_col(fill = "steelblue") +
  theme_light() +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"), # Add % symbol to labels
    breaks = seq(-100, 0, by = 10),
    expand = expansion(add = c(0.5, 0.5)),
    sec.axis = sec_axis(~ ., 
                        breaks = seq(-100, 0, by = 10), 
                        labels =  function(x) paste0(x, "%")
    )    
  ) +
  labs(
    title = "Percentage Drop in Number of Stack Overflow Questions by their Tag",
    subtitle = paste0("Comparing November 2022 to November 2024.\n",
              "Showing 75 most popular tags of 2022. Numbers in brackets next to tag names represent number of questions in November 2022."),
    x = "Percentage Decrease",
    y = "Tag Name"
  ) +
  annotate("label", x = 0, y = 0, 
         label = "tomazweiss.github.io \nData Source: StackExchange Data Explorer", hjust = 1.03, vjust = -0.3, 
         size = 3.5, fill = "white", label.size = 0)

fig

ggsave('plots/so_decline.png', plot = fig, width = 300, height = 300, units = 'mm', dpi = 'retina')

