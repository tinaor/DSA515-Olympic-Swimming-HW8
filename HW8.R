# Homework 8
rm(list = ls())
setwd("~/Downloads/DSA 515/HW 8")

# Part 1 - Getting Started

## load required libraries
library(ggplot2)
library(dplyr)
# install.packages("plotly")
library(plotly)
library(scales)

## load swim data
swimming <- read.csv("Olympic_Swimming_1912-2020.csv")

## quick data exploration
str(swimming)
head(swimming)
summary(swimming)

# Part 2 - Olympic Games Over Time

## count events per year and location
events_per_year <- swimming %>%
  group_by(Year, Location) %>%    # group by year and host city
  summarise(total_events = n(),   # count total events
            .groups = 'drop')     # remove grouping

## create line plot
plot1 <- ggplot(data = events_per_year, aes(x = Year, y = total_events)) +
  geom_line(color = "blue", size = 2)  +  # line to connect the points
  geom_point(color = "red", size = 3) +   # show individual Olympics
  labs(title = "Growth of Swimming Events in Olympic Games",
       subtitle = "Number of swimming events per Olympics (1912-2020)",
       x = "Olympic Year",
       y = "Number of Swimming Events") +
  theme_minimal() 

print(plot1)

# Part 3 - Performance by Gender and Stroke

## remove missing values and prepare data
clean_swimming <- swimming %>%
  filter(!is.na(Results)) %>%   # remove rows with missing times
  filter(Results > 0) %>%       # remove invalid times
  filter(Relay. == "No")        # focus on individual events only

## create box and whisker plot
plot2 <- ggplot(clean_swimming, aes(x = Stroke, y = Results, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +     # box and whisker plot
  facet_wrap(~Gender) +   # separate panels for men and women
  coord_flip() +       # better readability if stroke names overlap
  labs(title = "Swimming Performance by Stroke and Gender",
       subtitle = "Distribution of race times (in seconds)",
       x = "Swimming Stroke",
       y = "Race Time (seconds)") + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_minimal()
                    
print(plot2)

# Part 4 - Medal Winners Over Time

## filter and count medals
gold_medals <- swimming %>% 
  filter(Rank == 1) %>%     # only gold medals
  count(Team, sort = TRUE) %>%  # count by country and sort
  slice_head(n = 10)    # keep the top 10 countries

## create bar chart
plot3 <- ggplot(gold_medals, aes(x = reorder(Team, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +   # bar chart
  geom_text(aes(label = n), hjust = -0.1, size = 3.5) +   # put numbers on bars
  coord_flip() +  # horizontal bars
  labs(title = "Top 10 Countries by Olympic Swimming Gold Medals",
       subtitle = "Total gold medals won (1912-2020)",
       x = "Country",
       y = "Number of Gold Medals") +
  theme_minimal()

print(plot3)

# Part 5 - Interactive Timeline

## prepare summary data
time_trends <- clean_swimming %>% 
  group_by(Year) %>% 
  summarise(avg_time = mean(Results, na.rm = TRUE), 
            total_events = n(),
            .groups = 'drop')

## create interactive line plot
plot4 <- plot_ly(time_trends,
                 x = ~Year,
                 y = ~avg_time,
                 type = 'scatter',
                 mode = 'lines+markers',
                 ## customize what appears when you hover
                 hovertemplate = paste(
                   "<b>Year: %{x} </b> <br >",
                   "Average Time: %{y:.2f} seconds <br>",
                   "Total Events: %{customdata}",
                   "<extra> </extra>"
                 ),
                 ## pass extra data for hover information
                 customdata = ~total_events,
                 line = list(color = 'rgb(0, 123, 191)',
                              width = 3),
                 marker = list(color = 'rgb(255, 65, 54)',
                               size = 6)) %>%
    layout(
      title = list(
        text = "Olympic Swimming Times Over the Years <br> <sub> Hover over points for details </sub>",
        font = list (size = 16)) ,
      xaxis = list (title = "Olympic Year") ,
      yaxis = list (title = "Average Race Time (seconds)") ,
      hovermode = "closest"
    )
  
plot4

# Part 6 - Convert ggplot to Interactive

## convert plot1 to an interactive plotly
plot5 <- ggplotly(plot1, tooltip = c("x", "y")) %>%
  ## override the original ggplot formatting
  layout(
    title = list(
    text = "Interactive Growth of Olympic Swimming Events<br><sub>
                 Hover for details | Click and drag to zoom!</sub>",
    font = list(size = 10)
  ),
  xaxis = list(title = "Olympic Year"),
  yaxis = list(title = "Number of Events"),
  hovermode = "closest"
  )

plot5