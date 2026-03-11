library(tidyverse)
library(ggiraph)
library(reactable)

df <- readRDS('data/Rain_DW.rds')

# 1. Prepare the data
plot_data <- df %>%
  # Ensure we have a column for Financial Year (FY)
  mutate(
    Rain = as.numeric(Rain),
    FY_Year = if_else(month(Date) >= 7, year(Date), year(Date) - 1),
    FY_Label = paste0(FY_Year, "-", substr(FY_Year + 1, 3, 4))
  ) %>%
  group_by(FY_Label) %>%
  arrange(Date) %>%
  mutate(
    # Force both to be simple 'Date' class objects
    Start_of_FY = as.Date(paste0(FY_Year, "-07-01")),
    Date = as.Date(Date),

    # Now the subtraction will work perfectly
    Day_of_FY = as.numeric(Date - Start_of_FY) + 1,
    # Calculate cumulative rainfall
    Cum_Rain = cumsum(coalesce(Rain, 0)),
    # Create the tooltip string
    Max_Rain = max(Cum_Rain),
    my_tooltip = paste0("FY: ", FY_Label, "\nTotal: ", round(Max_Rain, 1), "mm")
  ) %>%
  ungroup()

# 2. Build the interactive ggplot
p <- ggplot(plot_data, aes(x = Day_of_FY, y = Cum_Rain, group = FY_Label)) +
  # We use geom_line_interactive for the magic
  geom_line_interactive(aes(tooltip = my_tooltip, data_id = FY_Label),
                        alpha = 0.4, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Cumulative Rainfall by Financial Year",
    x = "Day of Financial Year (Starting July 1st)",
    y = "Cumulative Rainfall (mm)"
  )

# 3. Render the interactive plot
girafe(ggobj = p,
       options = list(
         opts_hover(css = "stroke:orange; stroke-width:3px; opacity:1;"),
         opts_tooltip(css = "background-color:white; color:black; padding:5px; border-radius:5px;")
       ))


# Function for Annual Totals Bar Plot
create_bar_plot <- function(daily_df) {
  summary_data <- daily_df %>%
    group_by(FY_Year) %>%
    summarise(Total = sum(Rain, na.rm = TRUE)) %>%
    mutate(tooltip = paste0("FY: ", FY_Year, "\nTotal: ", round(Total, 1), "mm"))

  p <- ggplot(summary_data, aes(x = FY_Year, y = Total)) +
    geom_bar_interactive(aes(tooltip = tooltip, data_id = FY_Year),
                         stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Total Rainfall by Financial Year", x = "Year", y = "Rain (mm)")

  girafe(ggobj = p)
}
###
# Function for Standard Anomaly Plot

# 1. Calculate annual totals
annuals <- df %>%
  mutate(
    Rain = as.numeric(Rain),
    FY_Year = if_else(month(Date) >= 7, year(Date), year(Date) - 1),
    FY_Label = paste0(FY_Year, "-", substr(FY_Year + 1, 3, 4))
  ) %>%
  group_by(FY_Year, FY_Label) %>%
  summarise(Total = sum(Rain, na.rm = TRUE))

# 2. Calculate long-term stats (Climate Normal)
mu <- mean(annuals$Total)
sigma <- sd(annuals$Total)

# 3. Calculate Anomaly
annuals <- annuals %>%
  mutate(
    Anomaly = (Total - mu) / sigma,
    Direction = if_else(Anomaly >= 0, "Above", "Below"),
    tooltip = paste0("FY: ", FY_Label, "\nStd Anomaly: ", round(Anomaly, 2))
  )

p <- ggplot(annuals, aes(x = FY_Year, y = Anomaly, fill = Direction)) +
  geom_bar_interactive(aes(tooltip = tooltip, data_id = FY_Year), stat = "identity") +
  scale_fill_manual(values = c("Above" = "#2166ac", "Below" = "#b2182b")) +
  theme_minimal() +
  labs(title = "Standard Rainfall Anomaly", x = "Year", y = "Standard Deviations") +
  theme(legend.position = "none")

girafe(ggobj = p)

#### Bar plot
summary_data <- df %>%
  mutate(
    Rain = as.numeric(Rain),
    FY_Year = if_else(month(Date) >= 7, year(Date), year(Date) - 1),
    FY_Label = paste0(FY_Year, "-", substr(FY_Year + 1, 3, 4))
  ) %>%
  group_by(FY_Year, FY_Label) %>%
  summarise(Total_Rain = sum(Rain, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Professional tooltip with bold text and units
    tooltip = paste0("<b>", FY_Label, "</b><br>Total: ", round(Total_Rain, 1), " mm")
  )

# 2. Build the Plot
p <- ggplot(summary_data, aes(x = FY_Year, y = Total_Rain)) +
  # Use geom_bar_interactive for the tooltip and hover effects
  geom_bar_interactive(
    aes(tooltip = tooltip, data_id = FY_Year),
    stat = "identity",
    fill = "#4575b4"
  ) +

  # Matching the 10-year breaks of the anomaly plot
  scale_x_continuous(
    breaks = seq(1880, 2030, by = 10),
    expand = c(0.01, 0)
  ) +

  theme_minimal() +
  labs(
    title = "Annual Rainfall Totals",
    x = "Financial Year",
    y = "Total Rainfall (mm)"
  ) +
  # Optional: Add a subtle horizontal line for the long-term median
  geom_hline(yintercept = median(summary_data$Total_Rain),
             linetype = "dashed", color = "grey40", alpha = 0.7) +
  annotate("label",
           x = max(summary_data$FY_Year), # Places it at the right edge
           y = median(summary_data$Total_Rain),
           label = paste("Median:", round(median(summary_data$Total_Rain), 0), "mm"),
           vjust = -1,    # Moves text slightly above the line
           hjust = 1,     # Aligns text to the right
           color = "grey30",
           fontface = "italic",
           size = 3.5)

# 3. Render with ggiraph options
girafe(ggobj = p,
       options = list(
         opts_hover(css = "fill:orange;cursor:pointer;"),
         opts_tooltip(css = "font-family:sans-serif;background-color:white;padding:5px;border-radius:5px;")
       ))

######

# 1. Summarise the count of days > 0mm
table_data <- df %>%
  mutate(
    Rain = as.numeric(Rain),
    FY_Year = if_else(month(Date) >= 7, year(Date), year(Date) - 1),
    FY_Label = paste0(FY_Year, "-", substr(FY_Year + 1, 3, 4))
  ) %>%
  group_by(FY_Label) %>%
  summarise(
    Rainy_Days = sum(Rain > 0, na.rm = TRUE),
    Total_Rain = sum(Rain, na.rm = TRUE)
  ) %>%
  arrange(desc(Rainy_Days)) # Default to highest count first

# 2. Build the reactable
reactable(
  table_data,
  compact = TRUE,
  searchable = FALSE,
  resizable = TRUE,
  defaultSorted = "Rainy_Days",
  defaultSortOrder = "desc",
  columns = list(
    FY_Label = colDef(name = "Financial Year", minWidth = 150),
    Rainy_Days = colDef(
      name = "Rainy Days (>0mm)",
      align = "center",
      # Add a color scale (light blue to deep blue)
      style = function(value) {
        normalized <- (value - min(table_data$Rainy_Days)) / (max(table_data$Rainy_Days) - min(table_data$Rainy_Days))
        color <- rgb(colorRamp(c("#e7f1ff", "#004a99"))(normalized), maxColorValue = 255)
        list(background = color, color = if_else(normalized > 0.5, "white", "black"))
      }
    ),
    Total_Rain = colDef(
      name = "Total Rain (mm)",
      format = colFormat(digits = 1),
      align = "right"
    )
  ),
  bordered = TRUE,
  highlight = TRUE
)
