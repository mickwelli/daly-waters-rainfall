create_cum_plot <- function(df) {

  # Perform the math inside the function
  plot_ready_data <- df %>%
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

  # Create the plot
  p <- ggplot(plot_ready_data, aes(x = Day_of_FY, y = Cum_Rain, group = FY_Label)) +
    geom_line_interactive(aes(tooltip = my_tooltip, data_id = FY_Label),
                          alpha = 0.4, color = "steelblue") +
    theme_minimal() +
    labs(
      title = "Cumulative Rainfall by Financial Year",
      x = "Day of Financial Year (Starting July 1st)",
      y = "Cumulative Rainfall (mm)")

  return(girafe(ggobj = p))
}

create_bar_plot <- function(df) {

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
         width_svg = 10,  # Relative internal width
         height_svg = 4,  # Relative internal height
         options = list(
           # This is the key for "all space available":
           opts_sizing(rescale = TRUE, width = 1),
           opts_hover(css = "fill:orange;cursor:pointer;"),
           opts_toolbar(saveaspng = FALSE)
         ))
}

create_anomaly_plot <- function(df) {
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
    labs(title = "Standard Rainfall Anomaly", x = "Financial Year", y = "Standard Deviations") +
    theme(legend.position = "none")

  girafe(ggobj = p,
         width_svg = 10,  # Relative internal width
         height_svg = 4,  # Relative internal height
         options = list(
           opts_sizing(rescale = TRUE, width = 1),
           opts_hover(css = "fill:orange;cursor:pointer;"),
           opts_toolbar(saveaspng = FALSE)
         ))
}

create_rainy_days_table <- function(df) {
  require(tidyr)
  require(dplyr)
  require(lubridate)
  require(reactable)

  # 1. Prepare Month Names in FY order
  months_fy <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

  # 2. Process data
  table_data <- df %>%
    mutate(
      Rain = as.numeric(Rain),
      m = month(Date),
      y = year(Date),
      FY_Year = if_else(m >= 7, y, y - 1),
      FY_Label = paste0(FY_Year, "-", substr(FY_Year + 1, 3, 4)),
      Month_Name = month.abb[m]
    ) %>%
    group_by(FY_Label, Month_Name) %>%
    summarise(Rainy_Days = sum(Rain > 0, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Month_Name, values_from = Rainy_Days, values_fill = 0)

  # Ensure all months exist even if they have no data in the filtered range
  for(m in months_fy) {
    if(!(m %in% names(table_data))) table_data[[m]] <- 0
  }

  # 3. Calculate Total
  table_data <- table_data %>%
    mutate(Total = rowSums(select(., all_of(months_fy)), na.rm = TRUE)) %>%
    select(FY_Label, all_of(months_fy), Total)

  # 4. Build the reactable
  reactable(
    table_data,
    compact = TRUE,
    sortable = TRUE,
    resizable = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    defaultSorted = "Total",
    defaultSortOrder = "desc",
    # Highlight the active sort column header
    theme = reactableTheme(
      headerStyle = list(
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#dfe9f3", color = "#004a99")
      )
    ),
    defaultColDef = colDef(
      align = "center",
      minWidth = 55,
      # Add subtle font color logic
      style = function(value, index, name) {
        if (name == "FY_Label") return(list(fontWeight = "bold", borderRight = "1px solid #eee"))
        if (name == "Total") return(list(background = "#f0f5ff", fontWeight = "bold"))
        if (is.numeric(value) && value == 0) return(list(color = "#cbd5e0")) # Faded zeros
        return(list(color = "#2d3748"))
      }
    ),
    columns = list(
      FY_Label = colDef(name = "Year", minWidth = 100, align = "left"),
      Total = colDef(name = "Total", minWidth = 80)
    )
  )
}


