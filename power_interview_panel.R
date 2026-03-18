# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)

# File Paths
decisions_file <- file.path("SpaceWeatherMitigation", "interview_sat.csv")
stats_file <- file.path("SpaceWeatherMitigation", "interview_stats.csv")
# Create figures folder
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
folder_figs <- file.path(dir, "figures")
dir.create(folder_figs, showWarnings = FALSE)

# Load data
decisions <- read.csv(decisions_file, stringsAsFactors = FALSE)
stats <- read.csv(stats_file, stringsAsFactors = FALSE)

# Filter for Power sector only
power_decisions <- decisions %>% filter(sector == "Power")

# Process the id_list column to expand rows for each ID
power_long <- power_decisions %>%
  mutate(id_list = str_remove_all(id_list, '"')) %>%  # Remove quotes
  separate_rows(id_list, sep = ",") %>%  # Split comma-separated IDs
  mutate(id_list = str_trim(id_list)) %>%  # Trim whitespace
  rename(ID = id_list)

# Merge with employee statistics
power_data <- power_long %>%
  left_join(stats, by = "ID")

# Create employee count bins for Power (matching original code)
power_data <- power_data %>%
  mutate(employee_bin = case_when(
    employeecount < 250 ~ "<250",
    employeecount >= 250 & employeecount < 1000 ~ "250-1,000",
    employeecount >= 1000 & employeecount < 5000 ~ "1,000-5,000",
    employeecount >= 5000 & employeecount < 10000 ~ "5,000-10,000",
    employeecount >= 10000 ~ ">10,000",
    TRUE ~ "Unknown"
  ))

# Set factor levels for employee bins
power_data$employee_bin <- factor(power_data$employee_bin,
                                  levels = c("<250", "250-1,000", "1,000-5,000", "5,000-10,000", ">10,000", "Unknown"))

# Function to wrap text
wrap_text <- function(text, width = 35) {
  sapply(text, function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  }, USE.NAMES = FALSE)
}

# Count responses for each decision by employee bin
max_value <- 15  # Adjusted for power data

# Define fixed color palette for employee bins
employee_colors <- c(
  "<250" = "#FDE725FF",           # Yellow
  "250-1,000" = "#5DC863FF",      # Green
  "1,000-5,000" = "#21908CFF",    # Teal
  "5,000-10,000" = "#3B528BFF",   # Blue
  ">10,000" = "#440154FF"         # Purple
)

##################
### Pre-event Long-term
##################
data_subset <- power_data %>%
  filter(event_phase == "Pre-event Long-term") %>%
  select(event_phase, decision_text, employee_bin)

# Summarise the data
summarised_data <- data_subset %>%
  group_by(decision_text, employee_bin) %>%
  summarise(Response_Value = n(), .groups = "drop")

# Wrap decision text
summarised_data <- summarised_data %>%
  mutate(decision_wrapped = wrap_text(decision_text, width = 30))

# Get total sum per decision
labels <- summarised_data %>%
  group_by(decision_wrapped) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# Ensure all combinations exist
summarised_data <- summarised_data %>%
  left_join(labels, by = "decision_wrapped") %>%
  mutate(decision_wrapped = reorder(decision_wrapped, group_sum))

# Reverse employee bin order for stacking
original_levels <- c("<250", "250-1,000", "1,000-5,000", "5,000-10,000", ">10,000")
summarised_data$employee_bin <- factor(summarised_data$employee_bin, levels = rev(original_levels))
summarised_data <- summarised_data[complete.cases(summarised_data),]

# Plot
plot1 <- ggplot(summarised_data, aes(x = decision_wrapped, y = Response_Value)) +
  geom_bar(aes(fill = employee_bin), stat = "identity", width = 0.8) + 
  geom_text(data = labels, aes(x = decision_wrapped, y = group_sum, label = group_sum), 
            vjust = 0.5, hjust = -0.3, size = 3.5) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    # INCREASED Y-AXIS TEXT SIZE HERE (from 9 to 12)
    axis.text.y = element_text(size = 16),
    
    # ADDED/INCREASED TITLE SIZES HERE
    plot.title = element_text(size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(5, 15, 5, 5)
  ) +
  labs(
    title = "a) Long-Term Pre-Event Decisions",
    subtitle = "Decisions taken before event forecast",
    x = NULL, y = "Responses",
    fill = "Number of Employees"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 9)) +
  scale_fill_manual(values = employee_colors, drop = FALSE)

##################
### Pre-event Short-term
##################
data_subset <- power_data %>%
  filter(event_phase == "Pre-event Short-term") %>%
  select(event_phase, decision_text, employee_bin)

summarised_data <- data_subset %>%
  group_by(decision_text, employee_bin) %>%
  summarise(Response_Value = n(), .groups = "drop")

summarised_data <- summarised_data %>%
  mutate(decision_wrapped = wrap_text(decision_text, width = 30))

labels <- summarised_data %>%
  group_by(decision_wrapped) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  left_join(labels, by = "decision_wrapped") %>%
  mutate(decision_wrapped = reorder(decision_wrapped, group_sum))

summarised_data$employee_bin <- factor(summarised_data$employee_bin, levels = rev(original_levels))
summarised_data <- summarised_data[complete.cases(summarised_data),]

plot2 <- ggplot(summarised_data, aes(x = decision_wrapped, y = Response_Value)) +
  geom_bar(aes(fill = employee_bin), stat = "identity", width = 0.8) + 
  geom_text(data = labels, aes(x = decision_wrapped, y = group_sum, label = group_sum), 
            vjust = 0.5, hjust = -0.3, size = 3.5) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    # INCREASED Y-AXIS TEXT SIZE HERE (from 9 to 12)
    axis.text.y = element_text(size = 16),
    
    # ADDED/INCREASED TITLE SIZES HERE
    plot.title = element_text(size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(5, 15, 5, 5)
  ) +
  labs(
    title = "b) Short-Term Pre-Event Decisions",
    subtitle = "Decisions taken after a forecast has been issued",
    x = NULL, y = "Responses",
    fill = "Employees"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 9)) +
  scale_fill_manual(values = employee_colors, drop = FALSE)

##################
### During Event
##################
data_subset <- power_data %>%
  filter(event_phase == "During Event") %>%
  select(event_phase, decision_text, employee_bin)

summarised_data <- data_subset %>%
  group_by(decision_text, employee_bin) %>%
  summarise(Response_Value = n(), .groups = "drop")

summarised_data <- summarised_data %>%
  mutate(decision_wrapped = wrap_text(decision_text, width = 30))

labels <- summarised_data %>%
  group_by(decision_wrapped) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  left_join(labels, by = "decision_wrapped") %>%
  mutate(decision_wrapped = reorder(decision_wrapped, group_sum))

summarised_data$employee_bin <- factor(summarised_data$employee_bin, levels = rev(original_levels))
summarised_data <- summarised_data[complete.cases(summarised_data),]

plot3 <- ggplot(summarised_data, aes(x = decision_wrapped, y = Response_Value)) +
  geom_bar(aes(fill = employee_bin), stat = "identity", width = 0.8) + 
  geom_text(data = labels, aes(x = decision_wrapped, y = group_sum, label = group_sum), 
            vjust = 0.5, hjust = -0.3, size = 3.5) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    # INCREASED Y-AXIS TEXT SIZE HERE (from 9 to 12)
    axis.text.y = element_text(size = 16),
    
    # ADDED/INCREASED TITLE SIZES HERE
    plot.title = element_text(size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(5, 15, 5, 5)
  ) +
  labs(
    title = "c) During-Event Decisions",
    subtitle = "Decisions taken after event commencement",
    x = NULL, y = "Responses",
    fill = "Employees"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 9)) +
  scale_fill_manual(values = employee_colors, drop = FALSE)

##################
### Post-event
##################
data_subset <- power_data %>%
  filter(event_phase == "Post-event") %>%
  select(event_phase, decision_text, employee_bin)

summarised_data <- data_subset %>%
  group_by(decision_text, employee_bin) %>%
  summarise(Response_Value = n(), .groups = "drop")

summarised_data <- summarised_data %>%
  mutate(decision_wrapped = wrap_text(decision_text, width = 30))

labels <- summarised_data %>%
  group_by(decision_wrapped) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  left_join(labels, by = "decision_wrapped") %>%
  mutate(decision_wrapped = reorder(decision_wrapped, group_sum))

summarised_data$employee_bin <- factor(summarised_data$employee_bin, levels = rev(original_levels))
summarised_data <- summarised_data[complete.cases(summarised_data),]

plot4 <- ggplot(summarised_data, aes(x = decision_wrapped, y = Response_Value)) +
  geom_bar(aes(fill = employee_bin), stat = "identity", width = 0.8) + 
  geom_text(data = labels, aes(x = decision_wrapped, y = group_sum, label = group_sum), 
            vjust = 0.5, hjust = -0.3, size = 3.5) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    # INCREASED Y-AXIS TEXT SIZE HERE (from 9 to 12)
    axis.text.y = element_text(size = 16),
    
    # ADDED/INCREASED TITLE SIZES HERE
    plot.title = element_text(size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(5, 15, 5, 5)
  ) +
  labs(
    title = "d) Post-Event Decisions",
    subtitle = "Decisions taken once the event is over",
    x = NULL, y = "Responses",
    fill = "Employees"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 9)) +
  scale_fill_manual(values = employee_colors, drop = FALSE)

##################
### Adverse Impact
##################
data_subset <- power_data %>%
  filter(event_phase == "Adverse Impact") %>%
  select(event_phase, decision_text, employee_bin)

summarised_data <- data_subset %>%
  group_by(decision_text, employee_bin) %>%
  summarise(Response_Value = n(), .groups = "drop")

summarised_data <- summarised_data %>%
  mutate(decision_wrapped = wrap_text(decision_text, width = 30))

labels <- summarised_data %>%
  group_by(decision_wrapped) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  left_join(labels, by = "decision_wrapped") %>%
  mutate(decision_wrapped = reorder(decision_wrapped, group_sum))

summarised_data$employee_bin <- factor(summarised_data$employee_bin, levels = rev(original_levels))
summarised_data <- summarised_data[complete.cases(summarised_data),]

plot5 <- ggplot(summarised_data, aes(x = decision_wrapped, y = Response_Value)) +
  geom_bar(aes(fill = employee_bin), stat = "identity", width = 0.8) + 
  geom_text(data = labels, aes(x = decision_wrapped, y = group_sum, label = group_sum), 
            vjust = 0.5, hjust = -0.3, size = 3.5) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    # INCREASED Y-AXIS TEXT SIZE HERE (from 9 to 12)
    axis.text.y = element_text(size = 16),
    
    # ADDED/INCREASED TITLE SIZES HERE
    plot.title = element_text(size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(5, 15, 5, 5)
  ) +
  labs(
    title = "E) Impacts over Current Solar Maximum",
    subtitle = "Impacts experienced during the current solar maximum.",
    x = NULL, y = "Responses",
    fill = "Number of Employees"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 9)) +
  scale_fill_manual(values = employee_colors, drop = FALSE)

##################
### Create Panel
##################
total_n <- n_distinct(data$response_id)

panel <- ggarrange(plot1, plot2, plot3, plot4, 
                   ncol = 2, nrow = 2, 
                   common.legend = TRUE, 
                   legend = 'bottom')

# Add the title AND the respondent count
final_plot <- annotate_figure(panel,
                              top = text_grob("Power Sector Interview Responses", 
                                              color = "black", face = "bold", size = 22),
                              bottom = text_grob(paste0("Total Unique Respondents (N = ", 11, ")"), 
                                                 color = "black", face = "italic", size = 12, hjust = 1, x = 1))
# Save plots
ggsave(
  'power_panel_withn.png',
  plot = final_plot,
  device = "png",
  path = folder_figs, 
  units = c("in"),
  width = 15,
  height = 12,
  bg = "white"
)

ggsave(
  'power_1_long_term.png',
  plot = plot1,
  device = "png",
  path = folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg = "white"
)

ggsave(
  'power_2_short_term.png',
  plot = plot2,
  device = "png",
  path = folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg = "white"
)

ggsave(
  'power_3_during_event.png',
  plot = plot3,
  device = "png",
  path = folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg = "white"
)

ggsave(
  'power_4_post_event.png',
  plot = plot4,
  device = "png",
  path = folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg = "white"
)

ggsave(
  'power_5_adverse_impact.png',
  plot = plot5,
  device = "png",
  path = folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg = "white"
)

print("Power plots completed successfully!")