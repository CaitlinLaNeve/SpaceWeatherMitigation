# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(tibble)

# File Paths
# File Paths
filename <- file.path("SpaceWeatherMitigation", "SatelliteResponse.csv")
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
folder_figs <- file.path(dir, "figures")
dir.create(folder_figs, showWarnings = FALSE)

# 1. LOAD DATA ONCE
data_raw <- read.csv(filename)

# 2. CLEAN METADATA ONCE (Renaming common columns)
data_master <- data_raw %>% 
  rename(
    employees = "survey.section..How.many.employees.does.your.company.have.",
    customers = "survey.section..How.many.customers.does.your.organization.have.",
    satellites = "survey.section..How.many.primary.satellites.do.you.operate.",
    country = "survey.section..Which.country.region.is.your.HQ.located."
  ) %>%
  mutate(employees = factor(employees, 
                            levels = c("1-24", "25-99", "100-199", "200-499", "500+", ""),
                            labels = c("1-24", "25-99", "100-199", "200-499", ">500", "Not Disclosed")))

# 3. GET RESPONDENT COUNT
total_n <- n_distinct(data_master$response_id) 

# Define fixed color palette for employee bins
employee_colors <- c(
  "1-24"           = "#FDE725FF", # Yellow
  "25-99"          = "#5DC863FF", # Green
  "100-199"        = "#21908CFF", # Teal
  "200-499"        = "#3B528BFF", # Blue
  ">500"           = "#440154FF", # Purple
  "Not Disclosed"  = "grey80"     # Grey
)

####################################
### A) Long-Term Pre-Event
####################################

# 1. Define the Mapping (CSV Name = Clean Label)
# Check your console for the exact column names using: names(data_master)
p1_map <- c(
  "long.pre.event.section..Modeling.and.simulation.of.system.design.to.quantify.expected.radiation.exposure." = "Modeling and \nSimulation",
  "long.pre.event.section..Modeling.and.simulation.of.orbits.to.quantify.expected.radiation.exposure." = "Orbital Modeling",
  "long.pre.event.section..Evaluation.of.adequate.satellite.shielding.thickness.and.ongoing.quality.levels." = "Shielding Evaluation",
  "long.pre.event.section..Developed.redundancy.for.onboard.systems." = "Onboard System \nRedundancy",
  "long.pre.event.section..Monitored.orbital.trajectory..anticipate.largest.cumulative.radiation.exposure." = "Trajectory Monitoring",
  "long.pre.event.section..Implemented.power.supply.redundancy.for.ground.based.systems..e.g...Earth.stations.." = "Ground Power \nRedundancy",
  "long.pre.event.section..Implemented.optical.fiber.link.redundancy.for.ground.based.systems..e.g...Earth.stations.." = "Ground Fiber \nRedundancy"
)

# 2. Slice and Pivot
plot1_data <- data_master %>% 
  select(response_id, employees, starts_with("long.pre.event")) %>%
  select(-contains("Please.add")) %>%
  pivot_longer(cols = starts_with("long.pre.event"), 
               names_to = "Type", values_to = "Val")

# 3. Apply the labels using the Map
sum_p1 <- plot1_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(Val, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p1_map[Type]) # This matches the name to the label directly

# 4. Summarize for the end-of-bar totals
labels_p1 <- sum_p1 %>%
  group_by(Type_Label) %>%
  summarise(group_sum = sum(Total))

# 5. Plot (using the clean labels for the axis)
plot1 <- ggplot(sum_p1, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), stat = "identity") +
  geom_text(data = labels_p1, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  ylim(0, total_n) +
  theme_minimal(base_size = 14) +
  labs(title = "a) Long-Term Pre-Event", subtitle = "Decisions taken before event forecast", x = NULL, y = "Responses", fill = "Number of Employees")
####################################
### B) Short-Term Pre-Event
####################################

# Slice the data
plot2_data <- data_master %>% 
  select(response_id, employees, starts_with("short.pre.event")) %>%
  select(-contains("Please.add")) %>%
  pivot_longer(cols = starts_with("short.pre.event"), 
               names_to = "Type", values_to = "Val")

# Create Labels (Must match the number of columns in this section)
p2_labels <- c(
  "Moved Satellites", "Safe Mode Entry", "Notified Customers",
  "Postponed Launch", "Canceled Network Maint.", "Maint. Team Readiness",
  "Staff on Alert", "Canceled Time Off", "Increased Staff Awareness"
)
# 2. Map to CSV columns
p2_names <- colnames(data_master %>% select(starts_with("short.pre.event")) %>% select(-contains("Please.add")))
p2_map <- setNames(p2_labels, p2_names)

# 3. Apply to data
sum_p2 <- plot2_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(Val, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p2_map[Type])

labels_p2 <- sum_p2 %>%
  group_by(Type_Label) %>%
  summarise(group_sum = sum(Total))


plot2 <- ggplot(sum_p2, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), stat = "identity") +
  geom_text(data = labels_p2, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  ylim(0, total_n) +
  theme_minimal(base_size = 14) +
  labs(title = "b) Short-Term Pre-Event", subtitle = "Decisions taken after a forecast has been issued", x = NULL, y = "Responses", fill = "Number of Employees")

####################################
### C) During-Event
####################################

# Slice the data
plot3_data <- data_master %>% 
  select(response_id, employees, starts_with("during.event")) %>%
  select(-contains("Please.add")) %>%
  pivot_longer(cols = starts_with("during.event"), 
               names_to = "Type", values_to = "Val")

# Create Labels (Must match the number of columns in this section)
p3_labels <- c(
  "Orbit Adjustments", "Satellite Safe Mode", "Customer Notifications",
  "Launch Delays", "Network Maint. Pause", "Staff Allocation Maint.",
  "Teams on Alert", "Cancel Time Off", "Situational Awareness"
)

# 2. Map to CSV columns
p3_names <- colnames(data_master %>% select(starts_with("during.event")) %>% select(-contains("Please.add")))
p3_map <- setNames(p3_labels, p3_names)

# 3. Apply to data
sum_p3 <- plot3_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(Val, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p3_map[Type])

labels_p3 <- sum_p3 %>%
  group_by(Type_Label) %>%
  summarise(group_sum = sum(Total))

# Plot

plot3 <- ggplot(sum_p3, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), stat = "identity") +
  geom_text(data = labels_p3, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  ylim(0, total_n) +
  theme_minimal(base_size = 14) +
  labs(title = "c) During Event Decisions", subtitle = "Decisions taken after event commencment", x = NULL, y = "Responses", fill = "Number of Employees")


####################################
### C) During-Event
####################################

# Slice the data
plot4_data <- data_master %>% 
  select(response_id, employees, starts_with("post.event")) %>%
  select(-contains("Please.add")) %>%
  pivot_longer(cols = starts_with("post.event"), 
               names_to = "Type", values_to = "Val")

# Create Labels (Must match the number of columns in this section)
p4_labels <- c(
  "Joule Heating Assessment", "Ionizing Dose Est.", "Displacement Damage Est.",
  "Replace Assets (<$1m)", "Replace Assets ($1-10m)", "Replace Assets ($10-100m)", "Replace Assets (>$100m)",
  "Order Assets (<$1m)", "Order Assets ($1-10m)", "Order Assets ($10-100m)", "Order Assets (>$100m)",
  "Share Data (Operators)", "Share Experience (Operators)", "Revised Prep Plans", "New Modeling/Sims"
)

# 2. Map to CSV columns
p4_names <- colnames(data_master %>% select(starts_with("post.event")) %>% select(-contains("Please.add")))
p4_map <- setNames(p4_labels, p4_names)

# 3. Apply to data
sum_p4 <- plot4_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(Val, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p4_map[Type])

labels_p4 <- sum_p4 %>%
  group_by(Type_Label) %>%
  summarise(group_sum = sum(Total))

# Plot
plot4 <- ggplot(sum_p4, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), stat = "identity") +
  geom_text(data = labels_p4, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  ylim(0, total_n) +
  theme_minimal(base_size = 14) +
  labs(title = "d) Post-Event Decisions", subtitle = "Decisions taken once the event is over", x = NULL, y = "Responses", fill = "Number of Employees")



total_n <- n_distinct(data_master$response_id)

panel <- ggarrange(plot1, plot2, plot3, plot4, 
                   ncol = 2, nrow = 2, 
                   common.legend = TRUE, 
                   legend = 'bottom')

# Add the title AND the respondent count
final_plot <- annotate_figure(panel,
                              top = text_grob("Satellite Sector Online Survey Responses", 
                                              color = "black", face = "bold", size = 22),
                              bottom = text_grob(paste0("Total Unique Respondents (N = ", total_n, ")"), 
                                                 color = "black", face = "italic", size = 12, hjust = 1, x = 1))
ggsave(
  'satellite_panel.png',
  plot = final_plot,
  device = "png",
  path=folder_figs, 
  units = c("in"),
  width = 14,
  height = 12,
  bg="white"
)

ggsave(
  'satellite_1_long_term.png',
  plot = plot1,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_2_short_term.png',
  plot = plot2,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_3_during_event.png',
  plot = plot3,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_4_post_event.png',
  plot = plot4,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

