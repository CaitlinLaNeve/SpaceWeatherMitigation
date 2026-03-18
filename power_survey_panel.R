# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)

# 1. SETUP & DATA LOAD
filename <- file.path("SpaceWeatherMitigation", "ElectricityResponses.csv")
data_raw <- read.csv(filename)
max_val <- 16 # Setting unified scale for all plots

#Define levels from smallest to largest
employee_levels <- c("<250", "250-1,000", "1,000-5,000", "5,000-10,000", ">10,000")

# 2. Apply this to your master data
data_master$employees <- factor(data_master$employees, levels = employee_levels)

# 3. Double-check your color palette matches these labels EXACTLY
employee_colors <- c(
  "<250"         = "#FDE725FF", # Yellow
  "250-1,000"    = "#5DC863FF", # Green
  "1,000-5,000"  = "#21908CFF", # Teal
  "5,000-10,000" = "#3B528BFF", # Blue
  ">10,000"      = "#440154FF"  # Purple
)
# 3. CLEAN METADATA
data_master <- data_raw %>%
  rename(employees = "survey.section..How.many.employees.does.your.organization.have.") %>%
  mutate(employees = factor(employees, 
                            levels = c("lt250", "250-1000", "1000-5000", "5000-10000", "gt10000"),
                            labels = c("<250", "250-1,000", "1,000-5,000", "5,000-10,000", ">10,000"))) %>%
  filter(!is.na(employees))

total_n <- n_distinct(data_master$response_id)
# This string can be used in your plot annotations or subtitles
total_n_label <- paste0("Total Participants (N = ", total_n, ")")

####################################
### A) LONG-TERM PRE-EVENT
####################################
p1_labels <- c(
  "GIC Impact Modeling", "Voltage Fluctuation Procedures", "Transformer Heating Sims",
  "Extra Cooling Systems", "Negative Sequence Adj.", "Structure Rating (Capacitors)",
  "Protection System Adj.", "Asset Inventory", "Backup Asset Purchase",
  "Transformer Replace Prep", "Spare Transformer Review", "Reactive Demand Analysis",
  "Blackstart Practice"
)

# 2. Prepare the names, excluding the text box
p1_names <- colnames(data_master %>% 
                       select(starts_with("long.pre.event")) %>% 
                       select(-contains("box.below"), -contains("listed")))

p1_map <- setNames(p1_labels, p1_names)

# 3. Pivot ONLY the numeric columns
plot1_data <- data_master %>%
  select(response_id, employees, starts_with("long.pre.event")) %>%
  # THIS LINE REMOVES THE CHARACTER COLUMN SO PIVOT WORKS
  select(-contains("box.below"), -contains("listed")) %>% 
  pivot_longer(cols = starts_with("long.pre.event"), 
               names_to = "Type", 
               values_to = "Val")

# 4. Apply labels and summarize
sum_p1 <- plot1_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(as.numeric(Val), na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p1_map[Type])

labels_p1 <- sum_p1 %>% group_by(Type_Label) %>% summarise(group_sum = sum(Total))

plot1 <- ggplot(sum_p1, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), 
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_text(data = labels_p1, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  scale_y_continuous(limits = c(0, max_val)) +
  theme_minimal(base_size = 16) +
  labs(title = "a) Long-Term Pre-Event Decisions", subtitle = "Decisions taken before event forecast", x = NULL, y = "Responses", fill = "Number of Employees")

####################################
### B) SHORT-TERM PRE-EVENT
####################################
p2_labels <- c(
  "Reduced Transformer Loads", "Line Switching", "Activated GIC Blocking",
  "Remove Vulnerable Assets", "Bring Assets Online", "Increased Reactive Reserves",
  "Brought Generation Online", "Ceased Power Transfers", "Canceled Maint. (Assets)",
  "Canceled Maint. (Teams)", "Staff on Alert", "Canceled Time Off", "Increased Awareness"
)

# 2. Prepare the names, excluding the text box
p2_names <- colnames(data_master %>% 
                       select(starts_with("short.pre.event")) %>% 
                       select(-contains("box.below"), -contains("listed")))

p2_map <- setNames(p2_labels, p2_names)

# 3. Pivot ONLY the numeric columns
plot2_data <- data_master %>%
  select(response_id, employees, starts_with("short.pre.event")) %>%
  # THIS LINE REMOVES THE CHARACTER COLUMN SO PIVOT WORKS
  select(-contains("box.below"), -contains("listed")) %>% 
  pivot_longer(cols = starts_with("short.pre.event"), 
               names_to = "Type", 
               values_to = "Val")

# 4. Apply labels and summarize
sum_p2 <- plot2_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(as.numeric(Val), na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p2_map[Type])

labels_p2 <- sum_p2 %>% group_by(Type_Label) %>% summarise(group_sum = sum(Total))

plot2 <- ggplot(sum_p2, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), 
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_text(data = labels_p2, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  scale_y_continuous(limits = c(0, max_val)) +
  theme_minimal(base_size = 16) +
  labs(title = "b) Short-Term Pre-Event Decisions", subtitle = "Decisions taken after a forecast has been issued", x = NULL, y = "Responses", fill = "Number of Employees")

####################################
### C) DURING-EVENT
####################################
# Using the same labels as Short-Term for consistency
# 1. Define your labels (Make sure there are exactly 13)
p3_labels <- c(
  "Reduced loads on transformers", "Line switching/rearrangement", 
  "Activated GIC blocking", "Network config: Assets offline", 
  "Network config: Assets online", "Increased reactive power", 
  "Brought generation online", "Ceased power transfers", 
  "Canceled Maint. (assets)", "Canceled Maint. (teams)", 
  "Staff on alert", "Canceled time off", "Increased situational awareness"
)

# 2. Prepare the names, excluding the text box
p3_names <- colnames(data_master %>% 
                       select(starts_with("during.event")) %>% 
                       select(-contains("box.below"), -contains("listed")))

p3_map <- setNames(p3_labels, p3_names)

# 3. Pivot ONLY the numeric columns
plot3_data <- data_master %>%
  select(response_id, employees, starts_with("during.event")) %>%
  # THIS LINE REMOVES THE CHARACTER COLUMN SO PIVOT WORKS
  select(-contains("box.below"), -contains("listed")) %>% 
  pivot_longer(cols = starts_with("during.event"), 
               names_to = "Type", 
               values_to = "Val")

# 4. Apply labels and summarize
sum_p3 <- plot3_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(as.numeric(Val), na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p3_map[Type])

labels_p3 <- sum_p3 %>% group_by(Type_Label) %>% summarise(group_sum = sum(Total))

plot3 <- ggplot(sum_p3, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), 
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_text(data = labels_p3, aes(x = Type_Label, y = group_sum, label = group_sum), hjust = -0.5) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  scale_y_continuous(limits = c(0, max_val)) +
  theme_minimal(base_size = 16) +
  labs(title = "c) During-Event Decisions", subtitle = "Decisions taken after event commencement", x = NULL, y = "Responses", fill = "Number of Employees")

####################################
### D) POST-EVENT
####################################
# 1. The Master List (Exact order for the plot)
p4_labels_ordered <- c(
  "Shared Experiences", "Shared Data", "Unusual Condition Invest.", 
  "New Modeling/Sims", "Revised Prep Plans", "Gas Level Assessment", 
  "EHV Transformer Inspection", "Ordered New Apparatus", 
  "Replace Assets (<$10k)", "Replace Assets ($10-100k)", 
  "Replace Assets ($100k-$1m)", "Replace Assets (>$1m)"
)

# 2. Prepare names and create the Map using the NEW ordered list
p4_names <- colnames(data_master %>% 
                       select(starts_with("post.event")) %>% 
                       select(-contains("box.below"), -contains("listed")))

p4_map <- setNames(p4_labels_ordered, p4_names) # <--- USE ORDERED LIST HERE

# 3. Pivot (Stayed the same)
plot4_data <- data_master %>%
  select(response_id, employees, starts_with("post.event")) %>%
  select(-contains("box.below"), -contains("listed")) %>% 
  pivot_longer(cols = starts_with("post.event"), 
               names_to = "Type", 
               values_to = "Val")

# 4. Summarize and Fix Factors
sum_p4 <- plot4_data %>%
  group_by(Type, employees) %>%
  summarise(Total = sum(as.numeric(Val), na.rm = TRUE), .groups = "drop") %>%
  mutate(Type_Label = p4_map[Type])

# FORCE levels: This fixes the "jumbled" or "jumping" labels
#sum_p4$Type_Label <- factor(sum_p4$Type_Label, levels = rev(p4_labels_ordered))

# IMPORTANT: Re-calculate text labels so they align with the new factor order
labels_p4 <- sum_p4 %>% group_by(Type_Label) %>% summarise(group_sum = sum(Total))

# 5. Plot (No reorder() needed in aes anymore)
plot4 <- ggplot(sum_p4, aes(x = reorder(Type_Label, Total), y = Total)) +
  geom_bar(aes(fill = employees), 
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_text(data = labels_p4, aes(x = Type_Label, y = group_sum, label = group_sum), 
            hjust = -0.5, size = 4) +
  coord_flip() +
  scale_fill_manual(values = employee_colors) +
  scale_y_continuous(limits = c(0, max_val)) +
  theme_minimal(base_size = 16) +
  #theme(plot.title = element_text(face = "bold")) +
  labs(title = "d) Post-Event Decisions", subtitle = "Decisions taken once event is over", 
       x = NULL, y = "Responses", fill = "Number of Employees") +
  guides(fill = guide_legend(reverse = FALSE))

# FINAL ASSEMBLY


final_plot <- ggarrange(plot1, plot2, plot3, plot4, 
                        ncol = 2, nrow = 2, 
                        common.legend = TRUE, legend = "bottom")

final_plot <- annotate_figure(final_plot,
                              top = text_grob("Power Sector Online Survey Responses", 
                                              color = "black", face = "bold", size = 22),
                              bottom = text_grob(paste0("Total Unique Respondents (N = ", total_n, ")"), 
                                                 color = "black", face = "italic", size = 12, hjust = 1, x = 1))

# Define the path
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
folder_path <- file.path(dir, "figures")
# Create folder if it doesn't exist
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# List of your plots
plot_list <- list("plot1.png" = plot1, "plot2.png" = plot2, 
                  "plot3.png" = plot3, "plot4.png" = plot4)

# Loop to save each individually
for (filename in names(plot_list)) {
  ggsave(filename = file.path(folder_path, filename), 
         plot = plot_list[[filename]], 
         width = 10, height = 7, dpi = 300, bg = "white")
}

# Save the final combined panel
ggsave(filename = file.path(folder_path, "Full_Panel_Power.png"), 
       plot = final_plot, 
       width = 16, height = 14, dpi = 300, bg = "white")