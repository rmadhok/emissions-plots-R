#=======================================================================
# Project: Continuous Emissions Monitoring

# AUthor: Raahil Madhok

# Purpose: Visualize Extent of Emissions Underreporting

# Date Started: March 15, 2016
# Last Update: March 15, 2016
#=======================================================================

# Import Libraries
#---------------------------------
require(ggplot2)
require(dplyr)
require(reshape2)

# Set Directories
#---------------------------------
dir <- "/Users/rmadhok/Dropbox (Personal)/ETS"

# Load TN CEMS and Baseline Data
#---------------------------------
data <- read.csv(paste0(dir, "/data/clean/tn_cems_base_merged.csv"),
                 stringsAsFactors = F)


# Data Preparation
# =====================================================================
# Convert dates to R format
data$date_cems <- as.Date(data$date_s, '%m/%d/%Y')
data$date_base <- as.Date(data$k1_5_samp_date, '%d-%m-%Y')

# Subset Data
keep = c("pm_cems", "pm_base")
data_all <- subset(data, select = keep)
data_month <- subset(data, month_s == month_c, select = keep)
data_day <- subset(data, date_cems == date_base, select = keep)

# Construct Dataset for Plotting
# =====================================================================
data_plot <- data.frame()
dfs <- list(data_all, data_month, data_day)
# Get Means
for(i in dfs){
    means <- lapply(i, mean, na.rm = T)
    data_plot <- rbind(data_plot, means)
}
data_plot$sample <- c("All", "Same Month", "Same Day")

# Reshape for easier plotting
data_plot_long <- reshape(data_plot, 
                          varying = c("pm_cems", "pm_base"),
                          v.names = "pm",
                          timevar = "source",
                          times = c("pm_cems", "pm_base"),
                          direction = "long")


# Plot Data
# =====================================================================
# --------------------------
# AVERAVE CEMS AND BASELINE
#---------------------------
pm_plot <- ggplot(data_plot_long,
                  aes(factor(source), y = pm, fill = factor(source))) +
                  geom_bar(position = "dodge", stat="identity") +
                  geom_text(aes(label=round(pm)), vjust = -0.4) +
                  facet_grid(~sample) +
                  scale_fill_grey(labels = c("Baseline", "CEMS"), 
                                  start = 0.3, end = 0.6) + 
                  ggtitle("Figure 1: CEMS and Baseline PM (mg/m3)") +
                  labs(x=NULL, y="Mean PM Conc. (mg/m3)") +
                  theme(axis.text.x = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.y = element_text(size = 12),
                        axis.title = element_text(face = "bold", size = 12),
                        legend.position = "bottom",
                        legend.title = element_blank(),
                        legend.text = element_text(face = "bold", size = 12),
                        strip.text.x = element_text(face = "bold", size = 15),
                        plot.title = element_text(face = "bold", size = 20))

# Save to Disk
ggsave("tn_cems_base_R.png", pm_plot, path = paste0(dir, "/output"))

#--------------------------------
# Underreporting by Manufacturer
#--------------------------------
# Subset data
data_man <- select(data, manufacturer, pm_base, pm_cems)
data_man <- filter(data_man, !(manufacturer == '.m' | manufacturer == '.s'))

# Get mean
by_man <- group_by(data_man, manufacturer)
mean_man <- summarise(by_man, 
                      mean_cems = mean(pm_cems, na.rm = T),
                      mean_base = mean(pm_base, na.rm = T))

# Melt data for easy plotting
mean_man_long <- melt(mean_man, id = "manufacturer")
mean_man_long$variable <- factor(mean_man_long$variable, c("mean_base", "mean_cems"))
mean_man_long$manufacturer[mean_man_long$manufacturer == "ENDEE OMICRON"] <- "OMICRON"
mean_man_long$manufacturer[mean_man_long$manufacturer == "FORBES MARSHALL"] <- "F. MARSHALL" 

# Plot Data
pm_man <- ggplot(mean_man_long, 
                 aes(x = manufacturer, y = value, 
                     fill = factor(variable), label = round(value),
                     ymax = 180)) + 
                 geom_bar(width = 0.7, position = "dodge", stat = "identity") +
                 geom_text(position=position_dodge(width=0.5), vjust=-0.5) +
                 scale_fill_grey(labels = c("Baseline", "CEMS"), 
                                 start = 0.3, end = 0.6) +
                 ggtitle("Figure 2: CEMS and Baseline PM by Manufacturer (mg/m3)") +
                 labs(x = "Manufacturer", y="Mean PM Conc. (mg/m3)") +
                 theme(axis.text.x = element_text(size = 12, angle = 40, vjust = 0.6),
                       axis.title.x = element_text(size = 14, face = "bold"),
                       axis.text.y = element_text(size = 12),
                       axis.title = element_text(face = "bold", size = 12),
                       legend.title = element_blank(),
                       legend.text = element_text(face = "bold", size = 14),
                       plot.title = element_text(face = "bold", size = 20))
      
# Save to Disk
ggsave("tn_pm_man_R.png", pm_man, path = paste0(dir, "/output"))    
      