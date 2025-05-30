# ------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Title:        Dose-Response Script
# Description:  This script was written to generate dose-response curves and calculate C50 values (in association with the published protocol, linked below)
# Author:       Michaela E. Everly
# ORCID:        https://orcid.org/0009-0003-5689-0700
# Protocol DOI: 
# Version:      1.0
# Date:         May 20, 2025
# License:      MIT License
#               This script is licensed under the MIT License.
#               See https://opensource.org/licenses/MIT


## General information ---------------------------------------------------------------------------------------------------------------------------------------------
# INFORMATION ABOUT COMMENTS (i.e., anything starting with "#")
  # Comments are ignored when running the script & used below to explain what the code does
  # In some cases, #'s can be removed to reveal optional lines of code (indicated where applicable)

# BEFORE START
  # Compile data into a CSV file
  # Ensure your CSV file has 'Dose', 'Response', 'sd'(for standard deviation), and 'Probe' column headers as directed in Step 6.1 of the associated protocol
  # Set your working directory to where the file is located and enter your file name (lines 49-50)
  # Reveal lines 36-40 (i.e., delete "#") to install the required packages. Once installed, replace the #'s to disable their execution when running the code again.
  # Use colorspace or ggsci to generate color palettes (optional; lines 92-105)
  # Costomize chart elements (optional; lines 108-141, 152-154, 163-165, 170-172, 174, 179-180, 192-194, 204-215)

# INFORMATION ON ALGORITHMS & PACKAGES USED HEREIN
  # The purpose of nls.lm is to minimize the sum square of the vector returned by the function fn, by a modification of the Levenberg-Marquardt algorithm. 
    # Link for more info about the nls.lm package: https://cran.r-project.org/web/packages/minpack.lm/minpack.lm.pdf
    # Link for info on the regression algorithm employed by package: https://people.duke.edu/~hpgavin/lm.pdf
  # GGSCI = collection of high-quality color palettes inspired by colors used in scientific journals, data visualization libraries, science fiction movies, and TV shows.
    # Follow link for GGSCI usage info: https://nanx.me/ggsci/articles/ggsci.html


## Install and load required packages ------------------------------------------------------------------------------------------------------------------------------
#install.packages("ggplot2")
#install.packages("minpack.lm")
#install.packages("dplyr")
#install.packages("colorspace")
#install.packages("ggsci")
library(ggplot2)    #for plotting data
library(minpack.lm) #for nlsLM
library(dplyr)      #for data manipulation (specifically for 'Group' function)
library(colorspace) #for color palettes 
library(ggsci)      #for color palettes 


## Retrieve data ---------------------------------------------------------------------------------------------------------------------------------------------------
setwd("C:\\Documents\\CSV Files")         #CHANGE TO YOUR WORKING DIRECTORY (where your CSV file is located)
data <- read.csv("DR_Data.CSV", skip = 1) #CHANGE TO YOUR FILE NAME (skip = skips first row of CSV file)
print(data)                               #prints CSV data to R Console (to check all data is correctly formatted)


## Set up model ----------------------------------------------------------------------------------------------------------------------------------------------------
# Define logistic model function: y = C + A * (1 - exp(-k * Dose))
logistic_model <- function(Dose, A, k, C) {
  C + A * (1 - exp(-k * Dose))}

# Starting values for logistic model calculations
start_values <- list(A = 100, k = 10, C = 0)  #Adjust start values as needed

# Function to fit the model and generate predictions for each group
fit_model_and_predict <- function(group_data) {
  
  # Fit the model
  model <- nlsLM(Response ~ logistic_model(Dose, A, k, C),
                 data = group_data,
                 start = start_values)
 
   # Generate a sequence of Dose values (based off of real data Dose values) for prediction (i.e., extends x values)
  dose_seq <- seq(min(group_data$Dose, na.rm = TRUE), max(group_data$Dose, na.rm = TRUE), length.out = 400) 
      #length.out = number of values generated
      #na.rm = a logical value indicating whether NA values should be stripped before the computation proceeds
  
  # Generate predictions from the model using extended x-axis (i.e., generates extended y axis)
  predictions <- predict(model, newdata = data.frame(Dose = dose_seq))
  
  # Return a data frame with Dose and Predictions
  data.frame(Dose = dose_seq, Response = predictions, Probe = unique(group_data$Probe))}

# Apply the function to each group and bind the results together
predictions <- data %>%
  group_by(Probe) %>%
  do(fit_model_and_predict(.))%>%
  ungroup()

# Convert Group to a factor for consistent coloring
data$Probe <- as.factor(data$Probe)
predictions$Probe <- as.factor(predictions$Probe)


## Generate color palettes -----------------------------------------------------------------------------------------------------------------------------------------
## IF USING COLORSPACE:
  #Choose color palettes (only reveal and execute line 99 if you want to choose a new palette, hide again before executing entire code)
  #pal <- choose_palette() #reveal and execute line 100 only if you chose a new palette, set # = to # of colors in palette
  #pal(#) 
  #Palette examples (only reveal to see color, otherwise keep hidden when executing code):
  #Palette 2: "#005256" "#006B6E" "#008688" "#60A3A5" "#9EC1C2" "#D5E0E1" "#E5DCE2" "#CFB2C5" "#B98AAA" "#A3618F" "#8F3377" "#7F0064"
  #Palette 3: "#003B91" "#612D99" "#8C2595" "#A72E89" "#B84375" "#C05C5B" "#C0763B" "#B98E1B" "#ABA626" "#96BC52" "#7BD082" "#63E2B1" "#68EFDC" "#B0F4FA"

## IF USING GGSCI:
  #Choose color palettes from link (https://nanx.me/ggsci/articles/ggsci.html)
  #Reveal, edit, and execute lines 108 & 109 to see image of colors (hide again before executing entire code)
  #pal_npg(palette = c("NAME"), alpha = 1) #alpha = transparency, (value = 0-1)
  #show_col(pal_npg("nrc")(#))


## Define custom colors and shapes ---------------------------------------------------------------------------------------------------------------------------------
#The following lines include functions and variables specific to the ggplot2 package
  #A "cheat sheet" for data visualization with ggplot2 can be found in the 'Help' tab of the R Studio toolbar
#The following is set up to plot and calculate C50 values for up to four different data sets
  #Currently, elements for two data sets are hidden such that only two sets are plotted
  #Reveal and/or add additional lines (separated by comma) to plot additional data sets on the same graph

# Define custom colors 
custom_colors <- c(
  "#0080BE", 
 # "#003B58", 
 # "grey40", 
  "#7B0457")  

# Define custom data point shapes
custom_shapes <- c(
  21,  # = Filled-in circle
 # 22, # = Filled-in square
 # 23, # = Filled-in diamond
  24)  # = Filled-in triangle

# Define custom line types
custom_linetypes <- c(
  "solid", 
 # "longdash", 
 # "dotdash", 
  "twodash")

# Define custom line widths
custom_linewidth <- c(
  0.6, 
 # 0.6, 
 # 0.6, 
  0.6)


## Plot data points and fitted curve --------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = Dose, y = Response, color = Probe, shape = Probe, fill = Probe, linewidth = Probe)) +
  
  # add error bars
  geom_errorbar(
    aes(
      ymin = pmax(Response - sd, 0), #pmax() ensures the error bars show and don't go below 0 on the y-axis
      ymax = Response + sd, 
      color = Probe),                #color of error bar, currently set to color associated with Probe group ('Probe' can be changed to "black" or other color)
    width = 0.04,                    #width of horizontal lines in error bar (change when needed)
    linewidth = 0.3,                 #line thickness of error bar (change when needed)
    show.legend = FALSE              #hide error bar legend
  )+  
  
  # plot predicted line
  geom_line(data = predictions, aes(x = Dose, y = Response, color = Probe, linetype = Probe)) +
  
  # plot experimental data points
  geom_point(
    size = 1.5,     #size of data point (change when needed)
    stroke = 0.2,   #thickness of data point outline (change when needed)
    color = "black" #color of data point outline (change when needed)
  )+
  
  # adjust axis characteristics
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10),                               #specifies exact breaks (change when needed)
    labels = c("0.01", "0.1", "1", "10"),                       #what labels to include, dependent on specified breaks (change when needed)
    limits = c(0.01, 15),                                       #x-axis limits (change when needed)
    expand = c(0, 0),                                           #remove extra space around the plot
    guide = guide_axis_logticks(long = 1.5, mid = 1, short = 1) #sets length of log axis ticks (width is set below; change when needed)
  )+ 
  
  scale_y_continuous(
    expand = c(0, 0),             #remove extra space around the plot
    limits = c(0, 105),           #y-axis limits (change when needed)
    breaks = seq(0, 105, by = 20) #generalized breaks (change when needed)
  )+ 
  
  # functions that enable customization to graph appearance (set above)   
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_linewidth_manual(values = custom_linewidth) +
  
  # graph labels and legend elements
  labs(
    title = NULL,                 #graph title (change NULL to 'plot name' when needed)
    x = "[Probe] (μM)",           #x-axis label (change when needed)
    y = "dsDNA Invasion (%)",     #y-axis label (change when needed)
    color = NULL, shape = NULL,   #setting 'color', 'shape', 
    linetype = NULL, fill = NULL,   #'linetype', 'fill', 
    linewidth = NULL                #and 'linewidth' to 'NULL' removes unnecessary legend elements
  )+
  
  # graph theme/appearance
  theme_classic() +                                                          #ggplot2 theme with very simple graph aesthetics
  theme(                                                                     #alterations to classic theme
    # X-axis ticks and line                                                  #'element_text' elements applies to graph text (change when needed)
    axis.text.x = element_text(color = "black", size = 8, family = "sans"),  #'element_line' elements applies to graph axis and tick lines (change when needed)
    axis.title.x = element_text(color = "black", size = 8, family = "sans"),
    axis.line.x = element_line(color = "black", linewidth = 0.4),
    axis.ticks.x = element_line(color = "black", linewidth = 0.4),
    # Y-axis ticks and line 
    axis.text.y = element_text(color = "black", size = 8, family = "sans"),
    axis.title.y = element_text(vjust = 2, color = "black", size = 8, family = "sans"),
    axis.line.y = element_line(color = "black", linewidth = 0.4),
    axis.ticks.y = element_line(color = "black", linewidth = 0.4),
    # Legend
    legend.text = element_text(color = "black", size = 8, family = "sans"),
    legend.position = c(0.17, 0.83)                                          #sets position of legend in graph window (change when needed)
  )+
    coord_cartesian(clip = 'off')                                            #ensures points on the axes are fully shown


## Print line-fitting variables and C50 for each probe -------------------------------------------------------------------------------------------------------------
data %>%
  group_by(Probe) %>%
  do({
    coef_model <- coef(nlsLM(Response ~ logistic_model(Dose, A, k, C), data = ., start = start_values))
    A <- coef_model["A"]
    k <- coef_model["k"]
    C <- coef_model["C"]
    C50um <- (log((-(50 - C) / A) + 1)) / (-k)
    data.frame(Probe = unique(.$Probe), A = A, k = k, C = C, C50um = C50um)
  }) %>%
  print()

## Export plot as an SVG or TIFF file for best image quality. Import SVG into software like Inkscape or Adobe Illustrator to format publishable figures. 
