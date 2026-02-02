library(tidyverse)
library(readODS)
library(plotly)

# Primary Data Source is Office of Rail and Road 
# https://dataportal.orr.gov.uk/data-table-catalogue/
# For the Complaint Dataset, can search the link what I provided 
# Table Name is Table 4113 - Complaints per 100,000 journeys by operator

#Load the dataset into R
filePath <- "4113-complaint-rate-by-operator.ods"
df_complaints <- read_ods(
  path  = filePath,
  sheet = 3,   
  skip  = 5    
)

df_annual <- df_complaints [(0:18),]

#Cleaning stage and create the dataframe 
#Excluded the covid pperiod to get the consistent visualisation and result 
# According to the ORR catalogue, we need to excluded the covid period, the passender complaint and travel pattern are not consisternt
# For this three years, Lumo operator is excluded because it is established in 2021
# First Captital and Southern is also excluded because both of them were merged and established Govia Thameslink

VisualDataFrame <- df_annual |>
  filter(`Time period` %in% c(
    "Apr 2017 to Mar 2018 [b]",
    "Apr 2018 to Mar 2019",
    "Apr 2019 to Mar 2020",
    "Apr 2022 to Mar 2023 [p] [r]",
    "Apr 2023 to Mar 2024 [b] [p] [r]", 
    "Apr 2024 to Mar 2025 [p]"
  ))


# Remove z- value, data not applicable 

VisualDataFrame <- VisualDataFrame %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

# To remove annotation in Time Period column

VisualDataFrame<-VisualDataFrame %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

# To change the format from wide to long 

VisualDataFrame_Complaint <- VisualDataFrame |>
  pivot_longer(
    -`Time period`,
    names_to  = "Operator",
    values_to = "TotalComplaintRate"
  )

# To remove annotation in Operator column

VisualDataFrame_Complaint <- VisualDataFrame_Complaint %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

anyNA(VisualDataFrame_Complaint)

str(VisualDataFrame_Complaint)

VisualDataFrame_Complaint <- mutate(VisualDataFrame_Complaint,
                                    TotalComplaintRate = as.numeric(TotalComplaintRate))

# To show the distribution pre and post period 

VisualDataFrame_Complaint$PeriodGroup <- ifelse(
  VisualDataFrame_Complaint$`Time period` %in% c(
    "Apr 2017 to Mar 2018",
    "Apr 2018 to Mar 2019",
    "Apr 2019 to Mar 2020"
  ),
  "Pre-Covid",
  "Post-Covid"
)

#Aim is to show the distribution of complaint rate to identify the outlier, variance 
#use boxplot

ggplot(VisualDataFrame_Complaint, aes(x= factor(`Time period`),
                                      y = TotalComplaintRate,
                                      fill = PeriodGroup)) +
  geom_boxplot(varwidth = TRUE, outlier.size = 1.5)+
  scale_x_discrete(labels = c("Apr 2017 to Mar 2018"="Apr 2017- Mar 2018",
                              "Apr 2018 to Mar 2019"="Apr 2018- Mar 2019",
                              "Apr 2019 to Mar 2020"="Apr 2019- Mar 2020",
                              "Apr 2022 to Mar 2023"="Apr 2022- Mar 2023",
                              "Apr 2023 to Mar 2024"="Apr 2023– Mar 2024",
                              "Apr 2024 to Mar 2025"="Apr 2024– Mar 2025"))+
  scale_fill_manual(values = c("Pre-Covid" = "grey80",
                               "Post-Covid" = "#D35FB7"))+
  labs(x="Time Period", y = "Complaints per 100,000 Journeys", 
       title = "Distribution of Passenger Complaint Rates Before and After the Pandemic",
       fill = "Period",
       caption = "Office of Rail and Road Dataset")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey95", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 12),
    axis.text    = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )


#Finding the pattern Highest Complaint rate operator and lowest pattern 
#which pattern are stable or violate.

# Find the average for the operator rank and arrange the values 

Operator_rank <- VisualDataFrame_Complaint |>
  group_by(Operator) |>
  summarise(
    ComplaintMeanRate = mean(TotalComplaintRate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(ComplaintMeanRate)

#To reduce the clutter, only visual five operators for both , identify the pattern 
# Stale or fluctuation , can know the nature of the complaint rate 

ComplaintLowest_5<- Operator_rank |> slice_head(n = 5)
ComplaintHighest_5  <- Operator_rank |> slice_tail(n = 5)

lowops <- ComplaintLowest_5$Operator
highops <- ComplaintHighest_5$Operator

# To seperate high and low complaint 

NewSelected <- VisualDataFrame_Complaint %>%
  filter(Operator %in% c(highops, lowops)) %>%
  mutate(group = if_else(Operator %in%highops,
                         "High complaint operators",
                         "Low complaint operators"))

ggplot(NewSelected,
       aes(x = `Time period`,
           y = TotalComplaintRate,
           group = Operator,
           colour = Operator)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_discrete(labels = c("Apr 2017 to Mar 2018"="Apr 2017- Mar 2018",
                              "Apr 2018 to Mar 2019"="Apr 2018- Mar 2019",
                              "Apr 2019 to Mar 2020"="Apr 2019- Mar 2020",
                              "Apr 2022 to Mar 2023"="Apr 2022- Mar 2023",
                              "Apr 2023 to Mar 2024"="Apr 2023– Mar 2024",
                              "Apr 2024 to Mar 2025"="Apr 2024– Mar 2025"))+
  scale_colour_manual(values = c(
    "Avanti West Coast" = "#E69F00", # use the color-blind safe palette (Okabe-Ito)
    "Caledonian Sleeper" = "#56B4E9",
    "Chiltern Railways" = "#F0E442",
    "Elizabeth line" = "#009E73", # use bluish green, do not use pure green
    "Govia Thameslink Railway" = "#0072B2",
    "Grand Central" = "#A6761D",
    "Hull Trains" = "#7570B3",
    "London North Eastern Railway" = "#999999",
    "London Overground" = "#CC79A7", # use radish purple , do not use pure red
    "Merseyrail" = "#D55E00"
  ))+
  facet_wrap(~ group, scales = "free_y") +
  labs(
    x = "Time Period",
    y = "Complaints per 100,000 journeys",
    title = "Trends in Complaint Rates for Highest and Lowest Operators",
    caption = "Office of Rail and Road Dataset"
  ) +
  theme_minimal(base_size = 13) + # to improve text readability, increased based font size
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(hjust = 0.5, face = "bold"),
    axis.title.y =  element_text(face = "bold"), 
    strip.text = element_text(face="bold"),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 10),
    panel.grid.major = element_line(linewidth = 1),
    panel.grid.minor = element_blank()
  )


#To visualise the operators' characteristics by using clustering 
#For 2024 to 2025 
# for 2024-2025, Lumo is included 

filepath_2024 <- "4113-complaint-rate-by-operator.ods"
Complaint2024 <- read_ods(
  path  = filepath_2024,
  sheet = 3,   
  skip  = 5 
)

Complaint2024 <- Complaint2024 [(0:18),]

# To remove the annotation from Time Period column

Complaint2024 <- Complaint2024 %>%
  mutate(`Time period` = str_trim(str_remove_all(`Time period`, "\\[.*?\\]")))

#only use 2024-2025
Complaint2024 <- Complaint2024%>%
  filter(`Time period` == "Apr 2024 to Mar 2025")

#Remove z (data not applicable)

Complaint2024 <- Complaint2024 %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

#convert the format, wider to longer

Complaint2024 <- Complaint2024 %>%
  pivot_longer(
    cols = -c (`Time period`),
    names_to = "Operator",
    values_to = "TotalComplaintRate"
  ) 

# want to remove Time period column 

Complaint2024 <- select(Complaint2024, "Operator", "TotalComplaintRate") %>%
  mutate(TotalComplaintRate = as.numeric(TotalComplaintRate))

# To remove annotation from Opeartor column

Complaint2024 <- Complaint2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#This is another dataframe, which can find in data catalogue
# Table name is TOC key statistics,	Table 2200 - All operators
# There are multiple dataset in this dataframe, I used Table 2200d : Delay minutes, annual data

#To load data into R (TotalDelayAnualData)

x2024 <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200d : Delay minutes, annual data

z <- which(x2024[[1]] == "Table 2200d: Delay minutes, annual data")

# Extract table (header + data)
tbl2024 <- x2024[(z + 1):(z + 20), ]

# Clean
tbl2024 <- tbl2024[, colSums(!is.na(tbl2024)) > 0]
colnames(tbl2024) <- tbl2024[1, ]

Delay2024 <- tbl2024[-1, ]
View(Delay2024)

Delay2024<- Delay2024[(1:15),]

Delay2024 <- filter(Delay2024, `Time period` == "Apr 2024 to Mar 2025")

# There are three measurement for delay, to get the total delay, aggregate these three measurement
SumedDelay2024 <- Delay2024 %>%
  mutate(
    across(
      -c(`Time period`, Measure),
      ~ as.numeric(as.character(.))
    )
  )

SumedDelay2024 <- SumedDelay2024 %>%
  select(-`Time period`, -Measure) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

SumedDelay2024 <- SumedDelay2024  %>%
  pivot_longer(
    everything(),
    names_to = "Operator",
    values_to = "TotalDelayMinutes"
  )

SumedDelay2024 <- SumedDelay2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#For cancellation
#Table 2200c : Punctuality and Reliablility from TOC key statistics,	Table 2200 - All operators


# Load sheet 3 

x_canceldata <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# Find start of Table 2200c: Punctuality and reliability, annual data
c <- which(x_canceldata[[1]] ==
             "Table 2200c: Punctuality and reliability, annual data")

# Extract table (header + data)
tbl_cancel2024 <- x_canceldata[(c + 1):(c + 20), ]  

# Clean
tbl_cancel2024 <- tbl_cancel2024[, colSums(!is.na(tbl_cancel2024)) > 0]
colnames(tbl_cancel2024) <- tbl_cancel2024[1, ]

Cancel2024 <- tbl_cancel2024[-1, ]


Cancel2024 <- Cancel2024[(1:5),]

Cancel2024 <- filter(Cancel2024, `Time period` == "Apr 2024 to Mar 2025" )

CancelPercent2024 <- Cancel2024 %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

CancelPercent2024 <- select(CancelPercent2024, "Operator", "TotalCancelPercent")

CancelPercent2024 <- CancelPercent2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#Combine the three datasets, used inner_join (to mach all the datasets)
CombinedDataframe2024 <- Complaint2024 %>%
  inner_join(SumedDelay2024, by = "Operator") %>%
  inner_join(CancelPercent2024, by = "Operator")

#Use Clustering Method,
#Data Preparation

ClusterData2024 <- CombinedDataframe2024 %>%
  select(TotalComplaintRate,TotalDelayMinutes, TotalCancelPercent)

#Scaling the variables, measurement are different

ScaledVariables2024 <- scale(ClusterData2024)

set.seed(123)

# For this, I selected centroid 3, but it is satisfied enough seperation and
#homogeneity

ClusteredResult2024 <- kmeans(ScaledVariables2024, centers = 3, nstart = 25)

CombinedDataframe2024$Cluster <- factor(
  ClusteredResult2024$cluster,
  levels = c(1, 2, 3),
  labels = c("Group 1", "Group 2", "Group 3")
)

# #To Visualise the cluster dataset
#For three dimensional visualisation, I used plot_ly, can move easily 
#and can check easily the cluster point so, decided to use plot_ly instead of ggplot
# But when I report, it is reported static, it can be misinterpretation for users


plot_ly(
  CombinedDataframe2024,
  x = ~TotalDelayMinutes,
  y = ~TotalComplaintRate,
  z = ~TotalCancelPercent,
  color = ~Cluster,
  colors = c("red", "blue", "purple"),
  type = "scatter3d",
  mode = "markers",
  text = ~paste(
    "Operator:", Operator,
    "<br>Delay Minutes:", sprintf("%.2f", TotalDelayMinutes),
    "<br>Complaint Rate:", sprintf("%.2f", TotalComplaintRate),
    "<br>Cancellation Percent:", sprintf("%.2f", TotalCancelPercent)
  ),
  hoverinfo = "text",
  marker = list(size = 6)
) %>%
  layout(
    title = "<b> Characteristics of UK Rail Operators (2024–2025) </b>",
    legend = list(
      title = list(text = "Cluster Group")
    ),
    scene = list(
      xaxis = list(title = "Total Delay Minutes"),
      yaxis = list(title = "Total Complaint Rate"),
      zaxis = list(title = "Cancellation Percentage")
    )
  )


#For Camplaint Dataframe

Newfilepath <- "4113-complaint-rate-by-operator.ods"
NewComplaint <- read_ods(
  path  = Newfilepath,
  sheet = 3,  # only need third sheet
  skip  = 5    
)
# Used annual data, removing unused row

NewComplaint <- NewComplaint [(0:18),]

# Use three years, 2022-23,23-24,24-25, remove other rows

NewComplaint <- NewComplaint [(16:18),]

NewComplaint <- NewComplaint %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

NewComplaint <- NewComplaint %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

NewComplaint <- NewComplaint |>
  pivot_longer(
    -`Time period`,
    names_to  = "Operator",
    values_to = "TotalComplaintRate"
  )

NewComplaint <- NewComplaint %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )%>%
  mutate(TotalComplaintRate = as.numeric(TotalComplaintRate))

#For Delay Dataframe
# For the another data (Annual Total Delay)

#To load data into R
x <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200d : Delay minutes, annual data

i <- which(x[[1]] == "Table 2200d: Delay minutes, annual data")

# Extract table (header + data)
tbl <- x[(i + 1):(i + 20), ]  

# Cleaning 
tbl <- tbl[, colSums(!is.na(tbl)) > 0]
colnames(tbl) <- tbl[1, ]

NewDelay <- tbl [-1,]

NewDelay <- NewDelay[(7:15),]

NewDelay <- NewDelay %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

#Aggregate three measurement

SumedNewDelay <- NewDelay %>%
  group_by(`Time period`) %>%
  summarise(
    across(
      -Measure,
      ~ sum(as.numeric(.), na.rm = TRUE)
    )
  )

DelayDataframe <- SumedNewDelay %>%
  pivot_longer(
    cols = -`Time period`,
    names_to = "Operator",
    values_to = "TotalDelayMinutes"
  )

DelayDataframe <- DelayDataframe %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )
# For Cancel percent Dataframe

x_cancel <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200c: Punctuality and reliability, annual data

j <- which(x_cancel[[1]] ==
             "Table 2200c: Punctuality and reliability, annual data")

# Extract table (header + data)

tbl_cancel <- x_cancel[(j + 1):(j + 20), ]

# Clean
tbl_cancel <- tbl_cancel[, colSums(!is.na(tbl_cancel)) > 0]
colnames(tbl_cancel) <- tbl_cancel[1, ]
NewCancel <- tbl_cancel[-1,]

NewCancel <- NewCancel[(1:5),]

NewCancel <- NewCancel[(3:5),]

#Similar way with the above code, convert table format, wider to longer

NewCancel<- NewCancel %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

NewCancel <- select(NewCancel, `Time period`,"Operator", "TotalCancelPercent")

NewCancel <- NewCancel %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#Combining three datasets, used inner_join, to match with three dataset

NewDataFrame <- NewComplaint %>%
  inner_join(DelayDataframe, by = c("Time period","Operator")) %>%
  inner_join(NewCancel, by = c("Time period","Operator"))

# For Heatmap
# Performance calculation based on three metrics

# make normalisation, scales are different, used z-score 
PerformanceData <- NewDataFrame %>%
  group_by(`Time period`) %>% 
  mutate(
    Complaint_z = scale(TotalComplaintRate),
    Delay_z     = scale(TotalDelayMinutes),
    Cancel_z    = scale(TotalCancelPercent)
  ) %>%
  ungroup()


# Calculate the Composite performance score (derived variables) based on 3 indicators
PerformanceData <- PerformanceData %>%
  mutate(
    PerformanceScore = Complaint_z + Delay_z + Cancel_z
  ) 

# To visualise performance score, used visualisation
ForHeatmap <- PerformanceData %>%
  select(`Time period`,Operator, PerformanceScore) %>%
  mutate(
    Operator = factor(Operator),
    `Time period` = factor(`Time period`)
  )

View(ForHeatmap)

ggplot(ForHeatmap, aes(x = `Time period`, y = Operator, fill = PerformanceScore)) +
  geom_tile() +
  scale_fill_viridis_c( # For colour blind safe
    option = "viridis", 
    direction = -1,
    breaks = c(min(ForHeatmap$PerformanceScore+0.1, na.rm = TRUE),
               max(ForHeatmap$PerformanceScore-0.1, na.rm = TRUE)),
    labels = c("High performance", "Low performance"),
    name = "Performance Score",
    guide = guide_colorbar(reverse = TRUE)
  )+
  labs(
    title = "Composite Performance Patterns of UK Rail Operators, 2022–2025",
    x = "Time period", y = "Operator", caption = "Office of Rail and Road Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 12, hjust = 0.5, face = "bold"),
        axis.title.y =  element_text(size = 12,face = "bold"), 
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )







