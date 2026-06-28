# ------------------------------------------------------------
# IMPORTANT: Run this package setup first before running all code
# This section checks whether the required package is installed.
# If the package is missing, R will install it automatically.
# The package is needed to create the graphs in this analysis.
# ------------------------------------------------------------

required_packages <- c("ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}


# Load dataset
# IMPORTANT: data_new.csv must be in the same folder as this RMarkdown file
data <- read.csv(file.choose(), check.names = FALSE)

# Check column names
names(data)

data_clean <- data[, c(
  "Year",
  "QuarterDate",
  "Quarter",
  "City",
  "State",
  "HPGrowth_pct(%)",
  "InterestRate(%)",
  "Inflation(%)",
  "NetOverseasMigration",
  "HouseholdIncome_per_capita",
  "Covid19",
  "Election_dummy",
  "Quarter2",
  "Quarter3",
  "Quarter4",
  "Time Trend"
)]

# ============================================================
# Model 1: Baseline Time-Control Model
# Time trend + quarter seasonality + state controls
# ============================================================

model1 <- lm(
  `HPGrowth_pct(%)` ~ Quarter2 + Quarter3 + Quarter4 + `Time Trend` + State,
  data = data_clean
)

summary(model1)


# ============================================================
# Model 2: Add Interest Rate and Inflation
# Model 1 + basic macroeconomic variables
# ============================================================

model2 <- lm(
  `HPGrowth_pct(%)` ~ `InterestRate(%)` + `Inflation(%)` +
    Quarter2 + Quarter3 + Quarter4 + `Time Trend` + State,
  data = data_clean
)

summary(model2)


# ============================================================
# Model 3: Add Migration and Household Income
# Model 2 + demand and income variables
# ============================================================

model3 <- lm(
  `HPGrowth_pct(%)` ~ `InterestRate(%)` + `Inflation(%)` +
    NetOverseasMigration + HouseholdIncome_per_capita +
    Quarter2 + Quarter3 + Quarter4 + `Time Trend` + State,
  data = data_clean
)

summary(model3)


# ============================================================
# Model 4: Add COVID-19
# Model 3 + COVID-19 dummy variable
# ============================================================

model4 <- lm(
  `HPGrowth_pct(%)` ~ `InterestRate(%)` + `Inflation(%)` +
    NetOverseasMigration + HouseholdIncome_per_capita + Covid19 +
    Quarter2 + Quarter3 + Quarter4 + `Time Trend` + State,
  data = data_clean
)

summary(model4)


# ============================================================
# Model 5: Add Election Dummy
# Model 4 + election dummy variable
# ============================================================

model5 <- lm(
  `HPGrowth_pct(%)` ~ `InterestRate(%)` + `Inflation(%)` +
    NetOverseasMigration + HouseholdIncome_per_capita + Covid19 +
    Election_dummy + Quarter2 + Quarter3 + Quarter4 +
    `Time Trend` + State,
  data = data_clean
)

summary(model5)

# Function to calculate Significance F
get_f_pvalue <- function(model) {
  f <- summary(model)$fstatistic
  pf(f[1], f[2], f[3], lower.tail = FALSE)
}

comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  R_Squared = c(
    summary(model1)$r.squared,
    summary(model2)$r.squared,
    summary(model3)$r.squared,
    summary(model4)$r.squared,
    summary(model5)$r.squared
  ),
  Adjusted_R_Squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared,
    summary(model4)$adj.r.squared,
    summary(model5)$adj.r.squared
  ),
  Significance_F = c(
    get_f_pvalue(model1),
    get_f_pvalue(model2),
    get_f_pvalue(model3),
    get_f_pvalue(model4),
    get_f_pvalue(model5)
  )
)

comparison

# Function to calculate RMSE and MAE
get_accuracy <- function(model, data) {
  predicted <- predict(model)
  actual <- data$`HPGrowth_pct(%)`
  residual <- actual - predicted
  
  rmse <- sqrt(mean(residual^2))
  mae <- mean(abs(residual))
  
  return(c(RMSE = rmse, MAE = mae))
}

accuracy_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  rbind(
    get_accuracy(model1, data_clean),
    get_accuracy(model2, data_clean),
    get_accuracy(model3, data_clean),
    get_accuracy(model4, data_clean),
    get_accuracy(model5, data_clean)
  )
)

accuracy_table

install.packages("ggplot2")
library(ggplot2)

# Create quarter label
data_clean$QuarterLabel <- ifelse(data_clean$Quarter2 == 1, "Q2",
                                  ifelse(data_clean$Quarter3 == 1, "Q3",
                                         ifelse(data_clean$Quarter4 == 1, "Q4", "Q1")))

# Create quarter date
data_clean$QuarterDate <- as.Date(
  paste(
    data_clean$Year,
    ifelse(data_clean$QuarterLabel == "Q1", "01-01",
           ifelse(data_clean$QuarterLabel == "Q2", "04-01",
                  ifelse(data_clean$QuarterLabel == "Q3", "07-01", "10-01"))),
    sep = "-"
  )
)

# Predicted values from Model 4
data_clean$Predicted_Model4 <- predict(model4)


# Select NSW
state_data <- subset(data_clean, State == "NSW")

ggplot(state_data, aes(x = QuarterDate)) +
  annotate(
    "rect",
    xmin = min(state_data$QuarterDate[state_data$Covid19 == 1]),
    xmax = max(state_data$QuarterDate[state_data$Covid19 == 1]),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "grey"
  ) +
  geom_line(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), linewidth = 1) +
  geom_point(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), size = 1.5) +
  geom_line(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), size = 1.5) +
  scale_colour_manual(
    values = c(
      "Actual HPGrowth" = "steelblue",
      "Predicted HPGrowth" = "darkorange"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Model 4: Actual vs Predicted Quarterly Housing Price Growth in NSW",
    subtitle = "Each point represents one quarter; grey area represents the COVID-19 period",
    x = "Year",
    y = "Housing Price Growth (%)",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Predicted values from Model 4
data_clean$Predicted_Model4 <- predict(model4)


# Select VIC
state_data <- subset(data_clean, State == "VIC")

ggplot(state_data, aes(x = QuarterDate)) +
  annotate(
    "rect",
    xmin = min(state_data$QuarterDate[state_data$Covid19 == 1]),
    xmax = max(state_data$QuarterDate[state_data$Covid19 == 1]),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "grey"
  ) +
  geom_line(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), linewidth = 1) +
  geom_point(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), size = 1.5) +
  geom_line(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), size = 1.5) +
  scale_colour_manual(
    values = c(
      "Actual HPGrowth" = "steelblue",
      "Predicted HPGrowth" = "darkorange"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Model 4: Actual vs Predicted Quarterly Housing Price Growth in VIC",
    subtitle = "Each point represents one quarter; grey area represents the COVID-19 period",
    x = "Year",
    y = "Housing Price Growth (%)",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Select QLD
state_data <- subset(data_clean, State == "QLD")

ggplot(state_data, aes(x = QuarterDate)) +
  annotate(
    "rect",
    xmin = min(state_data$QuarterDate[state_data$Covid19 == 1]),
    xmax = max(state_data$QuarterDate[state_data$Covid19 == 1]),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "grey"
  ) +
  geom_line(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), linewidth = 1) +
  geom_point(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), size = 1.5) +
  geom_line(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), size = 1.5) +
  scale_colour_manual(
    values = c(
      "Actual HPGrowth" = "steelblue",
      "Predicted HPGrowth" = "darkorange"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Model 4: Actual vs Predicted Quarterly Housing Price Growth in QLD",
    subtitle = "Each point represents one quarter; grey area represents the COVID-19 period",
    x = "Year",
    y = "Housing Price Growth (%)",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Select SA
state_data <- subset(data_clean, State == "SA")

ggplot(state_data, aes(x = QuarterDate)) +
  annotate(
    "rect",
    xmin = min(state_data$QuarterDate[state_data$Covid19 == 1]),
    xmax = max(state_data$QuarterDate[state_data$Covid19 == 1]),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "grey"
  ) +
  geom_line(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), linewidth = 1) +
  geom_point(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), size = 1.5) +
  geom_line(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), size = 1.5) +
  scale_colour_manual(
    values = c(
      "Actual HPGrowth" = "steelblue",
      "Predicted HPGrowth" = "darkorange"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Model 4: Actual vs Predicted Quarterly Housing Price Growth in SA",
    subtitle = "Each point represents one quarter; grey area represents the COVID-19 period",
    x = "Year",
    y = "Housing Price Growth (%)",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Select WA
state_data <- subset(data_clean, State == "WA")

ggplot(state_data, aes(x = QuarterDate)) +
  annotate(
    "rect",
    xmin = min(state_data$QuarterDate[state_data$Covid19 == 1]),
    xmax = max(state_data$QuarterDate[state_data$Covid19 == 1]),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "grey"
  ) +
  geom_line(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), linewidth = 1) +
  geom_point(aes(y = `HPGrowth_pct(%)`, colour = "Actual HPGrowth"), size = 1.5) +
  geom_line(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted_Model4, colour = "Predicted HPGrowth"), size = 1.5) +
  scale_colour_manual(
    values = c(
      "Actual HPGrowth" = "steelblue",
      "Predicted HPGrowth" = "darkorange"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Model 4: Actual vs Predicted Quarterly Housing Price Growth in WA",
    subtitle = "Each point represents one quarter; grey area represents the COVID-19 period",
    x = "Year",
    y = "Housing Price Growth (%)",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )




