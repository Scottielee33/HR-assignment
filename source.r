# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stats)

# Read data
ceo_data <- read_csv("survey_response_data.csv")

# Data manipulation
ceo_data <- ceo_data %>%
  filter(level1 == 'interacting', type != 'personal_family', id != 1115)

# Construct dataset with six features
df1 <- table(ceo_data$id, ceo_data$F_duration)
df2 <- table(subset(ceo_data, F_planned != 'missing')$id, subset(ceo_data, F_planned != 'missing')$F_planned)
df3 <- table(subset(ceo_data, F_participants != 'missing')$id, subset(ceo_data, F_participants != 'missing')$F_participants)

ceo_data$ins_alone <- ifelse(ceo_data$ins == 1.0 & ceo_data$out == 0.0, 1, 0)
ceo_data$out_alone <- ifelse(ceo_data$ins == 0.0 & ceo_data$out == 1.0, 1, 0)
ceo_data$ins_out <- ifelse(ceo_data$ins == 1.0 & ceo_data$out == 1.0, 1, 0)
ceo_data$coordinate <- ifelse(ceo_data$n_functions > 1, 1, 0)
ceo_data$activity_dummy <- 1

df4 <- table(ceo_data$id, ceo_data$ins_alone)
df4a <- table(ceo_data$id, ceo_data$ins)
df5 <- table(ceo_data$id, ceo_data$ins_out)
df5a <- table(ceo_data$id, ceo_data$out)
df6 <- table(ceo_data$id, ceo_data$coordinate)

# Additional crosstabs for other features
df_groupcom <- table(ceo_data$id, ceo_data$groupcom)
df_bunits <- table(ceo_data$id, ceo_data$bunits)
df_coo <- table(ceo_data$id, ceo_data$coo)
df_cao <- table(ceo_data$id, ceo_data$cao)

df1 <- as.data.frame.matrix(df1)
df2 <- as.data.frame.matrix(df2)
df3 <- as.data.frame.matrix(df3)
df5a <- as.data.frame.matrix(df5a)
df_groupcom <- as.data.frame.matrix(df_groupcom)
df_bunits <- as.data.frame.matrix(df_bunits)
df_coo <- as.data.frame.matrix(df_coo)
df_cao <- as.data.frame.matrix(df_cao)
df6 <- as.data.frame.matrix(df6)

# Aggregate data
agg_data <- data.frame(id = rownames(df1))
agg_data$long <- df1['1hrplus']
agg_data$planned <- df2['planned']
agg_data$large <- df3['two_plus_ppl']
df5a <- rename(df5a, out = `1`)
agg_data$out <- df5a['out']
df_groupcom <- rename(df_groupcom, coordinate1 = `1`)
df_bunits <- rename(df_bunits, coordinate1 = `1`)
df_coo <- rename(df_coo, coordinate1 = `1`)
df_cao <- rename(df_cao, coordinate1 = `1`)
agg_data$coordinate1 <- df_groupcom['coordinate1'] + df_bunits['coordinate1'] + df_coo['coordinate1'] + df_cao['coordinate1']
df6 <- rename(df6, coordinate2 = `1`)
agg_data$coordinate2 <- df6['coordinate2']
id <- agg_data$id
agg_data$id <- NULL

activities <- table(ceo_data$id)
activities <- as.data.frame.array(activities)

agg_data_share <- sweep(agg_data, 1, activities$activities, "/")

# Calculate correlation
cor <- cor(agg_data_share)

# Eigen decomposition
eig <- eigen(cor)

# Get the indices of the two largest eigenvalues
indices <- tail(order(eig$values), 2)

# Select the corresponding eigenvectors
selected_eig_vecs <- eig$vectors[, indices]

if (!is.matrix(agg_data_share) || !is.numeric(agg_data_share)) {
  # Convert agg_data_share to a numeric matrix
  agg_data_share <- as.matrix(sapply(agg_data_share, as.numeric))
}

# Check if selected_eig_vecs is a numeric matrix
if (!is.matrix(selected_eig_vecs) || !is.numeric(selected_eig_vecs)) {
  # Convert selected_eig_vecs to a numeric matrix
  selected_eig_vecs <- as.matrix(sapply(selected_eig_vecs, as.numeric))
}

# Now you should be able to perform the PCA
pca <- agg_data_share %*% selected_eig_vecs

# K-MEANS
k_means_result <- kmeans(agg_data_share, 2)

# OUTPUT RESULTS TO FILE
ceo_type <- data.frame(id = id, pca1 = pca[,2], pca2 = pca[,1], k_means = k_means_result$cluster)
row.names(ceo_type) <- NULL

write.csv(ceo_type, 'clustersr.csv', row.names = FALSE)