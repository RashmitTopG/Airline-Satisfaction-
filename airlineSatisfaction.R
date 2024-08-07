df <- read.csv("C:/Users/Rashmit Mhatre/Downloads/Invistico_Airline.csv")

# Extract the training data and labels
train_data <-
    scale(matrix(
        c(
            df$Gender,
            df$Customer.Type,
            df$Age,
            df$Type.of.Travel,
            df$Class,
            df$Flight.Distance,
            df$Seat.comfort,
            df$Departure.Arrival.time.convenient,
            df$Food.and.drink,
            df$Gate.location,
            df$Inflight.wifi.service,
            df$Inflight.entertainment,
            df$Online.support,
            df$Ease.of.Online.booking,
            df$On.board.service,
            df$Leg.room.service,
            df$Baggage.handling,
            df$Checkin.service,
            df$Cleanliness,
            df$Online.boarding,
            df$Departure.Delay.in.Minutes,
            df$Arrival.Delay.in.Minutes
        ),
        ncol = 22
    ))
train_labels <- df$satisfaction

# KNN algorithm with weighted Euclidean distance

# Define a function to calculate the weighted Euclidean distance
weighted_euclidean_distance <- function(point1, point2, weights) {
    distance <- sqrt(sum((weights * (point1 - point2))^2))
    return(distance)
}

# Define a function to perform weighted k-nearest neighbors
knn <- function(train_data,
                train_labels,
                new_ratings,
                weights,
                k) {
    distances <- matrix(0, nrow = nrow(train_data), ncol = 1)

    for (i in 1:nrow(train_data)) {
        distances[i, 1] <-
            weighted_euclidean_distance(train_data[i, ], new_ratings, weights)
    }

    neighbor_info <- cbind(distances, train_labels)
    neighbor_info <- neighbor_info[order(neighbor_info[, 1]), ]
    nearest_neighbors <- neighbor_info[1:k, ]
    predicted_satisfaction <-
        ifelse(sum(nearest_neighbors[, 2]) >= k / 2, 1, 0)

    return(predicted_satisfaction)
}

# Define a function to calculate accuracy
calculate_accuracy <- function(predicted, actual) {
    return(mean(predicted == actual))
}

# Example usage

# New ratings
new_ratings <- scale(c(0, 1, 29, 1, 0, 1843, 5, 3, 3, 4, 3, 1, 5, 2, 4, 3, 5, 3, 1, 3, 200, 204))

# Weights
weights <- c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25)

# Set a range of values for k to tune
# k_values <- c(3, 29, 449, 1013, 8161)  # Add more values to the list
if (nrow(df) > 100000) {
    k_values <- runif(5, min = 3, max = 19999)
} else if (nrow(df) < 100000 && nrow(df) > 50000) {
    k_values <- runif(5, min = 3, max = 9999)
} else if (nrow(df) < 10000) {
    k_values <- runif(5, min = 3, max = 1999)
}

l <- 1
for (num in k_values) {
    k_values[l] <- round(num, digit = 0)
    if (k_values[l] %% 2 == 0) {
        k_values[l] <- k_values[l] + 1
    }
    l <- l + 1
}


# Initialize a vector to store accuracy results
accuracy_results <- numeric(length(k_values))

# Perform hyperparameter tuning
for (i in 1:length(k_values)) {
    k <- k_values[i]
    predicted_satisfaction <-
        knn(train_data, train_labels, new_ratings, weights, k)
    accuracy <-
        calculate_accuracy(predicted_satisfaction, train_labels)
    accuracy_results[i] <- accuracy
    cat(
        "k =",
        k,
        " -> Predicted Satisfaction:",
        predicted_satisfaction,
        " -> Accuracy:",
        accuracy,
        "\n"
    )
}

# Find the best k value (e.g., the one with the highest accuracy)
best_k <- k_values[which.max(accuracy_results)]
cat(
    "Best k:",
    best_k,
    " -> Best Accuracy:",
    max(accuracy_results),
    "\n"
)

# Apply the elbow method to find the optimal k
library(ggplot2)

# Create a data frame for plotting
elbow_data <- data.frame(k = k_values, Accuracy = accuracy_results)

# Plot the accuracy curve
ggplot(elbow_data, aes(x = k, y = Accuracy)) +
    geom_line() +
    geom_point() +
    labs(x = "k", y = "Accuracy") +
    ggtitle("KNN Elbow Method")

# You can visually inspect the plot to identify the "elbow point"
