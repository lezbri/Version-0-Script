# Version-0-Script
Version 0 Script
Annotated Script

# Install and load required packages- this is a comment
if (!require(tidyverse)) install.packages("tidyverse") – if statement used to specify a block of code to be executed if a condition is true
if (!require(gganimate)) install.packages("gganimate")– if statement used to specify a block of code to be executed if a condition is true
if (!require(gifski)) install.packages("gifski")– if statement used to specify a block of code to be executed if a condition is true
# Load the required packages – this is a comment
library(tidyverse)
library(gganimate)
library(gifski)
# Define the Generalized Context Model function- this is a comment
gcm <- function(stimulus, exemplars, weights, c = 1) {     - creates a function
similarities <- exp(-c * colSums((t(exemplars) - stimulus)^2))
numerator <- sum(similarities * weights[, 1])
denominator <- sum(similarities * rowSums(weights))
prob_A <- numerator / denominator
return(prob_A) – lets the function return a result
}
# Generate exemplars – this is a comment
set.seed(123)
n_exemplars <- 20
n_dimensions <- 2
exemplars <- matrix(runif(n_exemplars * n_dimensions), ncol = n_dimensions)
weights <- matrix(runif(n_exemplars * 2), ncol = 2)
weights <- weights / rowSums(weights) # Normalize weights
# Generate grid of stimuli
grid_size <- 50
x <- seq(0, 1, length.out = grid_size)
y <- seq(0, 1, length.out = grid_size)
grid <- expand.grid(x = x, y = y)
# Calculate GCM probabilities for each point in the grid – this is a comment
grid$prob_A <- apply(grid, 1, function(point) gcm(point, exemplars, weights))
# Create a data frame for exemplars with a frame column – this is a comment
exemplar_df <- as.data.frame(exemplars) %>%
mutate(weight = weights[,1],
frame = row_number())
# Create the base plot – this Is a comment
p <- ggplot() +
geom_raster(data = grid, aes(x = x, y = y, fill = prob_A)) +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
geom_point(data = exemplar_df, aes(x = V1, y = V2, size = weight, group = seq_along(frame)), color
= "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Generalized Context Model",
subtitle = "Exemplars shown: {closest_state}",
x = "Dimension 1",
y = "Dimension 2",
fill = "P(A|X)")
# Animate the plot – this is a comment
anim <- p +
transition_states(frame, transition_length = 2, state_length = 1) +
enter_fade() +
exit_fade()
# Render and save the animation – this is a comment
animate(anim,
nframes = n_exemplars * 3, # Increased number of frames for smoother animation
fps = 5, # Adjusted fps for better viewing
width = 800,
height = 600,
renderer = gifski_renderer(),
start_pause = 10,
end_pause = 10)
anim_save("gcm_animated.gif")
cat("Animation completed. Check your working directory for gcm_animated.gif\n")

