#' Random Forest Cross Validation for penguins data
#'
#' This function does the Cross Validation test for the random forest predict model.
#'
#' @param k Numeric input of number of folds.
#'
#' @keywords prediction
#'
#' @return a numeric of the cross-validation error value.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Remove NA and get variable data
  penguins_data <- na.omit(myProj3::penguins)
  X_data <- penguins_data[, 3:5]

  # Get the Y data
  Y_data <- penguins_data[, 6]
  # random split data
  inds <- sample(rep(1:k, length = nrow(X_data)))
  # Construct the data frame with group
  data <- data.frame("x" = X_data, "y" = Y_data, "split" = inds)

  # k folds Cross Validation test
  err_vec <- c()
  for (i in 1:k) {
    # Construct data
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)

    # Train models
    model <- randomForest(body_mass_g ~ x.bill_length_mm + x.bill_depth_mm + x.flipper_length_mm,
                          data = data_train, ntree = 100)
    # Predict
    pred_value <- predict(model, data_test[, 1:3])
    # Calculate the MSE
    err_vec[i] <- mean((pred_value - data_test[, 4])^2)
  }

  # Calculate the average MSE and return it
  ave_MSE <- mean(err_vec)
  return(ave_MSE)
}
