# Clean up memory
rm(list = ls())
gc()

# Load required libraries
require("data.table")
require("rpart")


# Parameters for the experiment
PARAM <- list()
PARAM$experimento <- 3211

# Set the random seed
PARAM$semilla <- 100057

# Define hyperparameters to test
cp_values <- c(-1, -1, -1, -1, -1, -1, -1)
minsplit_values <- c(50, 100, 250, 500, 750, 750, 1000)
minbucket_values <- c(5, 20, 20, 10, 5, 100, 20)
maxdepth_values <- c(8, 14, 10, 6, 14, 8, 6)

# Define the list of values for the number of trees in the ensemble
num_trees_list <- c(1, 5, 10, 50, 100, 200, 500)

# Create a matrix of hyperparameters to iterate through
hyperparameters <- data.frame(
  cp = cp_values,
  minsplit = minsplit_values,
  minbucket = minbucket_values,
  maxdepth = maxdepth_values
)

# Initialize a counter for ensemble number
ensemble_counter <- 1

# Loop through each combination of hyperparameters and tree options
for (i in 1:nrow(hyperparameters)) {
  # Set rpart parameters based on the current hyperparameters
  PARAM$rpart_param <- list(
    "cp" = hyperparameters$cp[i],
    "minsplit" = hyperparameters$minsplit[i],
    "minbucket" = hyperparameters$minbucket[i],
    "maxdepth" = hyperparameters$maxdepth[i]
  )

  # Loop through each value in the num_trees_list
  for (num_trees in num_trees_list) {
    # Set the number of trees in the ensemble
    PARAM$num_trees_max <- num_trees

    # Change the working directory if needed
    
    setwd("~/buckets/b1/")

    # Load the dataset
    dataset <- fread("./datasets/dataset_pequeno.csv")

    # Create a folder for the experiment
    dir.create(paste0("./exp/KA", PARAM$experimento, "/"), showWarnings = FALSE)
    carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
    dir.create(carpeta_experimento, showWarnings = FALSE)
    setwd(carpeta_experimento)

    # Define training and application datasets
    dtrain <- dataset[foto_mes == 202107]
    dapply <- dataset[foto_mes == 202109]

    # Initialize probability accumulation
    dapply[, prob_acumulada := 0]

    # Set the fields to use for prediction
    campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))

    # Generate the outputs
    set.seed(PARAM$semilla)

    # Initialize entrega here
    entrega <- data.table()

    for (arbolito in 1:PARAM$num_trees_max) {
      qty_campos_a_utilizar <- as.integer(length(campos_buenos) * PARAM$feature_fraction)

      # Ensure qty_campos_a_utilizar is within the valid range
      qty_campos_a_utilizar <- min(qty_campos_a_utilizar, length(campos_buenos))

      campos_random <- sample(campos_buenos, qty_campos_a_utilizar)
      campos_random <- paste(campos_random, collapse = " + ")
      formulita <- paste0("clase_ternaria ~ ", campos_random)

      modelo <- rpart(formulita,
        data = dtrain,
        xval = 0,
        control = PARAM$rpart_param
      )

      prediccion <- predict(modelo, dapply, type = "prob")

      dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]
      
      # Calculate umbral_corte
      umbral_corte <- (1 / 40) * arbolito

      # Create a string representation of the current hyperparameters
      hyperparam_string <- paste("cp", hyperparameters$cp[i], "minsplit", hyperparameters$minsplit[i], "minbucket", hyperparameters$minbucket[i], "maxdepth", hyperparameters$maxdepth[i], sep = "_")

      # Output file name includes tree size, hyperparameters, and ensemble number
      nom_arch <- paste0(
        "KA", PARAM$experimento, "_trees", num_trees, "_", hyperparam_string, "_ensemble", ensemble_counter, "_",
        sprintf("%.3d", arbolito), # for leading zeros
        ".csv"
      )

      entrega <- rbind(entrega, data.table("numero_de_cliente" = dapply[, numero_de_cliente], "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)))

      cat(arbolito, " ")
    }
    
    # Write the file
    fwrite(entrega, file = nom_arch, sep = ",")

    # Increment the ensemble_counter for the next iteration
    ensemble_counter <- ensemble_counter + 1
  }
}
