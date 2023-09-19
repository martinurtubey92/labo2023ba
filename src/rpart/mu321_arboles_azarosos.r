# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol en un subconjunto distinto de atributos del dataset

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

# Bucle para variar hiperparámetros
for (i in 1:nrow(config_table)) { # config_table es la tabla que proporcionaste
    PARAM$rpart_param <- list(
        "cp" = cp_values[i],
        "minsplit" = minsplit_values[i],
        "minbucket" = minbucket_values[i],
        "maxdepth" = maxdepth_values[i]
    )

    PARAM$experimento <- num_trees_list[i] # Actualizar el experimento

    # Crear carpeta para la configuración actual
    carpeta_experimento <- paste0(base_experimento, "KA", PARAM$experimento, "/")
    dir.create(carpeta_experimento, showWarnings = FALSE)
    setwd(carpeta_experimento)


    # que tamanos de ensemble grabo a disco, pero siempre debo generar los 500
    grabar <- c(1, 5, 10, 50, 100, 200, 500)


    # defino los dataset de entrenamiento y aplicacion
    dtrain <- dataset[foto_mes == 202107]
    dapply <- dataset[foto_mes == 202109]

    # aqui se va acumulando la probabilidad del ensemble
    dapply[, prob_acumulada := 0]

    # Establezco cuales son los campos que puedo usar para la prediccion
    # el copy() es por la Lazy Evaluation
    campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



    # Genero las salidas
    set.seed(PARAM$semilla) # Establezco la semilla aleatoria

    for (arbolito in 1:PARAM$num_trees_max) {
        qty_campos_a_utilizar <- as.integer(length(campos_buenos)
        * PARAM$feature_fraction)

        campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

        # paso de un vector a un string con los elementos
        # separados por un signo de "+"
        # este hace falta para la formula
        campos_random <- paste(campos_random, collapse = " + ")

        # armo la formula para rpart
        formulita <- paste0("clase_ternaria ~ ", campos_random)

        # genero el arbol de decision
        modelo <- rpart(formulita,
            data = dtrain,
            xval = 0,
            control = PARAM$rpart_param
        )

        # aplico el modelo a los datos que no tienen clase
        prediccion <- predict(modelo, dapply, type = "prob")

        dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

        if (arbolito %in% grabar) {
            # Genero la entrega para Kaggle
            umbral_corte <- (1 / 40) * arbolito
            entrega <- as.data.table(list(
                "numero_de_cliente" = dapply[, numero_de_cliente],
                "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
            )) # genero la salida

            # Guardar resultados en un archivo único
            nom_arch <- paste0(
                "KA", PARAM$experimento, "_",
                "cp", PARAM$rpart_param$cp, "_",
                "minsplit", PARAM$rpart_param$minsplit, "_",
                "minbucket", PARAM$rpart_param$minbucket, "_",
                "maxdepth", PARAM$rpart_param$maxdepth, "_",
                sprintf("%.3d", arbolito), ".csv"
            )
            fwrite(entrega, file = nom_arch, sep = ",")

            cat(arbolito, " ")

            # Volver al directorio base del experimento
            setwd(carpeta_experimento)
        }
    }
}
