#### Install Spark ----

install.packages("sparklyr")
library(sparklyr)
spark_install(version = "2.4.5")

#### Start Spark Cluster ----

# initialize a list to store spark options in
conf <- list()

# Set the number of cores
conf$`sparklyr.cores.local` <- 4

# Set the memory for the cluster 
conf$`sparklyr.shell.driver-memory` <- "4G"
# Set the fraction of available memory that the clsuter can use
conf$spark.memory.fraction <- 0.9

# Create the cluster object/connection
sc <- spark_connect(master = "local", version = "2.4.5", config = conf)

#### Read Data into the Cluster ----

# Read data directly to the cluster using spark_read_csv
df_s <- spark_read_csv(sc = sc, name = "df_s", path = "path_to_file.csv")

# copy data to the spark cluster
df <- iris
df_s <- copy_to(sc, df, name = "df_s")

#### Manipulate Data in Cluster ----

# You can use dplyr to manipulate a data frame as normal, then collect at the end
library(dplyr)
df_sum <- df_s %>%
  group_by(Species) %>%
  summarize(
    Sepal.Width = mean(Sepal.Width),
    Sepal.Length = mean(Sepal.Length),
    Petal.Length = mean(Petal.Length),
    Petal.Width = mean(Petal.Width)
  ) %>%
  ungroup() %>%
  collect()

df_sub <- df_s %>%
  filter(Species == "setosa") %>%
  mutate(Petal.Dim = Petal.Length * Petal.Width) %>%
  select(-Petal.Length, -Petal.Width) %>%
  collect()

# Note that not all dplyr functions are supported

#### Create Models ----

# It is possible to create a model in Spark, but we prefer the H2O framework
# and use it consistently throughout this repository.
# See https://spark.rstudio.com/mlib/ for more information on creating 
# machine learning models in Spark

#### Close Cluster ----

spark_disconnect()
