library(bigrquery)

projectid <- "bionic-union-378011"
sql <- "SELECT * FROM bqtest.train"


query <- bq_project_query(projectid, query = sql)

dt <- bq_table_download(query, bigint = "numeric")

write.csv(dt, file = "data/data.csv", row.names = FALSE)
