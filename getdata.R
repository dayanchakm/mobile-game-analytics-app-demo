library(bigrquery)

projectid <- "bionic-union-378011"
sql <- "SELECT * FROM dbt_dakmyradov.train"

query <- bq_project_query(projectid, query = sql)

dt <- bq_table_download(query, bigint = "character")

dt$user_first_engagement <- substr(dt$user_first_engagement,1,nchar(dt$user_first_engagement)-6)

col_names <- colnames(dt)

write.csv(dt, file = "data/data.csv", row.names = FALSE)
write.csv(col_names[-1], file = "data/col_names.csv", row.names = FALSE)

sql <- "SELECT * FROM dbt_dakmyradov.stg_user_in_app_purchase"
query <- bq_project_query(projectid, query = sql)
users_in_app_purchase <- bq_table_download(query)
write.csv(x = users_in_app_purchase, file = "data/users_in_app_purchase.csv")
