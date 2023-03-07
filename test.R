library(bigrquery)
library(dplyr)
library(lubridate)

projectid <- "bionic-union-378011"

# Create connection to bigquery
con <- dbConnect(
  bigquery(),
  project = "firebase-public-project",
  dataset = "analytics_153293282",
  billing = projectid,
  bigint = "numeric"
)

# Create an object to reference events data
events <- tbl(src = con, from = "events_*")


counts <- select(events, user_pseudo_id) %>%
  summarise(user_count = n_distinct(user_pseudo_id),
            even_count = n()) %>%
  collect()

counts

user_engagement <- events %>%
  filter(event_name == "user_engagement") %>%
  group_by(user_pseudo_id) %>%
  summarize(user_first_engagement = min(event_timestamp),
            user_last_engagement = max(event_timestamp)) %>% 
  group_by(user_pseudo_id, user_first_engagement, user_last_engagement) %>% 
  select(user_pseudo_id, user_first_engagement, user_last_engagement) %>% 
  collect()


user_engagement$month <- month(as.POSIXct(user_engagement$user_first_engagement/1000000, origin = "1970-01-01"))
user_engagement$julianday <- yday(as.POSIXct(user_engagement$user_first_engagement/1000000, origin = "1970-01-01"))
user_engagement$dayofweek <- wday(as.POSIXct(user_engagement$user_first_engagement/1000000, origin = "1970-01-01"))
user_engagement$ts_24hr_after_first_engagement = user_engagement$user_first_engagement + 86400000000
user_engagement$churned = ifelse(user_engagement$user_last_engagement < (user_engagement$user_first_engagement + 86400000000), 1, 0)
user_engagement$bounced = ifelse(user_engagement$user_last_engagement <= (user_engagement$user_first_engagement + 600000000), 1, 0)

sum(user_engagement$bounced)
sum(user_engagement$churned)


# user_demographics <- events %>%
#   filter(event_name == "user_engagement") %>%
#   group_by(user_pseudo_id) %>% 
#   filter(row_num() == 1) %>% 
#   select(-row_num, country = geo.country, operating_system = device, language = device.language) %>%
#   show_query()

sql <- "WITH first_values AS (SELECT
          user_pseudo_id,
          geo.country as country,
          device.operating_system as operating_system,
          device.language as language,
          ROW_NUMBER() OVER (PARTITION BY user_pseudo_id ORDER BY event_timestamp DESC) AS row_num
      FROM `firebase-public-project.analytics_153293282.events_*`
      WHERE event_name='user_engagement')
      SELECT * EXCEPT (row_num)
      FROM first_values
      WHERE row_num = 1"

query <- bq_project_query(projectid, query = sql)

user_demographics <- bq_table_download(query, bigint = "numeric")






