library(lobsteR)
library(dplyr)

lobster_login <- account_login(login = Sys.getenv("user"), pwd = Sys.getenv("pwd"))

data_request <- request_query(
  symbol = c("TLT", "SPY"),
  start_date = "2006-06-27",
  end_date = "2025-10-26",
  level = 50)

request_submit(account_login = lobster_login,
               request = data_request)

lobster_archive <- account_archive(account_login = lobster_login)

data_download(
  requested_data = lobster_archive,
  account_login = lobster_login,
  path = "data/lobster-orderbook")
