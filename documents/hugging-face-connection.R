get_available_files <- function(organization, dataset) {
  api_url <- glue::glue(
    "https://huggingface.co/api/datasets/{organization}/{dataset}/tree/main?recursive=1"
  )

  out <- tibble::tibble(NULL)
  next_url <- api_url

  repeat {
    resp <- httr2::request(next_url) |>
      httr2::req_user_agent("httr2") |>
      httr2::req_perform()

    body <- resp |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE) |>
      tibble::tibble() |>
      dplyr::filter(
        type == "file" & grepl("\\.parquet$", path, ignore.case = TRUE)
      ) |>
      dplyr::select(path, size)

    out <- out |> dplyr::bind_rows(tibble::tibble(data = list(body)))
    link <- httr2::resp_headers(resp)$link
    if (is.null(link) || !grepl('rel="next"', link)) {
      break
    }
    next_url <- sub(".*<([^>]+)>; rel=\"next\".*", "\\1", link)
  }

  date_pattern <- "date=([0-9]{4}-[0-9]{2}-[0-9]{2})"

  out |>
    tidyr::unnest(data) |>
    dplyr::transmute(
      date = as.Date(stringr::str_match(path, date_pattern)[, 2]),
      url = glue::glue(
        "https://huggingface.co/datasets/{organization}/{dataset}/resolve/main/{path}"
      )
    )
}

request_data <- function(start_date = "2007-06-27", end_date = "2007-07-27") {
  available_files <- get_available_files("voigtstefan", "sp500")
  tibble::tibble(
    date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  ) |>
    dplyr::inner_join(available_files, by = "date") |>
    dplyr::transmute(
      data = purrr::map(url, ~ arrow::read_parquet(.x))
    ) |>
    tidyr::unnest(data)
}

data <- request_data("2020-01-01", "2020-01-31")
