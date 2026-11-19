#' Collect a fixed list of GET requests
#'
#' `collect_gets()` performs a fixed list of HTTP `GET` requests concurrently,
#' respecting global and per-origin rate limits. It is designed for data
#' platforms that collect from many sites at once: rate-limit waiting on one
#' site never blocks requests to other, ready sites.
#'
#' @details
#' `collect_gets()` paces and retries the requests according to the rules
#' below, and always returns one terminal state per request identifier.
#'
#' @section Scheduling:
#' * At most `max_inflight` requests are in flight at any time across all
#'   origins, and at most `max_inflight_origin` per origin (an origin is the
#'   scheme, host and port of a URL).
#' * Consecutive request starts to the same origin are spaced by at least
#'   `min_interval` seconds.
#' * A `429` (Too Many Requests) or `503` (Service Unavailable) response
#'   pauses its origin until the time given by the `Retry-After` response
#'   header, which may be delta-seconds or an HTTP-date (`default_retry_after`
#'   seconds is used when the header is missing or cannot be parsed). The
#'   pause applies to every subsequent request to that origin, first attempts
#'   and retries alike, and a new `Retry-After` can only extend an existing
#'   pause, never shorten it.
#' * Waiting requests hold no in-flight slot: while one origin is paused or
#'   throttled, requests to other ready origins continue.
#'
#' @section Retries and terminal states:
#' Requests are retried after `429`/`503` responses and after transport
#' errors (e.g. timeouts or connection failures), up to `max_attempts`
#' attempts in total. Other statuses — including redirects, which are not
#' followed — are terminal immediately.
#' Every request identifier ends with exactly one terminal state, reported in
#' the `requests` component of the result.
#'
#' @section Results:
#' Successful (`2xx`) responses are parsed into the declared `type` and
#' composed, in input order and keyed by `id`, into the native `values`
#' vector. Failed requests keep their HTTP status and reason in `requests`
#' and are `NA` in `values`; use `requests$ok` to tell a failed request apart
#' from a legitimate `NA`, `0`, `FALSE` or `""` value. A response body that
#' cannot be parsed as the declared type is a (terminal) response type error,
#' reported in `requests$reason`.
#'
#' @param requests A data frame with columns `id` (unique request
#'   identifiers), `url` (URLs to GET) and optionally `type` (declared
#'   response types); or a character vector of URLs, named with the request
#'   identifiers (positions are used as identifiers when unnamed).
#' @param type The declared response type: `"character"`, `"double"` (or
#'   `"numeric"`), `"integer"` or `"logical"`; a single value used for every
#'   request, or one value per request. A `type` column of `requests` takes
#'   precedence over this argument. Bodies are parsed strictly: `"double"`
#'   accepts decimal/scientific notation plus `NA`, `NaN` and `Inf`; `"integer"`
#'   accepts whole numbers in the integer range plus `NA`; `"logical"` accepts
#'   `TRUE`/`FALSE`/`T`/`F` (any case) plus `NA`; `"character"` takes the body
#'   as is.
#' @param max_inflight Maximum number of requests in flight at once, across
#'   all origins.
#' @param max_inflight_origin Maximum number of requests in flight at once
#'   per origin.
#' @param min_interval Minimum number of seconds between consecutive request
#'   starts to the same origin.
#' @param max_attempts Maximum number of attempts per request, including the
#'   first one.
#' @param timeout Per-attempt timeout in seconds.
#' @param default_retry_after Seconds to pause an origin for when a
#'   `429`/`503` response carries no usable `Retry-After` header.
#' @param user_agent The `User-Agent` header sent with every request.
#'
#' @return An object of class `collect_gets_result`: a list with components
#' \describe{
#'   \item{`values`}{The parsed response values, in input order and named
#'     with the request identifiers. An atomic vector of the declared type
#'     when every request declares the same type, otherwise a list of typed
#'     values. Failed requests are `NA` here; see `requests$ok`.}
#'   \item{`requests`}{A data frame holding exactly one terminal state per
#'     request identifier: `id`, `ok` (`TRUE` when the request succeeded),
#'     `status` (the HTTP status code, `NA` when no response was received),
#'     `reason` (the HTTP reason phrase, or a description of the transport or
#'     response type error), `attempts`, `origin` and `url`.}
#' }
#'
#' @export
#' @importFrom curl curl_parse_url multi_add multi_run new_handle new_pool
#' @examples
#' \dontrun{
#' reqs <- data.frame(
#'   id = c("cats", "dogs"),
#'   url = c("https://example.com/api/cats", "https://example.org/api/dogs"),
#'   type = "double"
#' )
#' res <- collect_gets(reqs)
#' res$values
#' res$requests
#' }
collect_gets <- function(requests, type = NULL, max_inflight = 8L,
                         max_inflight_origin = 2L, min_interval = 0,
                         max_attempts = 3L, timeout = 30,
                         default_retry_after = 1,
                         user_agent = "tsibble (https://tsibble.tidyverts.org)") {
  jobs <- collect_requests(requests, type)
  cfg <- collect_config(
    max_inflight = max_inflight,
    max_inflight_origin = max_inflight_origin,
    min_interval = min_interval,
    max_attempts = max_attempts,
    default_retry_after = default_retry_after
  )
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) {
    abort("`timeout` must be a positive number of seconds.")
  }
  if (!is.character(user_agent) || length(user_agent) != 1L || is.na(user_agent)) {
    abort("`user_agent` must be a single string.")
  }
  transport <- curl_transport(timeout = timeout, user_agent = user_agent)
  out <- collect_run(jobs, cfg, transport)
  new_collect_result(out$values, out$states)
}

# Declared response types
collect_types <- c("character", "double", "integer", "logical")

# Normalise and validate `requests` + `type` into a jobs data frame with
# columns id, url, type, origin (all character, in input order).
collect_requests <- function(requests, type = NULL) {
  if (is.character(requests) && is.null(dim(requests))) {
    ids <- names(requests)
    if (is.null(ids)) ids <- as.character(seq_along(requests))
    requests <- data.frame(id = ids, url = unname(requests), stringsAsFactors = FALSE)
  } else if (is.data.frame(requests)) {
    requests <- as.data.frame(requests)
  } else {
    abort("`requests` must be a data frame or a character vector of URLs.")
  }
  missing_cols <- setdiff(c("id", "url"), names(requests))
  if (length(missing_cols) > 0L) {
    abort(sprintf(
      "`requests` is missing required column(s): %s.",
      paste0("`", missing_cols, "`", collapse = ", ")
    ))
  }
  n <- nrow(requests)

  ids <- requests[["id"]]
  if (is.factor(ids)) ids <- as.character(ids)
  if (!is.atomic(ids) || is.object(ids)) {
    abort("`requests$id` must be an atomic vector of request identifiers.")
  }
  if (anyNA(ids)) abort("`requests$id` must not contain missing values.")
  ids <- as.character(ids)
  if (any(!nzchar(ids))) abort("`requests$id` must not contain empty strings.")
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0L) {
    abort(sprintf(
      "Duplicate request identifiers in `requests$id`: %s.",
      paste0('"', dup, '"', collapse = ", ")
    ))
  }

  urls <- requests[["url"]]
  if (is.factor(urls)) urls <- as.character(urls)
  if (!is.character(urls)) abort("`requests$url` must be a character vector of URLs.")
  if (anyNA(urls) || any(!nzchar(urls))) {
    abort("`requests$url` must not contain missing or empty URLs.")
  }

  types <- requests[["type"]]
  if (is.null(types)) {
    types <- type
    if (is.null(types) && n > 0L) {
      abort(paste0(
        "Declare the response `type` (one of \"character\", \"double\", ",
        "\"integer\", \"logical\"), either as an argument or as a `type` ",
        "column of `requests`."
      ))
    }
  }
  if (is.null(types)) types <- character(0)
  if (!is.character(types) || anyNA(types)) {
    abort("`type` must be a character vector without missing values.")
  }
  if (length(types) == 1L) {
    types <- rep.int(types, n)
  } else if (length(types) != n) {
    abort(sprintf("`type` must have length 1 or %d (the number of requests).", n))
  }
  types[types == "numeric"] <- "double"
  bad <- !types %in% collect_types
  if (any(bad)) {
    abort(sprintf(
      "`type` must be one of %s (found %s).",
      paste0('"', collect_types, '"', collapse = ", "),
      paste0('"', unique(types[bad]), '"', collapse = ", ")
    ))
  }

  origins <- character(n)
  for (i in seq_len(n)) origins[i] <- url_origin(urls[i], ids[i])

  data.frame(
    id = ids, url = urls, type = types, origin = origins,
    stringsAsFactors = FALSE
  )
}

# The origin (scheme + host + optional port) of a URL; aborts on invalid URLs.
url_origin <- function(url, id = NULL) {
  parts <- tryCatch(curl_parse_url(url), error = function(e) NULL)
  where <- if (is.null(id)) "" else sprintf(" (request id \"%s\")", id)
  if (is.null(parts) || is.null(parts$scheme) || is.na(parts$scheme) ||
      is.null(parts$host) || is.na(parts$host) || !nzchar(parts$host)) {
    abort(sprintf("Invalid URL in `requests`%s: %s.", where, url))
  }
  scheme <- tolower(parts$scheme)
  if (!scheme %in% c("http", "https")) {
    abort(sprintf(
      "`requests` URLs must use the \"http\" or \"https\" scheme%s: %s.",
      where, url
    ))
  }
  host <- tolower(parts$host)
  port <- parts$port
  if (is.null(port) || is.na(port) || !nzchar(port)) {
    paste0(scheme, "://", host)
  } else {
    paste0(scheme, "://", host, ":", port)
  }
}

# Validate the rate-limit / retry configuration.
collect_config <- function(max_inflight, max_inflight_origin, min_interval,
                           max_attempts, default_retry_after) {
  list(
    max_inflight = check_whole(max_inflight, "max_inflight", min = 1),
    max_inflight_origin = check_whole(max_inflight_origin, "max_inflight_origin", min = 1),
    min_interval = check_seconds(min_interval, "min_interval"),
    max_attempts = check_whole(max_attempts, "max_attempts", min = 1),
    default_retry_after = check_seconds(default_retry_after, "default_retry_after")
  )
}

check_whole <- function(x, name, min = 0) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < min || x != trunc(x)) {
    abort(sprintf("`%s` must be a whole number of at least %d.", name, min))
  }
  as.integer(x)
}

check_seconds <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    abort(sprintf("`%s` must be a non-negative number of seconds.", name))
  }
  as.numeric(x)
}

# The scheduler. `jobs` comes from collect_requests(), `cfg` from
# collect_config(), and `transport` provides now()/sleep()/start()/poll()
# (see curl_transport()). Waiting requests never occupy in-flight slots, and
# only ready requests are started, so a paused origin never blocks others.
collect_run <- function(jobs, cfg, transport) {
  n <- nrow(jobs)
  attempts <- integer(n)
  not_before <- rep.int(-Inf, n)
  ok <- rep.int(NA, n)
  status <- rep.int(NA_integer_, n)
  reason <- rep.int(NA_character_, n)
  values <- vector("list", n)

  origins <- unique(jobs$origin)
  o_inflight <- rep.int(0L, length(origins))
  names(o_inflight) <- origins
  o_next_start <- rep.int(-Inf, length(origins))
  names(o_next_start) <- origins
  o_paused <- rep.int(-Inf, length(origins))
  names(o_paused) <- origins

  pending <- seq_len(n)
  inflight <- new.env(parent = emptyenv())
  n_inflight <- 0L
  token_counter <- 0L

  finish <- function(i, ok_i, status_i, reason_i, value_i) {
    ok[i] <<- ok_i
    status[i] <<- status_i
    reason[i] <<- reason_i
    values[[i]] <<- value_i
  }

  start_one <- function(i, now) {
    token_counter <<- token_counter + 1L
    token <- token_counter
    attempts[i] <<- attempts[i] + 1L
    job <- list(
      id = jobs$id[i], url = jobs$url[i], type = jobs$type[i],
      origin = jobs$origin[i], attempt = attempts[i]
    )
    err <- tryCatch({
      transport$start(job, token)
      NULL
    }, error = function(e) e)
    if (!is.null(err)) {
      # A request that cannot even be started fails terminally.
      finish(i, FALSE, NA_integer_,
             sprintf("failed to start request: %s", conditionMessage(err)),
             typed_na(jobs$type[i]))
      return(invisible())
    }
    inflight[[as.character(token)]] <- i
    n_inflight <<- n_inflight + 1L
    o <- jobs$origin[i]
    o_inflight[o] <<- o_inflight[o] + 1L
    o_next_start[o] <<- now + cfg$min_interval
    invisible()
  }

  ready <- function(i, now) {
    o <- jobs$origin[i]
    not_before[i] <= now && o_paused[o] <= now && o_next_start[o] <= now &&
      o_inflight[o] < cfg$max_inflight_origin
  }

  handle_event <- function(ev) {
    i <- inflight[[as.character(ev$token)]]
    rm(list = as.character(ev$token), envir = inflight)
    n_inflight <<- n_inflight - 1L
    o <- jobs$origin[i]
    o_inflight[o] <<- o_inflight[o] - 1L
    now <- transport$now()
    type_i <- jobs$type[i]
    if (!isTRUE(ev$ok)) {
      # Transport-level failure (timeout, connection error, ...): retryable.
      if (attempts[i] < cfg$max_attempts) {
        not_before[i] <<- now
        pending <<- c(pending, i)
      } else {
        finish(i, FALSE, NA_integer_,
               sprintf("transport error: %s", ev$error), typed_na(type_i))
      }
      return(invisible())
    }
    st <- ev$status
    if (st >= 200L && st < 300L) {
      parsed <- parse_response(ev$body, type_i)
      if (parsed$ok) {
        finish(i, TRUE, st, ev$reason, parsed$value)
      } else {
        finish(i, FALSE, st, parsed$reason, typed_na(type_i))
      }
    } else if (st == 429L || st == 503L) {
      pause_until <- retry_after_time(ev$headers, now, cfg$default_retry_after)
      # A Retry-After pause can only be extended, never shortened.
      o_paused[o] <<- max(o_paused[o], pause_until)
      if (attempts[i] < cfg$max_attempts) {
        not_before[i] <<- max(not_before[i], pause_until)
        pending <<- c(pending, i)
      } else {
        finish(i, FALSE, st, ev$reason, typed_na(type_i))
      }
    } else {
      finish(i, FALSE, st, ev$reason, typed_na(type_i))
    }
    invisible()
  }

  repeat {
    now <- transport$now()
    # Start every request that is ready and within the limits; requests that
    # are waiting (paused origin, initiation interval, retry delay) are
    # skipped without blocking later, ready requests.
    while (length(pending) > 0L && n_inflight < cfg$max_inflight) {
      pos <- which(vapply(pending, ready, logical(1L), now = now))[1L]
      if (is.na(pos)) break
      i <- pending[pos]
      pending <- pending[-pos]
      start_one(i, now)
    }
    if (length(pending) == 0L && n_inflight == 0L) break
    # Wait for the next completion, or until the earliest waiting request
    # becomes ready, whichever comes first.
    timeout <- Inf
    if (length(pending) > 0L) {
      blockers <- c(
        not_before[pending],
        o_paused[jobs$origin[pending]],
        o_next_start[jobs$origin[pending]]
      )
      future <- blockers[is.finite(blockers) & blockers > now]
      if (length(future) > 0L) timeout <- min(future) - now
    }
    if (n_inflight > 0L) {
      events <- transport$poll(timeout)
    } else {
      if (!is.finite(timeout)) timeout <- 0
      transport$sleep(timeout)
      events <- list()
    }
    for (ev in events) handle_event(ev)
  }

  states <- data.frame(
    id = jobs$id, ok = ok, status = status, reason = reason,
    attempts = attempts, origin = jobs$origin, url = jobs$url,
    stringsAsFactors = FALSE
  )
  if (n > 0L && length(unique(jobs$type)) == 1L) {
    values <- unlist(values, recursive = FALSE, use.names = FALSE)
  }
  names(values) <- jobs$id
  list(values = values, states = states)
}

# A typed missing value for failed requests of a declared type.
typed_na <- function(type) {
  switch(type,
    character = NA_character_,
    double = NA_real_,
    integer = NA_integer_,
    logical = NA,
    NA
  )
}

# Parse a response body into its declared type; returns list(ok, value) or
# list(ok = FALSE, reason) describing the response type error explicitly.
parse_response <- function(body, type) {
  if (type == "character") return(list(ok = TRUE, value = body))
  x <- trimws(body)
  parsed <- switch(type,
    double = parse_scalar_double(x),
    integer = parse_scalar_integer(x),
    logical = parse_scalar_logical(x)
  )
  if (parsed$ok) return(list(ok = TRUE, value = parsed$value))
  list(ok = FALSE, reason = sprintf(
    "cannot parse response body as \"%s\": %s", type, abbreviate_body(body)
  ))
}

parse_scalar_double <- function(x) {
  if (x == "NA") return(list(ok = TRUE, value = NA_real_))
  if (x == "NaN") return(list(ok = TRUE, value = NaN))
  if (x %in% c("Inf", "+Inf", "Infinity", "+Infinity")) {
    return(list(ok = TRUE, value = Inf))
  }
  if (x %in% c("-Inf", "-Infinity")) return(list(ok = TRUE, value = -Inf))
  if (!grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x)) {
    return(list(ok = FALSE))
  }
  list(ok = TRUE, value = as.numeric(x))
}

parse_scalar_integer <- function(x) {
  if (x == "NA") return(list(ok = TRUE, value = NA_integer_))
  if (!grepl("^[+-]?[0-9]+$", x)) return(list(ok = FALSE))
  value <- suppressWarnings(as.integer(x))
  if (is.na(value)) return(list(ok = FALSE)) # outside the integer range
  list(ok = TRUE, value = value)
}

parse_scalar_logical <- function(x) {
  x <- toupper(x)
  if (x %in% c("TRUE", "T")) return(list(ok = TRUE, value = TRUE))
  if (x %in% c("FALSE", "F")) return(list(ok = TRUE, value = FALSE))
  if (x == "NA") return(list(ok = TRUE, value = NA))
  list(ok = FALSE)
}

abbreviate_body <- function(body, max = 40L) {
  x <- gsub("\\s+", " ", body)
  if (nchar(x) > max) x <- paste0(substr(x, 1L, max), "...")
  paste0('"', x, '"')
}

# The absolute time until which an origin must be paused, from a Retry-After
# header given as delta-seconds or as an HTTP-date.
retry_after_time <- function(headers, now, default) {
  value <- headers[!is.na(names(headers)) & tolower(names(headers)) == "retry-after"]
  if (length(value) == 0L) return(now + default)
  value <- trimws(value[[1L]])
  if (grepl("^[0-9]+$", value)) return(now + as.numeric(value))
  date <- parse_http_date(value)
  if (is.na(date)) return(now + default)
  max(now, date)
}

# Parse an HTTP-date (IMF-fixdate, with the obsolete RFC 850 and asctime
# formats as fallbacks); returns seconds since the epoch, or NA_real_.
parse_http_date <- function(x) {
  formats <- c(
    "%a, %d %b %Y %H:%M:%S GMT", # IMF-fixdate, e.g. Sun, 06 Nov 1994 08:49:37 GMT
    "%A, %d-%b-%y %H:%M:%S GMT", # RFC 850, e.g. Sunday, 06-Nov-94 08:49:37 GMT
    "%a %b %e %H:%M:%S %Y" # asctime, e.g. Sun Nov  6 08:49:37 1994
  )
  for (fmt in formats) {
    date <- tryCatch(
      as.POSIXct(x, format = fmt, tz = "GMT"),
      error = function(e) as.POSIXct(NA_real_, tz = "GMT", origin = "1970-01-01")
    )
    if (!is.na(date)) return(as.numeric(date))
  }
  NA_real_
}

# The production transport, backed by curl's multi interface. A request only
# occupies an in-flight slot while its handle is actually running.
curl_transport <- function(timeout, user_agent) {
  pool <- new_pool()
  queue <- new.env(parent = emptyenv())
  queue$events <- list()
  push_event <- function(ev) {
    queue$events[[length(queue$events) + 1L]] <- ev
  }
  timeout_ms <- as.integer(round(timeout * 1000))
  list(
    now = function() as.numeric(Sys.time()),
    sleep = function(secs) Sys.sleep(secs),
    start = function(job, token) {
      handle <- new_handle(
        url = job$url, httpget = TRUE, followlocation = FALSE,
        timeout_ms = timeout_ms, useragent = user_agent
      )
      multi_add(
        handle = handle, pool = pool,
        done = function(res) {
          head <- split_response_headers(res$headers)
          push_event(list(
            token = token, ok = TRUE, status = as.integer(res$status_code),
            reason = head$reason, headers = head$headers,
            body = raw_to_text(res$content)
          ))
        },
        fail = function(error) {
          push_event(list(token = token, ok = FALSE, error = as.character(error)))
        }
      )
      invisible(token)
    },
    poll = function(timeout) {
      if (!is.finite(timeout)) timeout <- 3600
      multi_run(timeout = max(timeout, 0.01), poll = TRUE, pool = pool)
      events <- queue$events
      queue$events <- list()
      events
    }
  )
}

# Split raw response headers into the reason phrase and a named character
# vector of header fields (the last header block, in case of interim 1xx
# blocks).
split_response_headers <- function(raw) {
  empty <- setNames(character(0), character(0))
  lines <- strsplit(raw_to_text(raw), "\r\n|\n")[[1L]]
  is_status <- grepl("^HTTP/", lines)
  if (!any(is_status)) return(list(reason = NA_character_, headers = empty))
  start <- max(which(is_status))
  reason <- sub("^HTTP/\\S+\\s+[0-9]+\\s*", "", lines[start])
  if (!nzchar(reason)) reason <- NA_character_
  fields <- lines[seq.int(start + 1L, length(lines))]
  fields <- fields[nzchar(fields)]
  colon <- regexpr(":", fields, fixed = TRUE)
  fields <- fields[colon > 0L]
  colon <- colon[colon > 0L]
  headers <- trimws(substring(fields, colon + 1L))
  names(headers) <- trimws(substr(fields, 1L, colon - 1L))
  list(reason = reason, headers = headers)
}

raw_to_text <- function(x) {
  if (length(x) == 0L) return("")
  text <- tryCatch(rawToChar(x), error = function(e) "")
  Encoding(text) <- "UTF-8"
  text
}

new_collect_result <- function(values, states) {
  structure(
    list(values = values, requests = states),
    class = "collect_gets_result"
  )
}

#' @export
print.collect_gets_result <- function(x, ...) {
  n <- nrow(x$requests)
  n_ok <- sum(x$requests$ok)
  cat(sprintf(
    "<collect_gets_result: %d request%s, %d succeeded, %d failed>\n",
    n, if (n == 1L) "" else "s", n_ok, n - n_ok
  ))
  print(x$values)
  failed <- x$requests[!x$requests$ok, , drop = FALSE]
  if (nrow(failed) > 0L) {
    cat("\nFailed requests:\n")
    print(failed[, c("id", "status", "reason", "attempts")], row.names = FALSE)
  }
  invisible(x)
}
