test_that("successful responses are parsed into the declared native type", {
  df <- data.frame(
    id = c("a", "b", "c"),
    url = paste0("http://a.test/", 1:3),
    stringsAsFactors = FALSE
  )
  bodies <- c(a = "1", b = "2", c = "3")
  latencies <- c(a = 0.3, b = 0.2, c = 0.1) # complete out of order
  res <- run_fake(df, "integer", function(job) {
    list(latency = latencies[[job$id]], status = 200L, body = bodies[[job$id]])
  })
  expect_true(all(res$states$ok))
  # native atomic vector, in input order, keyed by id
  expect_identical(res$values, c(a = 1L, b = 2L, c = 3L))
  expect_identical(res$states$attempts, c(1L, 1L, 1L))
  expect_identical(res$states$status, c(200L, 200L, 200L))
})

test_that("mixed declared types compose into a list of typed values", {
  df <- data.frame(
    id = c("i", "d", "l", "c"),
    url = paste0("http://a.test/", 1:4),
    stringsAsFactors = FALSE
  )
  types <- c("integer", "double", "logical", "character")
  bodies <- c(i = "1", d = "2.5", l = "TRUE", c = "hello")
  res <- run_fake(df, types, function(job) {
    list(latency = 0, status = 200L, body = bodies[[job$id]])
  })
  expect_identical(
    res$values,
    list(i = 1L, d = 2.5, l = TRUE, c = "hello")
  )
})

test_that("failures are not confused with NA, 0, FALSE or empty string", {
  df <- data.frame(
    id = c("na", "zero", "no", "empty", "missing"),
    url = paste0("http://a.test/", 1:5),
    stringsAsFactors = FALSE
  )
  types <- c("integer", "integer", "logical", "character", "integer")
  res <- run_fake(df, types, function(job) {
    switch(job$id,
      na = list(latency = 0, status = 200L, body = "NA"),
      zero = list(latency = 0, status = 200L, body = "0"),
      no = list(latency = 0, status = 200L, body = "FALSE"),
      empty = list(latency = 0, status = 200L, body = ""),
      missing = list(latency = 0, status = 404L, reason = "Not Found", body = "nope")
    )
  })
  expect_identical(res$states$ok, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  # legitimate values, including NA/0/FALSE/"", are kept as successes
  expect_identical(res$values$na, NA_integer_)
  expect_identical(res$values$zero, 0L)
  expect_identical(res$values$no, FALSE)
  expect_identical(res$values$empty, "")
  # the failure keeps its response status and reason
  failed <- res$states[res$states$id == "missing", ]
  expect_identical(failed$status, 404L)
  expect_identical(failed$reason, "Not Found")
  expect_identical(res$values$missing, NA_integer_)
})

test_that("response type errors are explicit", {
  df <- data.frame(
    id = c("word", "fraction", "overflow", "empty"),
    url = paste0("http://a.test/", 1:4),
    stringsAsFactors = FALSE
  )
  bodies <- c(word = "abc", fraction = "1.5", overflow = "99999999999", empty = "")
  res <- run_fake(df, "integer", function(job) {
    list(latency = 0, status = 200L, body = bodies[[job$id]])
  })
  expect_true(all(!res$states$ok))
  expect_identical(res$states$status, rep(200L, 4L))
  expect_match(res$states$reason[1], 'cannot parse response body as "integer"')
  expect_match(res$states$reason[1], '"abc"', fixed = TRUE)
  expect_true(all(is.na(res$values)))
})

test_that("429 Retry-After pauses all later first attempts and retries of the origin", {
  df <- data.frame(
    id = c("a1", "a2", "b1"),
    url = c("http://a.test/1", "http://a.test/2", "http://b.test/1"),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", max_inflight_origin = 1L, handler = function(job) {
    if (job$id == "a1" && job$attempt == 1L) {
      list(latency = 0, status = 429L, reason = "Too Many Requests",
           headers = c("Retry-After" = "2"), body = "slow down")
    } else {
      bodies <- c(a1 = "5", a2 = "6", b1 = "7")
      list(latency = 0, status = 200L, body = bodies[[job$id]])
    }
  })
  expect_true(all(res$states$ok))
  expect_identical(res$values, c(a1 = 5L, a2 = 6L, b1 = 7L))
  expect_identical(res$states$attempts, c(2L, 1L, 1L))
  # the retry of a1 and the first attempt of a2 both wait for the pause
  expect_equal(start_times(res$log, "a1"), c(0, 2))
  expect_equal(start_times(res$log, "a2"), 2)
  # the other origin is not held back
  expect_equal(start_times(res$log, "b1"), 0)
  expect_equal(res$clock, 2)
})

test_that("Retry-After accepts HTTP dates and falls back for bad values", {
  df <- data.frame(
    id = c("date", "past", "garbage"),
    url = c("http://a.test/1", "http://b.test/1", "http://c.test/1"),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", default_retry_after = 0.5, handler = function(job) {
    if (job$attempt > 1L) return(list(latency = 0, status = 200L, body = "1"))
    header <- switch(job$id,
      date = "Thu, 01 Jan 1970 00:00:05 GMT", # 5s after the (virtual) epoch
      past = "Wed, 01 Jan 1969 00:00:00 GMT", # in the past: no wait
      garbage = "not a retry-after"
    )
    list(latency = 0, status = 503L, reason = "Service Unavailable",
         headers = c("Retry-After" = header), body = "busy")
  })
  expect_true(all(res$states$ok))
  expect_equal(start_times(res$log, "date"), c(0, 5))
  expect_equal(start_times(res$log, "past"), c(0, 0))
  expect_equal(start_times(res$log, "garbage"), c(0, 0.5))
})

test_that("a Retry-After pause is never shortened by a later response", {
  df <- data.frame(
    id = c("a1", "a2", "a3"),
    url = paste0("http://a.test/", 1:3),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", max_inflight_origin = 2L, handler = function(job) {
    if (job$attempt > 1L) return(list(latency = 0, status = 200L, body = "1"))
    if (job$id == "a1") { # pauses the origin for 10s from t = 0.2
      list(latency = 0.2, status = 429L, reason = "Too Many Requests",
           headers = c("Retry-After" = "10"), body = "")
    } else if (job$id == "a2") { # a later, shorter pause must not win
      list(latency = 0.4, status = 429L, reason = "Too Many Requests",
           headers = c("Retry-After" = "1"), body = "")
    } else {
      list(latency = 0, status = 200L, body = "3")
    }
  })
  expect_true(all(res$states$ok))
  # nothing on this origin restarts before t = 10.2
  expect_equal(start_times(res$log, "a1"), c(0, 10.2))
  expect_equal(start_times(res$log, "a2"), c(0, 10.2))
  expect_equal(start_times(res$log, "a3"), 10.2)
  expect_equal(res$clock, 10.2)
})

test_that("waiting holds no in-flight slot and other origins continue", {
  df <- data.frame(
    id = c("a1", "b1", "b2"),
    url = c("http://a.test/1", "http://b.test/1", "http://b.test/2"),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", max_inflight = 2L, handler = function(job) {
    if (job$id == "a1" && job$attempt == 1L) {
      list(latency = 0, status = 429L, reason = "Too Many Requests",
           headers = c("Retry-After" = "5"), body = "")
    } else if (job$id == "a1") {
      list(latency = 0, status = 200L, body = "1")
    } else {
      list(latency = 1, status = 200L, body = "2")
    }
  })
  expect_true(all(res$states$ok))
  # b2 starts immediately: a1's pause frees its slot instead of holding it
  expect_equal(start_times(res$log, "b1"), 0)
  expect_equal(start_times(res$log, "b2"), 0)
  expect_equal(start_times(res$log, "a1"), c(0, 5))
  expect_lte(res$log$max_inflight, 2L)
  expect_equal(res$clock, 5)
})

test_that("global and per-origin in-flight limits are respected", {
  df <- data.frame(
    id = c(paste0("a", 1:3), paste0("b", 1:3)),
    url = c(paste0("http://a.test/", 1:3), paste0("http://b.test/", 1:3)),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", max_inflight = 3L, max_inflight_origin = 2L,
                  handler = function(job) list(latency = 1, status = 200L, body = "1"))
  expect_true(all(res$states$ok))
  expect_lte(res$log$max_inflight, 3L)
  expect_lte(res$log$max_origin_inflight, 2L)
  expect_equal(res$clock, 2) # two waves of three
})

test_that("request starts to one origin are spaced by min_interval", {
  df <- data.frame(
    id = paste0("a", 1:3),
    url = paste0("http://a.test/", 1:3),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", min_interval = 2,
                  handler = function(job) list(latency = 0, status = 200L, body = "1"))
  expect_true(all(res$states$ok))
  expect_equal(start_times(res$log, "a1"), 0)
  expect_equal(start_times(res$log, "a2"), 2)
  expect_equal(start_times(res$log, "a3"), 4)
})

test_that("attempts are bounded and every id gets exactly one terminal state", {
  df <- data.frame(
    id = c("limited", "flaky", "down"),
    url = c("http://a.test/1", "http://b.test/1", "http://c.test/1"),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", max_attempts = 2L, handler = function(job) {
    switch(job$id,
      limited = list(latency = 0, status = 429L, reason = "Too Many Requests",
                     headers = c("Retry-After" = "0"), body = ""),
      flaky = if (job$attempt < 2L) {
        list(latency = 0, error = "connection reset")
      } else {
        list(latency = 0, status = 200L, body = "3")
      },
      down = list(latency = 0, error = "could not connect")
    )
  })
  # one terminal state per request identifier
  expect_identical(sort(res$states$id), sort(df$id))
  expect_identical(res$states$ok, c(FALSE, TRUE, FALSE))
  limited <- res$states[res$states$id == "limited", ]
  expect_identical(limited$attempts, 2L)
  expect_identical(limited$status, 429L)
  expect_identical(limited$reason, "Too Many Requests")
  flaky <- res$states[res$states$id == "flaky", ]
  expect_identical(flaky$attempts, 2L)
  expect_identical(res$values[["flaky"]], 3L)
  down <- res$states[res$states$id == "down", ]
  expect_identical(down$attempts, 2L)
  expect_identical(down$status, NA_integer_)
  expect_match(down$reason, "transport error: could not connect", fixed = TRUE)
  expect_identical(res$values[["down"]], NA_integer_)
})

test_that("duplicate identifiers, invalid config and bad input are explicit errors", {
  df <- data.frame(id = c("a", "b"), url = c("http://a.test/1", "http://b.test/1"),
                   stringsAsFactors = FALSE)
  dup <- data.frame(id = c("a", "a"), url = c("http://a.test/1", "http://a.test/2"),
                    stringsAsFactors = FALSE)
  expect_error(collect_gets(dup, "integer"), "Duplicate request identifiers")
  expect_error(collect_gets(c(a = "http://a.test/1", a = "http://a.test/2"), "integer"),
               "Duplicate request identifiers")
  expect_error(collect_gets(data.frame(id = "a", url = "notaurl"), "integer"),
               "Invalid URL")
  expect_error(collect_gets(data.frame(id = "a", url = "ftp://a.test/1"), "integer"),
               '"http" or "https"')
  expect_error(collect_gets(df, "factor"), "`type` must be one of")
  expect_error(collect_gets(df), "Declare the response `type`")
  expect_error(collect_gets(df, "integer", max_inflight = 0), "`max_inflight`")
  expect_error(collect_gets(df, "integer", max_inflight = 1.5), "`max_inflight`")
  expect_error(collect_gets(df, "integer", max_inflight_origin = 0), "`max_inflight_origin`")
  expect_error(collect_gets(df, "integer", min_interval = -1), "`min_interval`")
  expect_error(collect_gets(df, "integer", max_attempts = 0), "`max_attempts`")
  expect_error(collect_gets(df, "integer", default_retry_after = -1), "`default_retry_after`")
  expect_error(collect_gets(df, "integer", timeout = 0), "`timeout`")
  expect_error(collect_gets(df, "integer", timeout = NA), "`timeout`")
  expect_error(collect_gets(42, "integer"), "must be a data frame or a character vector")
  expect_error(collect_gets(data.frame(id = "a"), "integer"), "missing required column")
  expect_error(collect_gets(data.frame(id = NA, url = "http://a.test/1"), "integer"),
               "must not contain missing")
})

test_that("character vector input and origin derivation work", {
  jobs <- tsibble:::collect_requests(
    c(x = "http://a.test:8080/1", y = "https://a.test/2", z = "http://a.test/1"),
    "integer"
  )
  expect_identical(jobs$id, c("x", "y", "z"))
  expect_identical(
    jobs$origin,
    c("http://a.test:8080", "https://a.test", "http://a.test")
  )
  jobs <- tsibble:::collect_requests(c("http://a.test/1", "http://a.test/2"), "double")
  expect_identical(jobs$id, c("1", "2"))
  expect_identical(jobs$type, c("double", "double"))
})

test_that("empty request lists collect to an empty result", {
  res <- collect_gets(data.frame(
    id = character(0), url = character(0), type = character(0),
    stringsAsFactors = FALSE
  ))
  expect_s3_class(res, "collect_gets_result")
  expect_identical(nrow(res$requests), 0L)
  expect_length(res$values, 0L)
})

test_that("the result prints a summary", {
  df <- data.frame(
    id = c("a", "b"),
    url = c("http://a.test/1", "http://a.test/2"),
    stringsAsFactors = FALSE
  )
  res <- run_fake(df, "integer", handler = function(job) {
    if (job$id == "a") {
      list(latency = 0, status = 200L, body = "1")
    } else {
      list(latency = 0, status = 404L, reason = "Not Found", body = "")
    }
  })
  out <- tsibble:::new_collect_result(res$values, res$states)
  expect_output(print(out), "2 requests, 1 succeeded, 1 failed")
  expect_output(print(out), "Not Found")
})

test_that("HTTP-date parsing handles all three formats", {
  expect_equal(
    tsibble:::parse_http_date("Sun, 06 Nov 1994 08:49:37 GMT"),
    as.numeric(as.POSIXct("1994-11-06 08:49:37", tz = "GMT"))
  )
  expect_equal(
    tsibble:::parse_http_date("Sunday, 06-Nov-94 08:49:37 GMT"),
    as.numeric(as.POSIXct("1994-11-06 08:49:37", tz = "GMT"))
  )
  expect_equal(
    tsibble:::parse_http_date("Sun Nov  6 08:49:37 1994"),
    as.numeric(as.POSIXct("1994-11-06 08:49:37", tz = "GMT"))
  )
  expect_identical(tsibble:::parse_http_date("not a date"), NA_real_)
})

test_that("response headers are split into reason phrase and fields", {
  raw <- charToRaw(paste0(
    "HTTP/1.1 429 Too Many Requests\r\n",
    "Retry-After: 5\r\nContent-Type: text/plain\r\n\r\n"
  ))
  head <- tsibble:::split_response_headers(raw)
  expect_identical(head$reason, "Too Many Requests")
  expect_identical(head$headers[["Retry-After"]], "5")
  expect_identical(head$headers[["Content-Type"]], "text/plain")
})

test_that("collect_gets() collects from a real HTTP server", {
  skip_on_cran()
  srv <- local_test_server()
  df <- data.frame(
    id = c("one", "two", "truth", "text"),
    url = c(srv$url("/value?body=1"), srv$url("/value?body=2"),
            srv$url("/value?body=TRUE"), srv$url("/value?body=hello%20world")),
    stringsAsFactors = FALSE
  )
  types <- c("integer", "integer", "logical", "character")
  res <- collect_gets(df, types)
  expect_identical(res$requests$ok, rep(TRUE, 4L))
  expect_identical(res$requests$status, rep(200L, 4L))
  expect_identical(res$values$one, 1L)
  expect_identical(res$values$two, 2L)
  expect_identical(res$values$truth, TRUE)
  expect_identical(res$values$text, "hello world")
})

test_that("429/503 Retry-After pauses are honoured against a real server", {
  skip_on_cran()
  srv <- local_test_server()
  df <- data.frame(
    id = c("seconds", "date"),
    url = c(srv$url("/flaky"), srv$url("/flaky-date")),
    stringsAsFactors = FALSE
  )
  start <- Sys.time()
  res <- collect_gets(df, "integer")
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_identical(res$requests$ok, c(TRUE, TRUE))
  expect_identical(res$requests$attempts, c(2L, 2L))
  expect_identical(res$values, c(seconds = 7L, date = 8L))
  # the HTTP-date pause asks for ~2 seconds (dates have 1-second granularity,
  # so the effective wait may be trimmed to just over 1 second)
  expect_gte(elapsed, 1)
})

test_that("HTTP errors and timeouts get terminal states against a real server", {
  skip_on_cran()
  srv <- local_test_server()
  df <- data.frame(
    id = c("missing", "slow"),
    url = c(srv$url("/status?code=404"), srv$url("/slow?secs=2")),
    stringsAsFactors = FALSE
  )
  res <- collect_gets(df, "integer", timeout = 0.5, max_attempts = 2L)
  missing <- res$requests[res$requests$id == "missing", ]
  expect_identical(missing$ok, FALSE)
  expect_identical(missing$status, 404L)
  expect_identical(missing$reason, "Not Found")
  expect_identical(missing$attempts, 1L) # 404 is terminal, not retried
  slow <- res$requests[res$requests$id == "slow", ]
  expect_identical(slow$ok, FALSE)
  expect_identical(slow$status, NA_integer_)
  expect_identical(slow$attempts, 2L)
  expect_match(slow$reason, "transport error")
  expect_true(all(is.na(res$values)))
})
