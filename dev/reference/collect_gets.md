# Collect a fixed list of GET requests

`collect_gets()` performs a fixed list of HTTP `GET` requests
concurrently, respecting global and per-origin rate limits. It is
designed for data platforms that collect from many sites at once:
rate-limit waiting on one site never blocks requests to other, ready
sites.

## Usage

``` r
collect_gets(
  requests,
  type = NULL,
  max_inflight = 8L,
  max_inflight_origin = 2L,
  min_interval = 0,
  max_attempts = 3L,
  timeout = 30,
  default_retry_after = 1,
  user_agent = "tsibble (https://tsibble.tidyverts.org)"
)
```

## Arguments

- requests:

  A data frame with columns `id` (unique request identifiers), `url`
  (URLs to GET) and optionally `type` (declared response types); or a
  character vector of URLs, named with the request identifiers
  (positions are used as identifiers when unnamed).

- type:

  The declared response type: `"character"`, `"double"` (or
  `"numeric"`), `"integer"` or `"logical"`; a single value used for
  every request, or one value per request. A `type` column of `requests`
  takes precedence over this argument. Bodies are parsed strictly:
  `"double"` accepts decimal/scientific notation plus `NA`, `NaN` and
  `Inf`; `"integer"` accepts whole numbers in the integer range plus
  `NA`; `"logical"` accepts `TRUE`/`FALSE`/`T`/`F` (any case) plus `NA`;
  `"character"` takes the body as is.

- max_inflight:

  Maximum number of requests in flight at once, across all origins.

- max_inflight_origin:

  Maximum number of requests in flight at once per origin.

- min_interval:

  Minimum number of seconds between consecutive request starts to the
  same origin.

- max_attempts:

  Maximum number of attempts per request, including the first one.

- timeout:

  Per-attempt timeout in seconds.

- default_retry_after:

  Seconds to pause an origin for when a `429`/`503` response carries no
  usable `Retry-After` header.

- user_agent:

  The `User-Agent` header sent with every request.

## Value

An object of class `collect_gets_result`: a list with components

- `values`:

  The parsed response values, in input order and named with the request
  identifiers. An atomic vector of the declared type when every request
  declares the same type, otherwise a list of typed values. Failed
  requests are `NA` here; see `requests$ok`.

- `requests`:

  A data frame holding exactly one terminal state per request
  identifier: `id`, `ok` (`TRUE` when the request succeeded), `status`
  (the HTTP status code, `NA` when no response was received), `reason`
  (the HTTP reason phrase, or a description of the transport or response
  type error), `attempts`, `origin` and `url`.

## Details

`collect_gets()` paces and retries the requests according to the rules
below, and always returns one terminal state per request identifier.

## Scheduling

- At most `max_inflight` requests are in flight at any time across all
  origins, and at most `max_inflight_origin` per origin (an origin is
  the scheme, host and port of a URL).

- Consecutive request starts to the same origin are spaced by at least
  `min_interval` seconds.

- A `429` (Too Many Requests) or `503` (Service Unavailable) response
  pauses its origin until the time given by the `Retry-After` response
  header, which may be delta-seconds or an HTTP-date
  (`default_retry_after` seconds is used when the header is missing or
  cannot be parsed). The pause applies to every subsequent request to
  that origin, first attempts and retries alike, and a new `Retry-After`
  can only extend an existing pause, never shorten it.

- Waiting requests hold no in-flight slot: while one origin is paused or
  throttled, requests to other ready origins continue.

## Retries and terminal states

Requests are retried after `429`/`503` responses and after transport
errors (e.g. timeouts or connection failures), up to `max_attempts`
attempts in total. Other statuses — including redirects, which are not
followed — are terminal immediately. Every request identifier ends with
exactly one terminal state, reported in the `requests` component of the
result.

## Results

Successful (`2xx`) responses are parsed into the declared `type` and
composed, in input order and keyed by `id`, into the native `values`
vector. Failed requests keep their HTTP status and reason in `requests`
and are `NA` in `values`; use `requests$ok` to tell a failed request
apart from a legitimate `NA`, `0`, `FALSE` or `""` value. A response
body that cannot be parsed as the declared type is a (terminal) response
type error, reported in `requests$reason`.

## Examples

``` r
if (FALSE) { # \dontrun{
reqs <- data.frame(
  id = c("cats", "dogs"),
  url = c("https://example.com/api/cats", "https://example.org/api/dogs"),
  type = "double"
)
res <- collect_gets(reqs)
res$values
res$requests
} # }
```
