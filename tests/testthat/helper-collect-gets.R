# Helpers for test-collect-gets.R

# A fake transport with a virtual clock, for deterministic scheduler tests.
# `handler(job)` receives the job list (id, url, type, origin, attempt) and
# returns either list(latency, status, reason, headers, body) for an HTTP
# response or list(latency, error) for a transport-level failure.
new_fake_transport <- function(handler) {
  clock <- 0
  inprog <- new.env(parent = emptyenv())
  log <- new.env(parent = emptyenv())
  log$starts <- list() # list of list(id, origin, attempt, t)
  log$max_inflight <- 0L
  log$origin_inflight <- new.env(parent = emptyenv())
  log$max_origin_inflight <- 0L

  bump_origin <- function(origin, by) {
    cur <- if (exists(origin, log$origin_inflight)) get(origin, log$origin_inflight) else 0L
    cur <- cur + by
    assign(origin, cur, log$origin_inflight)
    if (by > 0L && cur > log$max_origin_inflight) log$max_origin_inflight <- cur
    invisible(cur)
  }

  transport <- list(
    now = function() clock,
    sleep = function(secs) {
      clock <<- clock + secs
    },
    start = function(job, token) {
      out <- handler(job)
      stopifnot(is.list(out), !is.null(out$latency))
      inprog[[as.character(token)]] <- list(
        token = token, origin = job$origin,
        finish = clock + out$latency, out = out
      )
      log$starts[[length(log$starts) + 1L]] <- list(
        id = job$id, origin = job$origin, attempt = job$attempt, t = clock
      )
      if (length(ls(inprog)) > log$max_inflight) log$max_inflight <- length(ls(inprog))
      bump_origin(job$origin, 1L)
      invisible(token)
    },
    poll = function(timeout) {
      toks <- ls(inprog)
      if (length(toks) == 0L) {
        if (is.finite(timeout)) clock <<- clock + timeout
        return(list())
      }
      finishes <- vapply(toks, function(k) inprog[[k]]$finish, numeric(1))
      clock <<- min(clock + timeout, min(finishes))
      due <- toks[finishes <= clock + 1e-9]
      lapply(due, function(k) {
        rec <- inprog[[k]]
        rm(list = k, envir = inprog)
        bump_origin(rec$origin, -1L)
        out <- rec$out
        if (!is.null(out$error)) {
          list(token = rec$token, ok = FALSE, error = out$error)
        } else {
          list(
            token = rec$token, ok = TRUE,
            status = out$status,
            reason = if (is.null(out$reason)) "OK" else out$reason,
            headers = if (is.null(out$headers)) character(0) else out$headers,
            body = if (is.null(out$body)) "" else out$body
          )
        }
      })
    }
  )
  list(transport = transport, log = log, clock = function() clock)
}

# Run collect_run() over a fake scenario. `df` is passed to
# collect_requests(); `handler` scripts the responses.
run_fake <- function(df, type = NULL, handler, max_inflight = 8L,
                     max_inflight_origin = 2L, min_interval = 0,
                     max_attempts = 3L, default_retry_after = 1) {
  jobs <- tsibble:::collect_requests(df, type)
  cfg <- tsibble:::collect_config(
    max_inflight = max_inflight,
    max_inflight_origin = max_inflight_origin,
    min_interval = min_interval,
    max_attempts = max_attempts,
    default_retry_after = default_retry_after
  )
  ft <- new_fake_transport(handler)
  out <- tsibble:::collect_run(jobs, cfg, ft$transport)
  list(values = out$values, states = out$states, log = ft$log, clock = ft$clock())
}

# Start times recorded for one request id, in order.
start_times <- function(log, id) {
  vapply(
    Filter(function(x) x$id == id, log$starts),
    function(x) x$t, numeric(1)
  )
}

# A tiny HTTP/1.1 test server (python3), started on 127.0.0.1 with a random
# port. Returns a list with url() and the server process; the process is
# killed when `env` exits.
local_test_server <- function(env = parent.frame()) {
  py <- Sys.which("python3")
  testthat::skip_if_not(nzchar(py), "python3 is not available")
  testthat::skip_if_not(
    requireNamespace("processx", quietly = TRUE),
    "the processx package is not installed"
  )
  script <- tempfile(fileext = ".py")
  writeLines(test_server_py, script)
  proc <- processx::process$new(py, script, stdout = "|", stderr = "|")
  port <- NULL
  deadline <- Sys.time() + 15
  while (is.null(port) && proc$is_alive() && Sys.time() < deadline) {
    io <- proc$poll_io(500)
    if (isTRUE(io["output"] == "ready")) {
      lines <- proc$read_output_lines(1)
      if (length(lines) >= 1L && grepl("^[0-9]+$", lines[1])) {
        port <- as.integer(lines[1])
      }
    }
  }
  if (is.null(port)) {
    try(proc$kill(), silent = TRUE)
    testthat::skip("the test server did not start")
  }
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(proc$kill(), envir = env)
  }
  list(
    url = function(path) sprintf("http://127.0.0.1:%d%s", port, path),
    proc = proc
  )
}

test_server_py <- '
import threading, time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs
from email.utils import formatdate

state = {}
lock = threading.Lock()

def hits(name):
    with lock:
        n = state.get(name, 0)
        state[name] = n + 1
    return n

class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    def log_message(self, *args):
        pass
    def respond(self, code, body, headers=None):
        data = body.encode("utf-8")
        self.send_response(code)
        for k, v in (headers or {}).items():
            self.send_header(k, v)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)
    def do_GET(self):
        u = urlparse(self.path)
        q = parse_qs(u.query)
        if u.path == "/value":
            self.respond(200, q.get("body", [""])[0])
        elif u.path == "/flaky":
            if hits("flaky") == 0:
                self.respond(429, "slow down", {"Retry-After": "1"})
            else:
                self.respond(200, "7")
        elif u.path == "/flaky-date":
            if hits("flaky-date") == 0:
                self.respond(503, "busy", {"Retry-After": formatdate(time.time() + 2, usegmt=True)})
            else:
                self.respond(200, "8")
        elif u.path == "/status":
            self.respond(int(q["code"][0]), q.get("body", [""])[0])
        elif u.path == "/slow":
            time.sleep(float(q.get("secs", ["1"])[0]))
            self.respond(200, "9")
        else:
            self.respond(404, "not found")

server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
'
