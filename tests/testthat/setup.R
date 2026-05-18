trace_observe()
trace_start_span()

withr::defer(untrace_observe(), teardown_env())
withr::defer(untrace_start_span(), teardown_env())
