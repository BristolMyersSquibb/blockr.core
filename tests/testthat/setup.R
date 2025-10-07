trace_observe()

withr::defer(untrace_observe(), teardown_env())
