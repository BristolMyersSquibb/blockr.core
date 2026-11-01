# bquoted_exprs print as the call they hold

    Code
      print(bbquote(utils::head(.(data), n = 10L)))
    Output
      <bquoted_expr>
      utils::head(.(data), n = 10L)

