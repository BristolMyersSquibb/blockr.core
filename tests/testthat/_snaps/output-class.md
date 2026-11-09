# outputs print their class, properties and body

    Code
      new_block_output(list(text = "Hi", n = 2L), "x_output")
    Output
      <x_output>
      Properties: "text" and "n"
    Code
      new_block_output(list(img = body, width = 1L), "x_output")
    Output
      <x_output>
      Properties: "width"
      Body: image/png, 2 kB
    Code
      body
    Output
      <reply_body> image/png, 2 kB
    Code
      new_reply_body(charToRaw("a,b"), "text/csv", name = "data.csv")
    Output
      <reply_body> text/csv, "data.csv", 3 B

