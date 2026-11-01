# The decoding and encoding that R applies to the messages of the wire protocol
# between the client and R, as inst/protocol/README.md gives them.

protocol_version <- function() "1.0"

# Decodes a message that the client sent, as its JSON text, into plain values
# only: an array of one type becomes an atomic vector, an array of several types
# a list, an object a named list, and a string stays a string, a 64-bit integer
# is read as a double, and no array of objects becomes a data frame and no array
# of arrays a matrix, so that nothing becomes code by being decoded.
decode_message <- function(x) {
  yyjsonr::read_json_str(
    x,
    opts = yyjsonr::opts_read_json(
      obj_of_arrs_to_df = FALSE,
      arr_of_objs_to_df = FALSE,
      arr_of_arrs_to_matrix = FALSE,
      int64 = "double",
      num_specials = "string"
    )
  )
}

# Encodes a message that R sends, an unnamed list of replies and progress notes,
# with every vector of length one written bare, so that a value of length one
# that has to stay an array is marked with `I()`, an object is a named list, and
# an empty one a named empty list. A string of class `json`, such as one that
# `encode_answer()` returns, goes in as it is. The result is such a string too,
# which Shiny's `session$sendCustomMessage()` sends without encoding it again.
encode_message <- function(x) {
  json_str(x, auto_unbox = TRUE)
}

# Encodes a request's answer, which no schema describes, with every vector
# written as an array, one of length one included, for `encode_message()` to
# put in a reply as it is.
encode_answer <- function(x) {
  json_str(x, auto_unbox = FALSE)
}

json_str <- function(x, auto_unbox) {
  structure(
    yyjsonr::write_json_str(
      x,
      opts = yyjsonr::opts_write_json(
        auto_unbox = auto_unbox,
        json_verbatim = TRUE
      )
    ),
    class = "json"
  )
}
