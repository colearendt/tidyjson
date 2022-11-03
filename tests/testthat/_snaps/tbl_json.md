# print.tbl_json works for a simple case

    Code
      as.tbl_json("\"a\"")
    Output
      # A tbl_json: 1 x 2 tibble with a "JSON" attribute
        ..JSON  document.id
        <chr>         <int>
      1 "\"a\""           1

# print.tbl_json json.width works correctly

    Code
      print(as.tbl_json("\"12345\""), json.width = 4)
    Output
      # A tbl_json: 1 x 2 tibble with a "JSON" attribute
        ..JSON     document.id
        <chr>            <int>
      1 "\"123..."           1

# print.tbl_json json.n works correctly

    Code
      print(as.tbl_json(c("\"a\"", "\"b\"")), json.n = 1)
    Output
      # A tbl_json: 2 x 2 tibble with a "JSON" attribute
        ..JSON  document.id
        <chr>         <int>
      1 "\"a\""           1
      2 "..."             2

