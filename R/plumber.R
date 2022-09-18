#* @filter cors
function(res) {
  plumber::forward()
}

#* @serializer unboxedJSON
#* @get /hello
function() {
  status <- 200
  message <- "Hello, Plumber!"

  list(
    status = status,
    data = list(
      message = message
    )
  )
}