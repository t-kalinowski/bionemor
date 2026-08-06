.onLoad <- function(...) {
  S7::methods_register()
}

.onUnload <- function(...) {
  if (exists("S7_on_unload", envir = asNamespace("S7"), inherits = FALSE)) {
    S7::S7_on_unload()
  }
}

if (exists("S7_on_build", envir = asNamespace("S7"), inherits = FALSE)) {
  S7::S7_on_build()
}
