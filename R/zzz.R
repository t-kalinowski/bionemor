.onLoad <- function(...) {
  S7::methods_register()
}

.onUnload <- function(...) {
  hook <- get0("S7_on_unload", envir = asNamespace("S7"), inherits = FALSE)
  if (is.function(hook)) {
    hook()
  }
}

local({
  hook <- get0("S7_on_build", envir = asNamespace("S7"), inherits = FALSE)
  if (is.function(hook)) {
    hook()
  }
})
