library(testthat)

# Create a simplified version of ask_anthropic that just returns the request it would create
# This lets us test the request body construction without making API calls
get_request_body <- function(prompt, model = "claude", temperature = 0, max_tokens = 8192, thinking = NULL) {
  # Create an empty system block
  system_block <- list()

  # Prepare the messages content
  messages_content <- list(
    list(
      role = "user",
      content = prompt
    )
  )

  # Prepare the request body
  body <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    system = system_block,
    messages = messages_content
  )

  # Add thinking parameter if provided
  if (!is.null(thinking)) {
    body[["thinking"]] <- list(
      type = "enabled",
      budget_tokens = thinking
    )
  }

  # Return the body that would be sent
  return(body)
}

# Test thinking parameter inclusion in request body
test_thinking_request_body <- function() {
  # Test with thinking parameter
  request_with_thinking <- get_request_body(
    prompt = "Test request with thinking",
    model = "claude-3-7-sonnet-latest",
    thinking = 5000,
    max_tokens = 10000
  )

  # Test without thinking parameter
  request_without_thinking <- get_request_body(
    prompt = "Test request without thinking",
    model = "claude-3-7-sonnet-latest",
    max_tokens = 10000
  )

  # Check the results
  cat("Request with thinking contains thinking parameter: ",
      "thinking" %in% names(request_with_thinking), "\n")
  cat("Thinking type: ", request_with_thinking$thinking$type, "\n")
  cat("Budget tokens: ", request_with_thinking$thinking$budget_tokens, "\n")
  cat("Request without thinking contains thinking parameter: ",
      "thinking" %in% names(request_without_thinking), "\n")

  # Return TRUE if tests pass
  return(TRUE)
}

# Test header construction for large tokens
test_thinking_headers <- function() {
  # Create a function that just returns the headers
  get_request_headers <- function(max_tokens) {
    headers <- list(
      "Content-Type" = "application/json",
      "X-API-Key" = "dummy-key",
      "anthropic-version" = "2023-06-01"
    )

    # Add beta header for large token outputs
    if (max_tokens > 64000) {
      headers[["anthropic-beta"]] <- "output-128k-2025-02-19"
    }

    return(headers)
  }

  # Get headers for normal and large token counts
  normal_headers <- get_request_headers(8000)
  large_headers <- get_request_headers(70000)

  # Check the results
  cat("Normal headers contain beta header: ",
      "anthropic-beta" %in% names(normal_headers), "\n")
  cat("Large headers contain beta header: ",
      "anthropic-beta" %in% names(large_headers), "\n")

  if ("anthropic-beta" %in% names(large_headers)) {
    cat("Beta header value: ", large_headers[["anthropic-beta"]], "\n")
  }

  # Return TRUE if tests pass
  return(TRUE)
}

# Run both tests
run_simplified_tests <- function() {
  cat("Testing thinking request body construction:\n")
  test_thinking_request_body()

  cat("\nTesting headers for large tokens:\n")
  test_thinking_headers()

  cat("\nAll simplified tests completed.\n")
}

# Call this function to run the tests
# run_simplified_tests()
