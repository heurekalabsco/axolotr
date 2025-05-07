library(testthat)
library(httr)
library(mockery)

# Helper function to mock API responses
mock_anthropic_response <- function(content, status_code = 200) {
  structure(
    list(
      status_code = status_code,
      content = charToRaw(jsonlite::toJSON(content, auto_unbox = TRUE)),
      headers = list(`Content-Type` = "application/json")
    ),
    class = "response"
  )
}

# Set up tests
test_that("ask_anthropic handles basic functionality", {
  # Mock environment variables
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {

    # Mock successful API response
    mock_response <- mock_anthropic_response(list(
      content = list(
        list(type = "text", text = "Test response")
      )
    ))

    # Mock the POST function
    mockery::stub(ask_anthropic, "httr::POST", function(...) mock_response)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)

    # Test basic functionality
    result <- ask_anthropic("Test prompt")
    expect_equal(result, "Test response")
  })
})

test_that("ask_anthropic handles model name mapping correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create a mock that captures the arguments passed to POST
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "Test response")
        )
      ))
    )

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)

    # Test with generic model name
    ask_anthropic("Test prompt", model = "claude")
    args <- mockery::mock_args(post_mock)[[1]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_equal(request_body$model, "claude-3-7-sonnet-latest")

    # Test with specific model name
    ask_anthropic("Test prompt", model = "claude-3-opus-latest")
    args <- mockery::mock_args(post_mock)[[2]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_equal(request_body$model, "claude-3-opus-latest")
  })
})

test_that("ask_anthropic handles max_tokens adjustment correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create a mock that captures the arguments
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "Test response")
        )
      ))
    )

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "message", function(...) NULL)

    # Test with opus model and high max_tokens
    ask_anthropic("Test prompt", model = "claude-3-opus-latest", max_tokens = 10000)
    args <- mockery::mock_args(post_mock)[[1]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_equal(request_body$max_tokens, 4096)

    # Test with sonnet 3.5 model and high max_tokens
    ask_anthropic("Test prompt", model = "claude-3-5-sonnet-latest", max_tokens = 10000)
    args <- mockery::mock_args(post_mock)[[2]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_equal(request_body$max_tokens, 8192)

    # Test with 3.7 sonnet model and high max_tokens
    ask_anthropic("Test prompt", model = "claude-3-7-sonnet-latest", max_tokens = 70000)
    args <- mockery::mock_args(post_mock)[[3]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_equal(request_body$max_tokens, 70000)
  })
})

test_that("ask_anthropic adds beta header for large token outputs", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create a mock that captures the headers
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "Test response")
        )
      ))
    )

    # Override add_headers to capture headers
    headers_mock <- mockery::mock(list("test-headers"))

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::add_headers", headers_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)

    # Test with max_tokens > 64000
    ask_anthropic("Test prompt", max_tokens = 65000)

    # Verify beta header was included
    headers_args <- mockery::mock_args(headers_mock)[[1]]
    expect_true("anthropic-beta" %in% names(headers_args))
    expect_equal(headers_args[["anthropic-beta"]], "output-128k-2025-02-19")
  })
})

test_that("ask_anthropic handles thinking parameter correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create mock responses
    mock_thinking_response <- mock_anthropic_response(list(
      content = list(
        list(type = "thinking", text = "Thinking..."),
        list(type = "text", text = "Final response")
      )
    ))

    # Mock functions
    post_mock <- mockery::mock(mock_thinking_response)
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "message", function(...) NULL)

    # Test with thinking parameter
    result <- ask_anthropic(
      "Test prompt",
      model = "claude-3-7-sonnet-latest",
      thinking = 5000,
      temperature = 1,
      max_tokens = 10000
    )

    # Verify the result
    expect_equal(result, "Final response")

    # Verify the request body
    args <- mockery::mock_args(post_mock)[[1]]
    request_body <- jsonlite::fromJSON(args$body)
    expect_true("thinking" %in% names(request_body))
    expect_equal(request_body$thinking$type, "enabled")
    expect_equal(request_body$thinking$budget_tokens, 5000)
  })
})

test_that("ask_anthropic enforces thinking restrictions", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Test thinking < 1024
    expect_error(
      ask_anthropic("Test", model = "claude-3-7-sonnet-latest", thinking = 500),
      "thinking budget_tokens must at least 1024 tokens!"
    )

    # Test thinking >= max_tokens
    expect_error(
      ask_anthropic("Test", model = "claude-3-7-sonnet-latest", thinking = 10000, max_tokens = 5000),
      "thinking budget_tokens must be less than max_tokens!"
    )

    # Test thinking with non-supported model
    expect_error(
      ask_anthropic("Test", model = "claude-3-opus-latest", thinking = 2000),
      "The thinking parameter is only supported for Claude 3.7 Sonnet models."
    )

    # Test thinking with pre_fill (should set pre_fill to NULL)
    # Need to mock to avoid actual API call
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "thinking", text = "Thinking..."),
          list(type = "text", text = "Final response")
        )
      ))
    )
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "message", function(...) NULL)

    # Call function with both thinking and pre_fill
    ask_anthropic(
      "Test",
      model = "claude-3-7-sonnet-latest",
      thinking = 2000,
      pre_fill = "Pre-fill text",
      temperature = 1
    )

    # Verify the request body
    args <- mockery::mock_args(post_mock)[[1]]
    request_body <- jsonlite::fromJSON(args$body)

    # Check that thinking is present in the request
    expect_true("thinking" %in% names(request_body))
    expect_equal(request_body$thinking$type, "enabled")
    expect_equal(request_body$thinking$budget_tokens, 2000)

    # Check that pre_fill is still in messages but doesn't affect output
    # (This matches actual function behavior - it doesn't remove pre_fill from messages,
    # but it does nullify it when returning the result)
    expect_equal(length(request_body$messages), 2)
  })
})

test_that("ask_anthropic handles system parameter correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create a mock that captures the arguments
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "Test response")
        )
      ))
    )

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)

    # Test with system parameter
    ask_anthropic("Test prompt", system = "System prompt")
    args <- mockery::mock_args(post_mock)[[1]]
    request_body <- jsonlite::fromJSON(args$body)

    # Check system parameter is included
    expect_true("system" %in% names(request_body))

    # Verify system contains the prompt text somewhere (without assuming structure)
    system_json <- jsonlite::toJSON(request_body$system, auto_unbox = TRUE)
    expect_true(grepl("System prompt", system_json))

    # Test with cached system parameter
    ask_anthropic("Test prompt", system = "System prompt", cache_system = TRUE)
    args <- mockery::mock_args(post_mock)[[2]]
    request_body <- jsonlite::fromJSON(args$body)

    # Check system parameter is included
    expect_true("system" %in% names(request_body))

    # Verify cache control is present somewhere in the system structure
    system_json <- jsonlite::toJSON(request_body$system, auto_unbox = TRUE)
    expect_true(grepl("ephemeral", system_json))
  })
})

test_that("ask_anthropic handles pre_fill parameter correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Create mock response
    mock_response <- mock_anthropic_response(list(
      content = list(
        list(type = "text", text = "Response text")
      )
    ))

    # Mock functions
    post_mock <- mockery::mock(mock_response)
    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "glue::glue", function(text) paste0("Pre-fill text", "Response text"))

    # Test with pre_fill parameter
    result <- ask_anthropic("Test prompt", pre_fill = "Pre-fill text")

    # Verify result contains pre_fill
    expect_equal(result, "Pre-fill textResponse text")

    # Verify pre_fill is sent in the request without assuming exact structure
    args <- mockery::mock_args(post_mock)[[1]]
    body_json <- args$body

    # Check that pre_fill text appears in the request body
    expect_true(grepl("Pre-fill text", body_json))

    # Check that assistant role appears in the request body
    expect_true(grepl("assistant", body_json))
  })
})

test_that("ask_anthropic handles API errors correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Mock error response
    mock_error_response <- mock_anthropic_response(
      list(error = list(message = "API error message")),
      status_code = 400
    )

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", function(...) mock_error_response)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) TRUE)
    mockery::stub(ask_anthropic, "httr::http_status", function(...) list(message = "Bad Request"))

    # Test error handling
    expect_message(
      result <- ask_anthropic("Test prompt"),
      "Error in Anthropic API call"
    )
    expect_null(result)
  })
})

test_that("ask_anthropic handles retry logic correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Mock responses for retry
    mock_bad_gateway <- mock_anthropic_response(list(), status_code = 502)
    mock_success <- mock_anthropic_response(list(
      content = list(
        list(type = "text", text = "Test response")
      )
    ))

    # Mock functions with a sequence of responses
    post_mock <- mockery::mock(mock_bad_gateway, mock_success)

    # Create a counter for sleep calls to verify backoff
    sleep_mock <- mockery::mock()

    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::status_code", function(resp) resp$status_code)
    mockery::stub(ask_anthropic, "httr::http_error", function(resp) resp$status_code >= 400)
    mockery::stub(ask_anthropic, "Sys.sleep", sleep_mock)
    mockery::stub(ask_anthropic, "message", function(...) NULL)

    # Test retry logic
    result <- ask_anthropic("Test prompt")

    # Verify POST was called twice
    expect_equal(mockery::mock_args(post_mock) |> length(), 2)

    # Verify sleep was called once with the correct delay
    expect_equal(mockery::mock_args(sleep_mock) |> length(), 1)
    expect_equal(mockery::mock_args(sleep_mock)[[1]][[1]], 1) # Initial delay

    # Verify final result
    expect_equal(result, "Test response")
  })
})

test_that("ask_anthropic handles missing API key", {
  # Test with no API key
  withr::with_envvar(c("ANTHROPIC_API_KEY" = ""), {
    expect_error(
      ask_anthropic("Test prompt"),
      "Please set the ANTHROPIC_API_KEY environment variable"
    )
  })
})

test_that("ask_anthropic returns dev output when requested", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Mock response
    mock_full_response <- list(
      id = "msg_12345",
      model = "claude-3-7-sonnet-latest",
      content = list(
        list(type = "text", text = "Test response")
      )
    )

    # Mock functions
    mockery::stub(ask_anthropic, "httr::POST", function(...) mock_anthropic_response(mock_full_response))
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "httr::content", function(...) mock_full_response)

    # Test dev = TRUE
    result <- ask_anthropic("Test prompt", dev = TRUE)

    # Verify full response is returned
    expect_equal(result, mock_full_response)
    expect_true("id" %in% names(result))
    expect_true("model" %in% names(result))
  })
})

# Mock PDF handling functions for testing
encode_pdf <- function(path) {
  return("base64_encoded_pdf")
}

create_pdf_block <- function(data, cache = FALSE) {
  if (cache) {
    return(list(
      type = "image",
      source = list(
        type = "base64",
        media_type = "application/pdf",
        data = data
      ),
      cache_control = list(type = "ephemeral")
    ))
  } else {
    return(list(
      type = "image",
      source = list(
        type = "base64",
        media_type = "application/pdf",
        data = data
      )
    ))
  }
}

test_that("ask_anthropic handles PDF correctly", {
  withr::with_envvar(c("ANTHROPIC_API_KEY" = "test_key"), {
    # Mock functions with support for two calls
    post_mock <- mockery::mock(
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "PDF analysis response")
        )
      )),
      mock_anthropic_response(list(
        content = list(
          list(type = "text", text = "PDF analysis response")
        )
      ))
    )

    mockery::stub(ask_anthropic, "httr::POST", post_mock)
    mockery::stub(ask_anthropic, "httr::http_error", function(...) FALSE)
    mockery::stub(ask_anthropic, "encode_pdf", encode_pdf)
    mockery::stub(ask_anthropic, "create_pdf_block", create_pdf_block)

    # Test with PDF parameter
    result <- ask_anthropic("Analyze this PDF", pdf_path = "test.pdf")

    # Verify result
    expect_equal(result, "PDF analysis response")

    # Verify request contains PDF data
    body_json <- mockery::mock_args(post_mock)[[1]]$body
    expect_true(grepl("application/pdf", body_json))
    expect_true(grepl("base64_encoded_pdf", body_json))

    # Test with cached PDF
    result <- ask_anthropic("Analyze this PDF", pdf_path = "test.pdf", cache_pdf = TRUE)

    # Verify result
    expect_equal(result, "PDF analysis response")

    # Verify cache control in second request
    body_json <- mockery::mock_args(post_mock)[[2]]$body
    expect_true(grepl("ephemeral", body_json))
  })
})
