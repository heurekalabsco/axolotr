test_that("ask function routes to correct API", {
  testthat::skip_if_not_installed("mockery")

  # Rest of the test remains the same
  mockery::stub(ask, "ask_openai", function(...) "OpenAI response")
  mockery::stub(ask, "ask_google", function(...) "Google response")
  mockery::stub(ask, "ask_groq", function(...) "Groq response")
  mockery::stub(ask, "ask_anthropic", function(...) "Anthropic response")

  expect_equal(ask(prompt = "Test", model = "gpt"), "OpenAI response")
  expect_equal(ask(prompt = "Test", model = "gemini"), "Google response")
  expect_equal(ask(prompt = "Test", model = "mixtral"), "Groq response")
  expect_equal(ask(prompt = "Test", model = "claude"), "Anthropic response")
})

test_that("ask function throws error for invalid model", {
  expect_error(ask(prompt = "Test", model = "invalid_model"), "Invalid model")
})
