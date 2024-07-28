test_that("API calls work correctly", {
  skip_if_offline()

  skip_if_not(Sys.getenv("OPENAI_API_KEY") != "", "OpenAI API key not available")
  skip_if_not(Sys.getenv("GOOGLE_GEMINI_API_KEY") != "", "Google API key not available")
  skip_if_not(Sys.getenv("GROQ_API_KEY") != "", "Groq API key not available")
  skip_if_not(Sys.getenv("ANTHROPIC_API_KEY") != "", "Anthropic API key not available")

  test_prompt <- "Briefly explain what an axolotl is."

  openai_response <- ask_openai(prompt = test_prompt, model = "gpt-3.5-turbo")
  expect_type(openai_response, "character")
  expect_true(nchar(openai_response) > 50)
  expect_true(grepl("axolotl", tolower(openai_response)))

  google_response <- ask_google(prompt = test_prompt)
  expect_type(google_response, "character")
  expect_true(nchar(google_response) > 50)
  expect_true(grepl("axolotl", tolower(google_response)))

  groq_response <- ask_groq(prompt = test_prompt)
  expect_type(groq_response, "character")
  expect_true(nchar(groq_response) > 50)
  expect_true(grepl("axolotl", tolower(groq_response)))

  anthropic_response <- ask_anthropic(prompt = test_prompt)
  expect_type(anthropic_response, "character")
  expect_true(nchar(anthropic_response) > 50)
  expect_true(grepl("axolotl", tolower(anthropic_response)))
})

test_that("API calls handle errors correctly", {
  skip_if_offline()

  expect_error(ask_openai(prompt = "Test", model = "invalid-model"), "OpenAI API error")
  expect_error(ask_google(prompt = "Test", model = "invalid-model"), "API error")
  expect_error(ask_groq(prompt = "Test", model = "invalid-model"), "Groq API error")
  expect_error(ask_anthropic(prompt = "Test", model = "invalid-model"), "Anthropic API error")
})
