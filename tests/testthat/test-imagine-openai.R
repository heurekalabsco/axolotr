test_that("imagine_openai function returns a valid URL", {
  skip_if_offline()
  skip_if_not(Sys.getenv("OPENAI_API_KEY") != "", "OpenAI API key not available")

  result <- imagine_openai(prompt = "A cute axolotl")
  expect_match(result, "^https://")

  # Verify the URL is accessible
  response <- httr::GET(result)
  expect_equal(httr::status_code(response), 200)
})
