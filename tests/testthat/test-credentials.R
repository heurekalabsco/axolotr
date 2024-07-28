test_that("create_credentials function works", {
  temp_env <- tempfile()
  create_credentials(path = temp_env, OPENAI_API_KEY = "test_key")

  env_contents <- readLines(temp_env)
  expect_true(any(grepl("OPENAI_API_KEY=test_key", env_contents)))

  unlink(temp_env)
})
