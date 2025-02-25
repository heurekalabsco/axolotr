#GENERIC----
#' Process a Prompt using Specified API
#'
#' This function acts as a wrapper to process a prompt using Anthropic, Google, Groq, or OpenAI's API based on the specified model.
#'
#' @param prompt A character string containing the user's message.
#' @param system An optional character string containing the system message. Defaults to NULL.
#' @param model A character string specifying the model or API to use.
#'              Can be a general API name (e.g., "anthropic", "google", "openai", "groq")
#'              or a specific model version (e.g., "gpt-4", "claude-3-opus-20240229").
#' @return A character string containing the response from the chosen API.
#'
#' @export
ask <- function(prompt,
                system = NULL,
                model = "claude") {
  if (stringr::str_detect(model, "gpt|openai")) {
    prompt_output <- ask_openai(prompt = prompt, system = system, model = model)
  } else if (stringr::str_detect(model, "gemini|google")) {
    prompt_output <- ask_google(prompt = prompt, system = system, model = model)
  } else if (stringr::str_detect(model, "llama|mixtral|groq")) {
    prompt_output <- ask_groq(prompt = prompt, system = system, model = model)
  } else if (stringr::str_detect(model, "claude|anthropic|haiku|sonnet|opus")) {
    prompt_output <- ask_anthropic(prompt = prompt, system = system, model = model)
  } else {
    stop("Invalid model. Please provide a valid model or API name.")
  }
  return(prompt_output)
}

#APIs----
#' Call Anthropic API with Optional Prompt Caching
#'
#' This function sends a prompt to the Claude API by Anthropic and returns the generated text response.
#' It supports prompt caching for improved performance when using consistent background context.
#'
#' @param prompt A character string representing the prompt to be sent to the Claude API.
#' @param system An optional character string to provide a system prompt to the model. Default is NULL.
#' @param model A character string specifying the model to use.
#' @param temperature A numeric value representing the temperature parameter. Default is 0.
#' @param max_tokens An integer specifying the maximum number of tokens in the response. Default is 4096.
#' @param pre_fill An optional character string to pre-fill the model's response. Default is NULL.
#' @param pdf_path Optional path to a PDF file.
#' @param cache_system Logical indicating whether to cache the system prompt. Default is FALSE. This content will be cached for 5 minutes and can be reused across multiple calls.
#' @param cache_pdf Logical indicating whether to cache the PDF content. Default is FALSE. This content will be cached for 5 minutes and can be reused across multiple calls.
#' @param dev Logical indicating whether to return the full response from the API. Default is FALSE.
#' @param ... Additional parameters to pass to the Anthropic API.
#'
#' @return The generated text response from the Claude API, or NULL if an error occurs.
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#'
#' @export
ask_anthropic <- function(prompt,
                          system = NULL,
                          model = "claude",
                          temperature = 0,
                          max_tokens = 8192, #~6.2K words \ 28K unicode characters \ ~12-13 single spaced pages
                          pre_fill = NULL,
                          pdf_path = NULL,
                          cache_system = FALSE,
                          cache_pdf = FALSE,
                          dev = FALSE,
                          ...) {
  # Validate API key
  if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
    stop("Please set the ANTHROPIC_API_KEY environment variable")
  }

  # Define model mapping for generic names
  model_mapping <- list(
    "claude" = "claude-3-7-sonnet-latest",
    "anthropic" = "claude-3-7-sonnet-latest",
    "haiku" = "claude-3-5-haiku-latest",
    "sonnet" = "claude-3-7-sonnet-latest",
    "opus" = "claude-3-opus-latest"
  )

  # Resolve the model name if it's a generic name
  if (model %in% names(model_mapping)) {
    model <- model_mapping[[model]]
  }

  # Alert user that maximum token limits for claude-3-opus and claude-3-haiku is 4096 and needs to be explicit
  if (model %in% c("claude-3-opus-latest", "claude-3-haiku-latest") && max_tokens > 4096) {
    stop("Maximum tokens for claude-3-opus-latest and claude-3-haiku-latest is 4096. Please set max_tokens to 4096 or lower.")
  }

  tryCatch({
    # Validate inputs
    if (!is.character(prompt)) {
      stop("prompt should be a string!")
    }
    if (!is.null(system) && !is.character(system)) {
      stop("system should be a string or NULL!")
    }

    # Set up the URL and headers
    url <- "https://api.anthropic.com/v1/messages"
    headers <- httr::add_headers(
      "Content-Type" = "application/json",
      "X-API-Key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )

    # Define an empty system block
    system_block <- list()

    # Add system content if provided
    if (!is.null(system)) {
      if (cache_system) {
        system_block <- c(
          system_block,
          list(
            list(
              type = "text",
              text = system,
              cache_control = list(type = "ephemeral")
            )
          )
        )
      } else {
        system_block <- c(
          system_block,
          list(
            list(
              type = "text",
              text = system
            )
          )
        )
      }
    }

    # Add PDF if provided
    if (!is.null(pdf_path)) {
      pdf_data <- encode_pdf(pdf_path)
      pdf_block <- create_pdf_block(pdf_data, cache = cache_pdf)

      # Create content blocks
      prompt <- list(
        pdf_block,
        list(
          type = "text",
          text = prompt
        )
      )
    }

    # Prepare the messages content
    messages_content <- list(
      list(
        role = "user",
        content = prompt
      )
    )

    # Add pre-fill content if provided
    if (!is.null(pre_fill)) {
      messages_content <- c(
        messages_content,
        list(list(
          role = "assistant",
          content = pre_fill
        ))
      )
    }

    # Prepare the request body
    body <- list(
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      system = system_block,
      messages = messages_content
    )

    # Remove NULL elements from the body
    body <- body[!sapply(body, is.null)]

    # Make the API call
    response <- httr::POST(url = url, config = headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))

    # Check for HTTP errors
    if (httr::http_error(response)) {
      http_status <- httr::http_status(response)
      stop(
        sprintf(http_status$message)
      )
    }

    # Parse the response
    result <- httr::content(response, as = "parsed")

    # Check for API-level errors
    if (!is.null(result$error)) {
      stop(paste("Anthropic API error:", result$error$message))
    }

    if(dev == TRUE){
      return(result)
    } else {
      # Extract and return the text
      if (!is.null(result$content) && length(result$content) > 0 && !is.null(result$content[[1]]$text)) {
        content <- result$content[[1]]$text

        # Add prefill back to content
        if(!is.null(pre_fill)){
          content <- glue::glue("{pre_fill}{content}")
        }
        return(content)
      } else {
        stop("Unexpected response format from Anthropic API")
      }
    }
  }, error = function(e) {
    message("Error in Anthropic API call: ", e$message)
    return(NULL)
  })
}

#' Call the Google Gemini API to Process a Prompt
#'
#' This function uses the Google Gemini API to process a given prompt.
#'
#' @param prompt A character string containing the user's message to be processed by the Google API.
#' @param system An optional character string to provide a system prompt to the model. Default is NULL.
#' @param model A character string specifying the model to use. Default is "gemini-1.5-pro".
#' @param temperature A numeric value representing the temperature parameter for the API call. Default is 0.7.
#' @param ... Additional parameters to pass to the Google API.
#'
#' @return A character string containing the processed text from the Google API response. If an error occurs, the function will return NULL.
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#'
#' @export
ask_google <- function(prompt,
                       system = NULL,
                       model = "gemini-1.5-pro",
                       temperature = 0.7,
                       ...) {
  # Validate API key
  if (Sys.getenv("GOOGLE_GEMINI_API_KEY") == "") {
    stop("Please set the GOOGLE_GEMINI_API_KEY environment variable")
  }

  tryCatch({
    # Validate input
    if (!is.character(prompt)) {
      stop("prompt should be a string!")
    }

    # Set up the URL
    url <- glue::glue('https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent')

    # Prepare the request body
    contents <- list(
      list(parts = list(list(text = prompt)))
    )

    if (!is.null(system)) {
      contents <- c(list(list(parts = list(list(text = system)))), contents)
    }

    body <- list(
      contents = contents,
      generationConfig = list(
        temperature = temperature
      )
    )

    # Make the API call
    response <- httr::POST(
      url = url,
      httr::add_headers(
        "Content-Type" = "application/json"
      ),
      query = list(key = Sys.getenv("GOOGLE_GEMINI_API_KEY")),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )

    # Check for HTTP errors
    httr::stop_for_status(response)

    # Parse the response
    result <- httr::content(response, as = "parsed")

    # Check for API-level errors
    if (!is.null(result$error)) {
      stop(paste("API error:", result$error$message))
    }

    # Extract and return the text
    if (length(result$candidates) > 0 && !is.null(result$candidates[[1]]$content$parts[[1]]$text)) {
      return(result$candidates[[1]]$content$parts[[1]]$text)
    } else {
      stop("No valid response from the API")
    }

  }, error = function(e) {
    message("Error occurred with Google API call: ", e$message)
    return(NULL)
  })
}

#' Call the Groq API to Process a Prompt
#'
#' This function uses the Groq API to process a given prompt. It supports various open-source models
#' available through Groq's API.
#'
#' @param prompt A character string containing the user's message to be processed by the Groq API.
#' @param system An optional character string to provide a system prompt to the model. Default is NULL.
#' @param model A character string specifying the model to use. Can be generic ("groq", "mixtral", "llama") or specific (e.g., "mixtral-8x7b-32768").
#' @param temperature A numeric value representing the temperature parameter for the API call. Default is 0.3.
#' @param max_tokens An integer specifying the maximum number of tokens in the response. Default is 4096.
#' @param ... Additional parameters to pass to the Groq API.
#'
#' @return A character string containing the processed text from the Groq API response. If an error occurs, the function will return NULL.
#' @importFrom httr POST add_headers content http_status
#' @importFrom jsonlite toJSON
#'
#' @export
ask_groq <- function(prompt,
                     system = NULL,
                     model = "groq",
                     temperature = 0.3,
                     max_tokens = 4096,
                     ...) {
  # Validate API key
  if (Sys.getenv("GROQ_API_KEY") == "") {
    stop("Please set the GROQ_API_KEY environment variable")
  }

  # Define model mapping for generic names
  model_mapping <- list(
    "groq" = "mixtral-8x7b-32768",
    "mixtral" = "mixtral-8x7b-32768",
    "llama" = "llama3-70b-8192"
  )

  # Resolve the model name if it's a generic name
  if (model %in% names(model_mapping)) {
    model <- model_mapping[[model]]
  }

  tryCatch({
    # Validate input
    if (!is.character(prompt)) {
      stop("prompt should be a string!")
    }

    # Set up the URL and headers
    url <- "https://api.groq.com/openai/v1/chat/completions"
    headers <- httr::add_headers(
      Authorization = paste0("Bearer ", Sys.getenv("GROQ_API_KEY")),
      "Content-Type" = "application/json"
    )

    # Set default system message if not provided
    if (is.null(system)) {
      system <- ""
      # Act as an expert writer. You are specific and concise.
    }

    # Prepare the request body
    body <- list(
      model = model,
      messages = list(
        list(role = "system", content = system),
        list(role = "user", content = prompt)
      ),
      temperature = temperature,
      max_tokens = max_tokens,
      ...
    )

    # Remove NULL elements from the body
    body <- body[!sapply(body, is.null)]

    # Make the API call
    response <- httr::POST(url = url, config = headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))

    # Check for HTTP errors
    if (httr::http_error(response)) {
      http_status <- httr::http_status(response)
      stop(
        sprintf(
          "HTTP error: %s (Status code: %s)\n%s",
          http_status$reason,
          http_status$status_code,
          httr::content(response, "text", encoding = "UTF-8")
        )
      )
    }

    # Parse the response
    result <- httr::content(response, as = "parsed")

    # Check for API-level errors
    if (!is.null(result$error)) {
      stop(paste("Groq API error:", result$error$message))
    }

    # Extract and return the text
    if (!is.null(result$choices) && length(result$choices) > 0 && !is.null(result$choices[[1]]$message$content)) {
      return(result$choices[[1]]$message$content)
    } else {
      stop("Unexpected response format from Groq API")
    }

  }, error = function(e) {
    message("Error in Groq API call: ", e$message)
    return(NULL)
  })
}

#' Process a Prompt using OpenAI's GPT Models
#'
#' This function uses the OpenAI API to communicate with a specified GPT model.
#'
#' @param prompt A character string containing the user's message to the GPT model.
#' @param system An optional character string to provide a system prompt to the model. Default is NULL.
#' @param model A character string containing the GPT model to use. Can be generic ("gpt", "openai") or specific (e.g., "gpt-4-1106-preview").
#' @param temperature A numeric value for the temperature parameter. Default is 0.7.
#' @param max_tokens An integer specifying the maximum number of tokens in the response. Default is NULL (uses model's maximum).
#' @param ... Additional parameters to pass to the OpenAI API.
#'
#' @return A character string containing the GPT model's response, or NULL if an error occurs.
#' @importFrom httr POST add_headers content http_status
#' @importFrom jsonlite toJSON
#'
#' @export
ask_openai <- function(prompt,
                       system = NULL,
                       model = "gpt",
                       temperature = 0.7,
                       max_tokens = 4096,
                       ...) {
  tryCatch({
    # Validate API key
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      stop("Please set the OPENAI_API_KEY environment variable")
    }

    # Define model mapping for generic names
    model_mapping <- list(
      "gpt" = "gpt-4o",
      "openai" = "gpt-4o",
      "gpt-3" = "gpt-3.5-turbo",
      "gpt-4" = "gpt-4o"
    )

    # Resolve the model name if it's a generic name
    if (model %in% names(model_mapping)) {
      model <- model_mapping[[model]]
    }

    # Set up the URL and headers
    url <- "https://api.openai.com/v1/chat/completions"
    headers <- httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
    )

    # Prepare the messages
    messages <- list(list(role = "user", content = prompt))
    if (!is.null(system)) {
      messages <- c(list(list(role = "system", content = system)), messages)
    }

    # Prepare the request body
    body <- list(
      model = model,
      messages = messages,
      temperature = temperature,
      max_tokens = max_tokens
    )

    # Remove NULL elements from the body
    body <- body[!sapply(body, is.null)]

    # Make the API call
    response <- httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))

    # Check for HTTP errors
    if (httr::http_error(response)) {
      http_status <- httr::http_status(response)
      stop(
        sprintf(
          "HTTP error: %s (Status code: %s)\n%s",
          http_status$reason,
          http_status$status_code,
          httr::content(response, "text", encoding = "UTF-8")
        )
      )
    }

    # Parse the response
    result <- httr::content(response, as = "parsed")

    # Check for API-level errors
    if (!is.null(result$error)) {
      stop(paste("OpenAI API error:", result$error$message))
    }

    # Extract and return the text
    if (length(result$choices) > 0 && !is.null(result$choices[[1]]$message$content)) {
      return(result$choices[[1]]$message$content)
    } else {
      stop("Unexpected response format from OpenAI API")
    }

  }, error = function(e) {
    message("Error in OpenAI API call: ", e$message)
    return(NULL)
  })
}

#' Process a Prompt using Ollama local server
#'
#' This function uses the Ollama local server to communicate with a specified Ollama model.
#'
#' @param prompt A character string containing the user's message to the Ollama model.
#' @param system An optional character string to provide a system prompt to the model. Default is NULL.
#' @param model A character string containing the Ollama model to use. Must be specific and contain the prefix "local_" ("local_llama3", "local_llama3.1").
#'
#' @return A character string containing the Ollama model's response, or NULL if an error occurs.
# ask_ollama <- function(prompt,
#                        system = NULL,
#                        model = "llama3",
#                        ...) {
#   tryCatch({
#     # Test Ollama local server
#     test_ollama <- ollamar::test_connection()
#     if (test_ollama$status_code != 200) {
#       stop("Please make sure your Ollama server is running")
#     }
#
#     if (is.null(system)) {
#       system <- ''
#     }
#     response <- ollamar::generate(model = model, prompt = prompt, system = system, output = "text")
#     return(response)
#   }, error = function(e) {
#     message("Error in Ollama call: ", e$message)
#     return(NULL)
#   })
# }

#IMAGE APIs----
#' Process a Prompt using OpenAI's DallE Models
#'
#' This function uses the OpenAI API to interactively communicate with a specified DallE model.
#' The default model is DallE3, but it can use DallE2.
#'
#' @param prompt A character string containing the user's message to the GPT model.
#' @param model A character string containing the GPT model to use.
#' @param image_size Default is "1024x1024" (but 1792 is banner).
#' @param image_style A character string containing the style to use (vivid or natural).
#' @param output Default is url.
#'
#' @return A character string containing the GPT model's response.
#'
#' @importFrom httr POST add_headers
#'
#' @export
imagine_openai <- function(prompt,
                           model = "dall-e-3",
                           image_size = "1024x1024",
                           image_style = "vivid",
                           output = "url") {
  tryCatch(
    {
      # Validate input
      if (!is.character(prompt)) {
        stop("prompt should be a string!")
      }

      # Validate api key
      if (Sys.getenv("OPENAI_API_KEY") == "") {
        stop("Please set the OPENAI_API_KEY environment variable")
      }

      # Set up the url + api key
      url <- "https://api.openai.com/v1/images/generations"
      headers <- httr::add_headers(Authorization = paste0("Bearer ", Sys.getenv("OPENAI_API_KEY")))

      # Prepare the request body
      body <- list(
        model = model,
        prompt = prompt,
        n = 1,
        quality = "hd",
        size = image_size,
        style = image_style,
        response_format = output
      )

      # Make the API call
      jsonResponse <- httr::POST(url = url, config = headers, body = body, encode = "json")
      result <- httr::content(jsonResponse, as = "parsed")

      # Check and handle null result
      if (is.null(result$data[[1]]$url)) {
        message("Retrying OpenAI image API call")

        jsonResponse <- httr::POST(url = url, config = headers, body = body, encode = "json")
        result <- httr::content(jsonResponse, as = "parsed")
      }

      # Extract and return the text
      url <- result$data[[1]]$url
      message(glue::glue('Image generated with revised prompt: {stringr::str_trunc(result$data[[1]]$revised_prompt, 120)}'))
      return(url)
    },
    error = function(e) {
      cat("Error occurred with OpenAI image API call\n")
      return(NULL)
    }
  )
}

#Helper----
#' Create or Update API Credentials in .Renviron
#'
#' This function creates or updates API credentials in the user's .Renviron file.
#' It handles credentials for Anthropic, Google Gemini, Groq, and OpenAI APIs.
#'
#' @param path Character string. The path to the .Renviron file. If NULL (default),
#'   uses the .Renviron file in the user's home directory.
#' @param ANTHROPIC_API_KEY Character string. The API key for Anthropic. Default is NULL.
#' @param GOOGLE_GEMINI_API_KEY Character string. The API key for Google Gemini. Default is NULL.
#' @param GROQ_API_KEY Character string. The API key for Groq. Default is NULL.
#' @param OPENAI_API_KEY Character string. The API key for OpenAI. Default is NULL.
#'
#' @return None. This function is called for its side effects.
#'
#' @details
#' The function reads the existing .Renviron file (if it exists), updates or adds
#' the provided API keys, and writes the changes back to the file. It then reloads
#' the environment to make the changes available in the current session.
#'
#' Only the provided API keys will be updated or added. Existing entries for
#' keys not provided will remain unchanged.
#'
#' @note
#' After running this function, it is recommended to restart the R session
#' for all changes to take full effect.
#'
#' @examples
#' \dontrun{
#' create_credentials(
#'   ANTHROPIC_API_KEY = "your_anthropic_key_here",
#'   OPENAI_API_KEY = "your_openai_key_here"
#' )
#' }
#'
#' @export
create_credentials <- function(path = NULL,
                               ANTHROPIC_API_KEY = NULL,
                               GOOGLE_GEMINI_API_KEY = NULL,
                               GROQ_API_KEY = NULL,
                               OPENAI_API_KEY = NULL) {
  # Determine the path for the .Renviron file
  if (is.null(path)) {
    path <- file.path(Sys.getenv("HOME"), ".Renviron")
  }

  # Read existing .Renviron file if it exists
  if (file.exists(path)) {
    lines <- readLines(path)
  } else {
    lines <- character()
  }

  # Function to update or add a key-value pair
  update_or_add <- function(key, value) {
    if (!is.null(value)) {
      existing <- grep(paste0("^", key, "="), lines)
      new_line <- paste0(key, "=", value)
      if (length(existing) > 0) {
        lines[existing] <- new_line
      } else {
        lines <- c(lines, new_line)
      }
    }
    lines
  }

  # Update or add each API key
  lines <- update_or_add("ANTHROPIC_API_KEY", ANTHROPIC_API_KEY)
  lines <- update_or_add("GOOGLE_GEMINI_API_KEY", GOOGLE_GEMINI_API_KEY)
  lines <- update_or_add("GROQ_API_KEY", GROQ_API_KEY)
  lines <- update_or_add("OPENAI_API_KEY", OPENAI_API_KEY)

  # Write the updated .Renviron file
  writeLines(lines, path)

  # Reload the environment
  readRenviron(path)

  message("Credentials updated successfully. Please restart your R session for changes to take effect.")
}

