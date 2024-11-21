
<!-- README.md is generated from README.Rmd. Please edit that file -->

# axolotr

<!-- badges: start -->
<!-- badges: end -->

The goal of axolotr is to provide a unified interface for interacting
with various Language Model APIs, including OpenAI’s GPT, Google’s
Gemini, Anthropic’s Claude, and Groq’s API. This package allows users to
easily switch between different LLMs and leverage their capabilities for
a wide range of natural language processing tasks.

## Installation

You can install the development version of axolotr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heurekalabsco/axolotr")
```

## Setup

Before using axolotr, you need to set up your API keys. You can do this
by adding them to your `.Renviron` file:

``` r
library(axolotr)

create_credentials(
OPENAI_API_KEY = "your_openai_key",
GOOGLE_GEMINI_API_KEY = "your_google_key",
ANTHROPIC_API_KEY = "your_anthropic_key",
GROQ_API_KEY = "your_groq_key"
)
```

Remember to restart your R session after setting up the credentials.

## Usage

Here are some examples of how to use `axolotr`:

``` r
library(axolotr)

# Using OpenAI's GPT model
gpt_response <- axolotr::ask(
  prompt = "What are the main differences between R and Python?",
  model = "gpt"
)

# Using Google's Gemini model
gemini_response <- axolotr::ask(
  prompt = "Explain the concept of machine learning in simple terms.",
  model = "gemini"
)

# Using Anthropic's Claude model
claude_response <- axolotr::ask(
  prompt = "Summarize the key points of the theory of relativity.",
  model = "sonnet"
)

# Using Groq's API with a specific model
groq_response <- axolotr::ask(
  prompt = "What are the potential applications of quantum computing?",
  model = "mixtral-8x7b-32768"
)
```

## Additional Arguments

Pass additional arguments, such as the common pre_fill argument for
Claude models, to the `ask` function:

``` r
> ask(
  prompt = "Continue this sentence: The quick brown fox",
  model = "claude",
  temperature = 0.7)
[1] "The quick brown fox jumped over the lazy dog."

> ask(
  prompt = "Continue this sentence: The quick brown fox",
  model = "claude",
  pre_fill = "jumps over",
  temperature = 0.7)
[1] " the lazy dog."
```

## Features

- Unified interface for multiple LLM APIs
- Flexible model selection (generic or specific model names)
- Easy setup of API credentials
- Customizable parameters for each API call

## TODO

- `...` isn’t set up yet. Add ability to pass additional model-specific
  parameters to the API calls.
- Make cute axolotl hex sticker

## Contributing

Contributions to axolotr are welcome!

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details.
