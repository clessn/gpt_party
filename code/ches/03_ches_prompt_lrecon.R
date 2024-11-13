library(dplyr)
library(openai)
library(stringr)
library(jsonlite)

df <- readRDS("data/ches/tmp/02_ches_data_id.rds")

# Initialize columns for storing results
df$lrecon_1 <- NA
df$lrecon_2 <- NA
df$lrecon_3 <- NA

# Initialize error logging dataframe
run_errors <- data.frame(
  "run" = NA,
  "party" = NA,
  "country" = NA
)

# Check for duplicate party_ids
if (anyDuplicated(df$party_id)) {
  warning("Duplicate party_ids found in dataset. This might cause issues with score assignment.")
}


# Outer loop for three runs
for (run in 1:3) {
  # Loop through unique countries instead of rows
  for (current_country in unique(df$country)) {
    # Create party list for the current country
    party_list <- df %>%
      filter(country == current_country) %>%
      mutate(party_info = paste0("- ", party_id, ": ", party)) %>%
      pull(party_info) %>%
      paste(collapse = "\n")

    role <- paste0("You are a political science expert focusing on party positions in ", current_country)

    prompt <- paste0(
      "Parties can be classified in terms of their stance on economic issues such as ",
      "privatization, taxes, regulation, government spending, and the welfare state. ",
      "Parties on the economic left want government to play an active role in the economy. Those on the economic right want a reduced role for government. ",
      "Please analyze the following political parties and assign each a position score on an 11 points scale from 0 (Extreme left) to 10 (Extreme right) ",
      "based on their ideological position in ", current_country, "'s political context.\n\n",
      "Here are the parties:\n",
      party_list, "\n\n",
      "Please return your response ONLY as a JSON object with the following structure:\n",
      "{\n",
      "  \"party_positions\": {\n",
      "    \"[party_id]\": [score],\n",
      "    ...\n",
      "  }\n",
      "}\n\n",
      "Important guidelines:\n",
      "- Work on this problem step by step. You MUST provide a score from each party based on your knowledge, even if it's limited.\n",
      "- Consider the specific political context of ", current_country, "\n",
      "- Use only numerical scores from 0 to 10\n",
      "- Provide scores with up to one decimal point precision\n",
      "- Include all parties listed above\n",
      "- Return ONLY the JSON object, no additional explanation"
    )

    # Make API call
    chat_prompt <- tryCatch(
      {
        create_chat_completion(
          model = "gpt-4o",
          messages = list(
            list(
              "role" = "system",
              "content" = role
            ),
            list(
              "role" = "user",
              "content" = prompt
            )
          )
        )
      },
      error = function(e) {
        message("API call failed for country ", current_country, ": ", e)
        return(NULL)
      }
    )

    if (!is.null(chat_prompt)) {
      output <- chat_prompt$choices$message.content

      # Strip everything before and after the JSON object
      clean_json <- sub(".*?(\\{.*\\}).*", "\\1", output)

      # Check if clean_json contains a valid JSON structure
      if (grepl("^\\{.*\\}$", clean_json)) {
        # Clean up newline characters and any extra whitespace
        clean_json <- gsub("\\n", "", clean_json)
        clean_json <- gsub("\\s+", " ", clean_json)

        # Try parsing the JSON content into a list with corrected tryCatch syntax
        parsed_json <- tryCatch(
          expr = {
            jsonlite::fromJSON(clean_json)
          },
          error = function(e) {
            message("Failed to parse JSON for country ", current_country, ": ", e)
            return(NULL)
          }
        ) # End of tryCatch

        if (!is.null(parsed_json) && !is.null(parsed_json$party_positions)) {
          # Alternative approach using tibble
          positions_df <- tibble::tibble(
            party_id = names(parsed_json$party_positions),
            score = as.numeric(unlist(parsed_json$party_positions))
          )

          # Update the main dataframe based on the run number
          score_column <- paste0("lrecon_", run)

          # Update scores for matching party_ids
          for (pid in positions_df$party_id) {
            df[df$party_id == pid, score_column] <- positions_df[positions_df$party_id == pid, "score"]
          }

          # Print a summary for validation
          print(paste0("Processed scores for ", current_country, " - Run ", run))
          print(positions_df)
        } else {
          print(paste("Invalid JSON structure or missing party_positions for", current_country))
          # Log the error
          run_errors <- rbind(
            run_errors,
            data.frame(
              run = run,
              party = party_list,
              country = current_country
            )
          )
        }
      } else {
        print(paste("Invalid JSON structure for", current_country))
        # Log the error
        run_errors <- rbind(
          run_errors,
          data.frame(
            run = run,
            party = party_list,
            country = current_country
          )
        )
      }
    } else {
      print(paste(
        "Error with API for Run:", run,
        "Country:", current_country
      ))
      # Log the error
      run_errors <- rbind(
        run_errors,
        data.frame(
          run = run,
          party = party_list,
          country = current_country
        )
      )
    }

    # Delay to avoid hitting rate limits
    Sys.sleep(5)
  }
}

# Remove the initial NA row from run_errors if there are actual errors
if (nrow(run_errors) > 1) {
  run_errors <- run_errors[-1, ]
}

# Save the results
saveRDS(df, "data/ches/tmp/03_ches_data_id_with_scores.rds")
saveRDS(run_errors, "data/ches/tmp/scoring_errors.rds")

# Print summary of errors if any occurred
if (nrow(run_errors) > 0) {
  print("Summary of errors encountered:")
  print(table(run_errors$country))
}
