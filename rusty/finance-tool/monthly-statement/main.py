import json
import os
from datetime import datetime
from typing import Any, Dict, List

import pandas as pd
from openai import OpenAI

STATEMENTS_FOLDER = os.path.join(os.getcwd(), "statements")
HISTORY_FOLDER = os.path.join(os.getcwd(), "history")


def read_statement_csv() -> pd.DataFrame | None:
    STATEMENTS_FOLDER = os.path.join(os.getcwd(), "statements")

    # Optional: Basic check to inform the user if the folder doesn't exist
    if not os.path.exists(STATEMENTS_FOLDER):
        print(f"Warning: The statements folder '{STATEMENTS_FOLDER}' does not exist.")
        print("Please ensure your CSV files are placed in a 'statements' folder")
        print("relative to where you are running this script.")
        print("Current working directory:", os.getcwd())

    filename = input(f"Enter the name of the CSV file in '{STATEMENTS_FOLDER}': ")
    file_path = os.path.join(STATEMENTS_FOLDER, filename)

    if not filename.lower().endswith(".csv"):
        print(
            "Note: The filename does not have a .csv extension. Attempting to read as CSV anyway."
        )

    try:
        df = pd.read_csv(file_path)
        print(f"\nSuccessfully read '{filename}':")
        print(df.head())
        return df
    except FileNotFoundError:
        print(f"Error: The file '{filename}' was not found in '{STATEMENTS_FOLDER}'.")
        print("Please ensure the file exists and the name is correct (case-sensitive).")
        if os.path.exists(STATEMENTS_FOLDER):
            print("Files found in statements folder:", os.listdir(STATEMENTS_FOLDER))
        return None
    except pd.errors.EmptyDataError:
        print(f"Error: The file '{filename}' is empty.")
        return None
    except pd.errors.ParserError as e:
        print(f"Error parsing CSV '{filename}': {e}")
        print("Please check if the file is a valid CSV format.")
        return None
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
        return None


def save_conversation_history(
    conversation_history: List[Dict[str, str]], filename_prefix: str = "chat_history"
) -> None:
    """
    Saves the conversation history to a JSON file in a 'history' folder.

    Args:
        conversation_history: A list of dictionaries representing the chat messages.
        filename_prefix: A prefix for the filename (e.g., "chat_history").
    """
    # Create the history folder if it doesn't exist
    os.makedirs(HISTORY_FOLDER, exist_ok=True)

    # Generate a unique filename using a timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    file_path = os.path.join(HISTORY_FOLDER, f"{filename_prefix}_{timestamp}.json")

    try:
        with open(file_path, "w", encoding="utf-8") as f:
            json.dump(conversation_history, f, indent=4, ensure_ascii=False)
        print(f"\nConversation history saved to: {file_path}")
    except IOError as e:
        print(f"Error saving conversation history to '{file_path}': {e}")
    except Exception as e:
        print(f"An unexpected error occurred while saving history: {e}")


def chat_with_csv_data(ai_client: Any, csv_data: pd.DataFrame) -> List[Dict[str, str]]:
    if csv_data.empty:
        print(
            "Warning: The provided CSV data is empty. The AI will not have data to analyze."
        )
        return

    csv_string = csv_data.to_csv(index=False)

    messages = [
        {
            "role": "system",
            "content": "You are an AI assistant specialized in analyzing tabular data. You will be provided with CSV data in text format, on which you should base your answers. Your goal is to help the user understand and extract insights from this data.",
        },
        {
            "role": "user",
            "content": f"Here is the CSV data for your analysis:\n\n{csv_string}\n\nPlease analyze this data and answer my questions. What's the first question you would like to ask me about this data?",
        },
    ]

    print("\n--- AI Chat with CSV Data ---")
    print("Type 'quit' or 'exit' to end the chat.")

    while True:
        user_input = input("\nYou: ")
        if user_input.lower() in ["quit", "exit"]:
            print("Ending chat. Goodbye!")
            break

        messages.append({"role": "user", "content": user_input})

        try:
            response = ai_client.chat.completions.create(
                model="gpt-5",
                messages=messages,
            )

            ai_response_content = response.choices[0].message.content
            print(f"AI: {ai_response_content}")

            messages.append({"role": "assistant", "content": ai_response_content})

        except Exception as e:
            print(f"An error occurred during AI communication: {e}")
            print(
                "Please ensure your AI client is correctly configured and accessible."
            )
            break  # Exit on error


def main():
    my_csv_data = read_statement_csv()

    if my_csv_data is not None:
        try:
            openai_client = OpenAI()
            print("\nOpenAI client initialized. Starting chat with CSV data...")
            full_conversation_history = chat_with_csv_data(openai_client, my_csv_data)

            if full_conversation_history:
                save_conversation_history(full_conversation_history)
        except ImportError:
            print(
                "\nError: 'openai' library not found. Please install it: pip install openai"
            )
        except Exception as e:
            print(
                f"\nError initializing OpenAI client. Make sure your API key is set. Details: {e}"
            )


if __name__ == "__main__":
    main()
