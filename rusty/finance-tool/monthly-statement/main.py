import os

import pandas as pd

STATEMENTS_FOLDER = os.path.join(os.getcwd(), "statements")


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


def main():
    print(read_statement_csv())


if __name__ == "__main__":
    main()
