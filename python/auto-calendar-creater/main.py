import json
import os
import re

from google import genai
from google.genai import types

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY")

if not API_KEY:
    print("Error: Please set the GEMINI_API_KEY environment variable.")
    exit(1)

# Initialize the new Gemini Client
client = genai.Client(api_key=API_KEY)

# --- System Prompt ---
SYSTEM_PROMPT = """
You are a helpful calendar assistant. Your goal is to help the user create calendar events.
Interact with the user to gather the following details:
1. Event Title
2. Date (Year, Month, Day)
3. Start Time
4. End Time or Duration
5. Location (Optional: Physical address or link like Zoom/Google Meet)

Once you have all the necessary information, you must provide a summary and then output a JSON block exactly in this format:
```json
{
  "summary": "Event Title",
  "start": "YYYYMMDDTHHMMSS",
  "end": "YYYYMMDDTHHMMSS",
  "location": "Event Location or Link",
  "timezone": "IANA Timezone ID",
  "description": "Any additional notes"
}
```
Guidelines:
- **Location**: Use this for physical addresses or virtual meeting links (Zoom, Meet, etc.).
- **Description**: Be conservative. Only include notes if the user explicitly provides them or if there is critical context. If not mentioned, leave as an empty string.
- **Timezone**: Default to "America/New_York" (EST) if unspecified.
- **Format**: Date-time must be iCalendar compatible (e.g., 20251225T090000).
"""


def create_ics_file(event_data):
    """Generates an .ics file from the event data dictionary."""
    # Clean filename: replace spaces and invalid characters
    safe_summary = (
        re.sub(r"[^\w\s-]", "", event_data["summary"]).strip().replace(" ", "_")
    )
    filename = f"{safe_summary}.ics"

    timezone = event_data.get("timezone", "America/New_York")
    location = event_data.get("location", "")

    ics_content = f"""BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Auto Calendar Creator//EN
BEGIN:VEVENT
SUMMARY:{event_data["summary"]}
DTSTART;TZID={timezone}:{event_data["start"]}
DTEND;TZID={timezone}:{event_data["end"]}
LOCATION:{location}
DESCRIPTION:{event_data.get("description", "")}
END:VEVENT
END:VCALENDAR"""

    with open(filename, "w") as f:
        f.write(ics_content)
    return filename


def main():
    # Start a chat session with the new SDK
    chat = client.chats.create(
        model="gemini-3-flash",
        config=types.GenerateContentConfig(
            system_instruction=SYSTEM_PROMPT,
            temperature=0.7,
        ),
    )

    print("--- AI Calendar Assistant (Powered by Gemini) ---")
    print("Type 'quit' or 'exit' to stop.\n")

    while True:
        try:
            user_input = input("You: ")
            if user_input.lower() in ["quit", "exit"]:
                break

            response = chat.send_message(user_input)
            print(f"\nAI: {response.text}\n")

            # Check if the response contains a JSON block
            json_match = re.search(r"```json\s*(.*?)\s*```", response.text, re.DOTALL)
            if json_match:
                try:
                    event_details = json.loads(json_match.group(1))
                    filename = create_ics_file(event_details)
                    print(f"--- Event Created! ---")
                    print(f"Calendar file generated: {filename}")
                    print(
                        f"You can now double-click {filename} to add it to your calendar.\n"
                    )
                except Exception as e:
                    print(f"Error parsing event details: {e}")
        except KeyboardInterrupt:
            print("\nGoodbye!")
            break
        except Exception as e:
            print(f"An unexpected error occurred: {e}")


if __name__ == "__main__":
    main()
