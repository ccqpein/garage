import json
import sys

import requests

# Default LM Studio local server URL
BASE_URL = "http://127.0.0.1:1234/api/v1"


def get_loaded_models():
    """Fetch the list of currently loaded models from LM Studio."""
    try:
        response = requests.get(f"{BASE_URL}/models", timeout=5)
        response.raise_for_status()
        data = response.json()

        # The /v1/models endpoint typically returns a list in 'data'
        models = data.get("models", [])
        if not models:
            # Debug: Print the raw response if no models found
            print(f"Debug: API returned no models. Full response: {data}")
            return []

        return [model["key"] for model in models]
    except Exception as e:
        print(f"Error connecting to LM Studio at {BASE_URL}/models: {e}")
        return []


def chat_native(model_id, user_input, system_prompt=None):
    """Send a message to the native /api/v1/chat endpoint (stateful)."""
    payload = {"model": model_id, "input": user_input, "stream": False}

    if system_prompt:
        payload["system_prompt"] = system_prompt

    try:
        response = requests.post(f"{BASE_URL}/chat", json=payload, timeout=60)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"\nError during chat request: {e}")
        if hasattr(e, "response") and e.response is not None:
            print(f"Server response: {e.response.text}")
        return None


def main():
    print("--- LM Studio Local Server Demo ---")
    print(f"Connecting to {BASE_URL}...")

    # 1. Check for loaded models
    models = get_loaded_models()

    model_id = None
    if models:
        print("\nAvailable loaded models:")
        for i, m in enumerate(models):
            print(f"{i}. {m}")

        try:
            choice = input(
                f"\nSelect model index (0-{len(models)-1}, default 0): "
            ).strip()
            if not choice:
                idx = 0
            else:
                idx = int(choice)

            if 0 <= idx < len(models):
                model_id = models[idx]
            else:
                print(f"Invalid index. Defaulting to index 0.")
                model_id = models[0]
        except ValueError:
            print("Invalid input. Defaulting to index 0.")
            model_id = models[0]

        print(f"Using model: {model_id}")
    else:
        print("\n[!] No models detected as 'loaded' via the API.")
        print("In LM Studio, make sure you have:")
        print(" 1. Downloaded a model.")
        print(" 2. Selected/Loaded it in the 'Local Server' or 'AI Chat' tab.")

        # Fallback: Ask user for a model ID if they know one (like in their curl example)
        manual_id = input(
            "\nEnter model ID to try anyway (or press Enter to exit): "
        ).strip()
        if not manual_id:
            sys.exit(1)
        model_id = manual_id

    # Optional System Prompt
    system_prompt = "You are a helpful assistant. You answer only in rhymes."
    print(f'\nSystem Prompt: "{system_prompt}"')
    print("Type 'exit' or 'quit' to end.\n")

    # 2. Interactive Chat Loop
    while True:
        try:
            user_input = input("User: ").strip()
        except (KeyboardInterrupt, EOFError):
            print("\nExiting...")
            break

        if not user_input:
            continue

        if user_input.lower() in ["exit", "quit"]:
            print("Goodbye!")
            break

        print("Assistant: ", end="", flush=True)
        result = chat_native(model_id, user_input, system_prompt=system_prompt)

        if result:
            output_list = result.get("output", [])
            assistant_message = ""
            if isinstance(output_list, list):
                for item in output_list:
                    if isinstance(item, dict) and item.get("type") == "message":
                        assistant_message = item.get("content", "")
                        break
            print(assistant_message)
        else:
            print("[No response or error occurred]")
        print("-" * 20)


if __name__ == "__main__":
    main()
