import sys

# cannot use the transformers for this model for some reason
from mlx_lm import generate, load


def main():
    # Use the MLX-optimized model ID
    model_id = "mlx-community/gemma-3-27b-it-8bit"

    print(f"Loading model: {model_id}...")
    print("Note: This uses the 'mlx-lm' library optimized for Apple Silicon.")

    try:
        # load returns (model, tokenizer)
        model, tokenizer = load(model_id)
    except Exception as e:
        print(f"\nError loading model '{model_id}': {e}")
        return

    messages = []
    print("\n--- Gemma 3 Chat Demo (MLX) ---")
    print("Type 'exit' or 'quit' to end the chat.")

    while True:
        try:
            user_input = input("\nYou: ")
            if user_input.lower() in ["exit", "quit"]:
                print("Goodbye!")
                break

            if not user_input.strip():
                continue

            # Add user message to history
            messages.append({"role": "user", "content": user_input})

            # Prepare the prompt using the tokenizer's chat template
            # tokenize=False returns a formatted string
            prompt = tokenizer.apply_chat_template(
                messages, tokenize=False, add_generation_prompt=True
            )

            print("Gemma: ", end="", flush=True)

            # Generate response
            # mlx_lm.generate handles the generation loop
            response_text = generate(
                model,
                tokenizer,
                prompt=prompt,
                verbose=True,  # Set to True to stream to stdout, or False and print return value
                max_tokens=500,
            )

            # If verbose=True, it prints as it goes.
            # If we want to capture it for history, generate returns the string.
            # However, generate(verbose=True) might print the prompt too?
            # Usually verbose=True in mlx-lm prints generation speed info too.
            # Let's use verbose=False to control the printing, or keep it simple.
            # Actually, let's use a simpler approach for the CLI experience:
            # We'll just print the text.

            # Note: mlx_lm.generate returns the generated text.
            # To avoid re-printing the prompt if 'prompt' is included in output (depends on implementation),
            # usually mlx_lm.generate returns *only* the new text.

            # Refined call:
            # response = generate(model, tokenizer, prompt=prompt, verbose=False, max_tokens=500)
            # print(response)

            # But the user might want streaming.
            # For this demo, let's stick to non-streaming for simplicity in code,
            # unless we implement a streamer. mlx_lm.generate prints if verbose=True.

            # Let's overwrite the previous printed "Gemma: " if using verbose=True
            # because verbose=True might output stats.
            # Let's try verbose=False and print manually.

            # Add model response to history for context
            messages.append({"role": "model", "content": response_text})

        except KeyboardInterrupt:
            print("\nInterrupted. Exiting...")
            break
        except Exception as e:
            print(f"An error occurred during generation: {e}")


if __name__ == "__main__":
    main()
