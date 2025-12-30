from transformers import AutoModelForCausalLM, AutoProcessor

# pipline demo

# from transformers import pipeline

# pipe = pipeline("text-generation", model="google/functiongemma-270m-it")
# messages = [
#     {"role": "user", "content": "Who are you?"},
# ]
# pipe(messages)

GEMMA_MODEL_ID = "google/functiongemma-270m-it"

processor = AutoProcessor.from_pretrained(GEMMA_MODEL_ID, device_map="auto")
model = AutoModelForCausalLM.from_pretrained(
    GEMMA_MODEL_ID, dtype="auto", device_map="auto"
)

weather_function_schema = {
    "type": "function",
    "function": {
        "name": "get_current_temperature",
        "description": "Gets the current temperature for a given location.",
        "parameters": {
            "type": "object",
            "properties": {
                "location": {
                    "type": "string",
                    "description": "The city name, e.g. San Francisco",
                },
            },
            "required": ["location"],
        },
    },
}

message = [
    # ESSENTIAL SYSTEM PROMPT:
    # This line activates the model's function calling logic.
    {
        "role": "developer",
        "content": "You are a model that can do function calling with the following functions",
    },
    {"role": "user", "content": "What's the temperature in London?"},
]

inputs = processor.apply_chat_template(
    message,
    tools=[weather_function_schema],
    add_generation_prompt=True,
    return_dict=True,
    return_tensors="pt",
)

out = model.generate(
    **inputs.to(model.device), pad_token_id=processor.eos_token_id, max_new_tokens=128
)
output = processor.decode(
    out[0][len(inputs["input_ids"][0]) :], skip_special_tokens=True
)

print(f"Output: {output}")


# def main():
#     print("Hello from functiongemma-demo!")


# if __name__ == "__main__":
#     main()
