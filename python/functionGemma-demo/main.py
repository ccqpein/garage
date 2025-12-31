import re

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

## weather_function_schema example

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


# get_current_weather, demo 2
def get_current_weather(location: str, unit: str = "celsius"):
    """
    Gets the current weather in a given location.

    Args:
        location: The city and state, e.g. "San Francisco, CA" or "Tokyo, JP"
        unit: The unit to return the temperature in. (choices: ["celsius", "fahrenheit"])

    Returns:
        temperature: The current temperature in the given location
        weather: The current weather in the given location
    """
    return {"temperature": 15, "weather": "sunny"}


message = [
    {
        "role": "developer",
        "content": "You are a model that can do function calling with the following functions",
    },
    {"role": "user", "content": "What's the temperature in London?"},
]

inputs = processor.apply_chat_template(
    message,
    # tools=[weather_function_schema], # demo 1
    tools=[get_current_weather],  # demo 2
    add_generation_prompt=True,
    return_dict=True,
    return_tensors="pt",
)

### demo 1
# out = model.generate(
#     **inputs.to(model.device), pad_token_id=processor.eos_token_id, max_new_tokens=128
# )
# output = processor.decode(
#     out[0][len(inputs["input_ids"][0]) :], skip_special_tokens=True
# )
### demo 1 end

# output = processor.decode(inputs["input_ids"][0], skip_special_tokens=False)
# print(f"Output1: {output}")

out = model.generate(
    **inputs.to(model.device), pad_token_id=processor.eos_token_id, max_new_tokens=128
)
generated_tokens = out[0][len(inputs["input_ids"][0]) :]
output = processor.decode(generated_tokens, skip_special_tokens=True)

print(f"Output after first call: {output}")

# return the tags response of function call


def extract_tool_calls(text):
    def cast(v):
        try:
            return int(v)
        except:
            try:
                return float(v)
            except:
                return {"true": True, "false": False}.get(v.lower(), v.strip("'\""))

    return [
        {
            "name": name,
            "arguments": {
                k: cast((v1 or v2).strip())
                for k, v1, v2 in re.findall(
                    r"(\w+):(?:<escape>(.*?)<escape>|([^,}]*))", args
                )
            },
        }
        for name, args in re.findall(
            r"<start_function_call>call:(\w+)\{(.*?)\}<end_function_call>",
            text,
            re.DOTALL,
        )
    ]


# check the call

calls = extract_tool_calls(output)
if calls:
    # add the message that tool calls
    message.append(
        {
            "role": "assistant",
            "tool_calls": [{"type": "function", "function": call} for call in calls],
        }
    )
    print(message[-1])

    # Call the function and get the result
    #####################################
    # WARNING: This is a demonstration. #
    #####################################
    # Using globals() to call functions dynamically can be dangerous in
    # production. In a real application, you should implement a secure way to
    # map function names to actual function calls, such as a predefined
    # dictionary of allowed tools and their implementations.
    results = [
        {"name": c["name"], "response": globals()[c["name"]](**c["arguments"])}
        for c in calls
    ]
    # add function call result
    message.append({"role": "tool", "content": results})
    print(message[-1])


inputs = processor.apply_chat_template(
    message,
    tools=[get_current_weather],
    add_generation_prompt=True,
    return_dict=True,
    return_tensors="pt",
)
out = model.generate(
    **inputs.to(model.device), pad_token_id=processor.eos_token_id, max_new_tokens=128
)

# skip_special_tokens false means left those tags inside
output = processor.decode(out[0], skip_special_tokens=False)
print(f"Output after call function: {output}")


generated_tokens = out[0][len(inputs["input_ids"][0]) :]
output = processor.decode(generated_tokens, skip_special_tokens=True)
print(f"Output final: {output}")

message.append({"role": "assistant", "content": output})

print("-" * 80)
# full history
for item in message:
    print(item)

print("-" * 80)


# def main():
#     print("Hello from functiongemma-demo!")


# if __name__ == "__main__":
#     main()
