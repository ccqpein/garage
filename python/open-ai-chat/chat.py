import json
from typing import List

MODEL = "gpt-4.1-2025-04-14"


def chat_completions(
    client,
    cl: List,
    use_tool: bool,
    tools=None,
):
    """cl example: [
    {"role": "system", "content": "You are a helpful assistant."},
    {"role": "user", "content": "Who won the world series in 2020?"},
    {"role": "assistant", "content": "The Los Angeles Dodgers won the World Series in 2020."},
    {"role": "user", "content": [{"type": "text", "text": "Where was it played?"}]}
    {"role": "user", "content": [{"type":"text", "text": "what's this"}, {"type": "image_url", "image_url":{"url":f"data:image/jpeg;base64,{img_test}"}}]}
    ]"""

    if use_tool and tools:
        response = client.chat.completions.create(
            model=MODEL,
            messages=cl,
            tools=tools,
            tool_choice="auto",
        )
    else:
        response = client.chat.completions.create(
            model=MODEL,
            messages=cl,
        )

    result = {
        "content": response.choices[0].message.content or None,
    }

    if response.choices[0].message.tool_calls:
        result["tool_calls"] = {
            "name": response.choices[0].message.tool_calls[0].function.name,
            "arguments": json.loads(
                response.choices[0].message.tool_calls[0].function.arguments
            ),
        }
    # Choice: https://github.com/openai/openai-python/blob/e661da69b4a11d48edfe21d2b12f53c201593596/src/openai/types/chat/chat_completion.py
    return json.dumps(result)
