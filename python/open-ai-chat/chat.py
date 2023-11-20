from typing import List
import json


MODEL = "gpt-4-1106-preview"


def chat_completions(client, cl: List, tools=None):
    '''cl example: [
    {"role": "system", "content": "You are a helpful assistant."},
    {"role": "user", "content": "Who won the world series in 2020?"},
    {"role": "assistant", "content": "The Los Angeles Dodgers won the World Series in 2020."},
    {"role": "user", "content": "Where was it played?"}
    ]'''

    if tools:
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
            "arguments": json.loads(response.choices[0].message.tool_calls[0].function.arguments),
        }

    # Choice: https://github.com/openai/openai-python/blob/e661da69b4a11d48edfe21d2b12f53c201593596/src/openai/types/chat/chat_completion.py
    #:= need to change the return type in main.py/ also the rust side
    return json.dumps(result)
