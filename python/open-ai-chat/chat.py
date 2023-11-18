from typing import List

MODEL = "gpt-4-1106-preview"


def chat_completions(client, cl: List):
    '''cl example: [
    {"role": "system", "content": "You are a helpful assistant."},
    {"role": "user", "content": "Who won the world series in 2020?"},
    {"role": "assistant", "content": "The Los Angeles Dodgers won the World Series in 2020."},
    {"role": "user", "content": "Where was it played?"}
    ]'''

    response = client.chat.completions.create(
        model=MODEL,
        messages=cl
    )

    # Choice: https://github.com/openai/openai-python/blob/e661da69b4a11d48edfe21d2b12f53c201593596/src/openai/types/chat/chat_completion.py
    return response.choices[0].message.content
