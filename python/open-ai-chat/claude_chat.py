import json
import os
from typing import List

import anthropic

MODEL = "claude-3-5-sonnet-20240620"

anthropic_client = anthropic.Anthropic(
    # defaults to os.environ.get("ANTHROPIC_API_KEY")
    api_key=os.environ.get("ANTHROPIC_API_KEY"),
)


def chat_completions(cl: List):
    """cl example: [
    {"role": "user", "content": "Who won the world series in 2020?"},
    {"role": "user", "content": [{"type": "text", "text": "Where was it played?"}]}
    {"role": "user", "content": [{"type":"text", "text": "what's this"}, {"type": "image_url", "image_url":{"url":f"data:image/jpeg;base64,{img_test}"}}]}
    ]"""

    response = anthropic_client.messages.create(
        model=MODEL,
        max_tokens=1000,
        messages=cl,
    )

    result = {
        "content": response.content[0].model_dump()["text"] or None,
    }

    return json.dumps(result)
