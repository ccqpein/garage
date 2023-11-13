from typing import Dict

default_dalle3_prompt = {
    "model": "dall-e-3",
    "prompt": "a white siamese cat",
    "size": "1024x1024",
    "quality": "standard",
    "n": 1,
}


def image_generate_dalle3(client, prompt):
    response = client.images.generate(
        model=prompt.get("model", default_dalle3_prompt["model"]),
        prompt=prompt.get("prompt", default_dalle3_prompt["prompt"]),
        size=prompt.get("size", default_dalle3_prompt["size"]),
        quality=prompt.get("quality", default_dalle3_prompt["quality"]),
        n=prompt.get("n", default_dalle3_prompt["n"]),
    )

    return response.data[0].url


def image_generate_dalle2(client, prompt=default_dalle3_prompt):
    #:= todo: need to know how to edit pic

    return
