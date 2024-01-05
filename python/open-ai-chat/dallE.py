from typing import Dict, String

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


def image_edit_dalle2(client, image: String, mask: String, prompt):
    #:= todo: need to know how to edit pic

    response = client.images.edit(
        model="dall-e-2",
        image=open(image, "rb"),
        mask=open(mask, "rb"),
        prompt="change the head to alligator and the location to florida",
        n=1,
        size="1024x1024")

    image_url = response.data[0].url
    return image_url
