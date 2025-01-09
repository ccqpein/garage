from openai import OpenAI

# so I don't need read api key here?

response = OpenAI().chat.completions.create(
    model="gpt-4o-mini",
    messages=[
        {
            "role": "user",
            "content": "Write a haiku about artificial intelligence.",
        }
    ],
)

print(response)
