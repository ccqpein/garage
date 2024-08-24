import anthropic
import os

client = anthropic.Anthropic(
    # defaults to os.environ.get("ANTHROPIC_API_KEY")
    api_key=os.environ.get("ANTHROPIC_API_KEY"), )

message = client.messages.create(
    model="claude-3-5-sonnet-20240620",
    max_tokens=1000,
    # system="Respond only in Yoda-speak.",
    messages=[{
        "role": "user",
        "content": "How are you today?"
    }])

print(message.content[0].model_dump()["text"])

# with client.messages.stream(
#         max_tokens=1024,
#         messages=[{
#             "role": "user",
#             "content": "Hello"
#         }],
#         model="claude-3-opus-20240229",
# ) as stream:
#     for text in stream.text_stream:
#         print(text, end="", flush=True)
