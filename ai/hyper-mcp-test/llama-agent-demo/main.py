import asyncio
import json

from fastmcp import Client


async def get_tools_des():
    async with Client("http://127.0.0.1:3001/sse") as client:
        tools = await client.list_tools()
        return tools


TOOLS = asyncio.run(get_tools_des())

# tools des in list of dict
TOOLS_DES = [json.loads(o.model_dump_json()) for o in TOOLS]

# the header of function call
FUNC_CALL_HEADER = r"""You have access to functions. If you decide to invoke any of the function(s),
 you MUST put it in the format of
name => [func_name1(params_name1=params_value1, params_name2=params_value2...), func_name2(params)] 

You SHOULD NOT include any other text in the response if you call a function
""" + json.dumps(TOOLS_DES)


async def call_mcp_server(tool_name):  #:= todo
    async with Client("http://127.0.0.1:3001/sse") as client:
        print(await client.call_tool("time", {"name": "get_time_utc"}))


def call_gemma():
    from mlx_lm import generate, load

    model, tokenizer = load("mlx-community/gemma-3-4b-it-4bit-DWQ")

    prompt = "What's the time now?"

    # if tokenizer.chat_template is not None:
    #     messages = [{"role": "user", "content": FUNC_CALL_HEADER + prompt}]
    #     prompt = tokenizer.apply_chat_template(messages, add_generation_prompt=True)

    response = generate(
        model, tokenizer, prompt=FUNC_CALL_HEADER + prompt, verbose=False
    )

    # return the "time => get_time_utc()"
    return response


async def main():
    # Connect via SSE
    pass
