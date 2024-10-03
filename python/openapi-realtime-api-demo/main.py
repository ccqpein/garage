import asyncio
import json
import os
import signal
from signal import SIGINT, SIGTERM

import websockets

# WebSocket URL
url = "wss://api.openai.com/v1/realtime?model=gpt-4o-realtime-preview-2024-10-01"
api_key = os.getenv("OPENAI_API_KEY")

shutdown_flag = False


async def cleanup(ws):
    # Send WebSocket close frame
    print("Closing the WebSocket connection...")
    await ws.close()
    print("Connection closed.")


async def send_custom_message(ws):
    """Function to continuously send new messages."""
    while True:
        await asyncio.sleep(2)  # Add delay between messages, for example

        new_message = {
            "type": "response.create",
            "response": {
                "modalities": ["text"],
                "instructions": "Send new data without waiting for previous response.",
            },
        }

        await ws.send(json.dumps(new_message))  # Send the new message
        print("New message sent.")

        # Breaking based on some external condition
        if shutdown_flag:
            break


async def connect():
    global shutdown_flag

    headers = {"Authorization": f"Bearer {api_key}", "OpenAI-Beta": "realtime=v1"}

    async with websockets.connect(url, extra_headers=headers) as ws:
        print("Connected to server.")

        # Sending the first message upon connection
        initial_message = {
            "type": "response.create",
            "response": {
                "modalities": ["text"],
                "instructions": "Please assist the user.",
            },
        }
        await ws.send(json.dumps(initial_message))

        message_task = asyncio.create_task(
            send_custom_message(ws)
        )  # Task for sending new data

        # Handling incoming messages (asynchronous receiving)
        try:
            async for message in ws:
                print(f"Received message: {json.loads(message)}")

                if shutdown_flag:
                    break

        except websockets.exceptions.ConnectionClosedOK:
            print("Connection closed successfully.")
        finally:
            await message_task  # Ensure message task finishes
            await cleanup(ws)


def handle_shutdown(signum, frame):
    global shutdown_flag
    print(f"Received shutdown signal: {signum}")
    shutdown_flag = True


# Registering signal handlers:
signal.signal(SIGINT, handle_shutdown)
signal.signal(SIGTERM, handle_shutdown)

try:
    asyncio.run(connect())
except Exception as e:
    print(f"Error: {e}")
