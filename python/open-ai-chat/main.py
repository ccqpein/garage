import json
import logging
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

from openai import OpenAI

from chat import chat_completions
from dallE import image_generate_dalle3
from funcall import reminder_example
from whisper import transcribe_audio

# logging.basicConfig(level=logging.DEBUG, stream=sys.stdout)

CLIENT = OpenAI()
FUNCS = [reminder_example()]


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        message = "Hello, World!"
        self.wfile.write(bytes(message, "utf8"))
        return

    def do_POST(self):
        if self.path == "/chat":
            self.chat_handle()
        elif self.path == "/image_DALLE3":
            self.DALLE3_handle()
        elif self.path == "/audio_transcribe":
            self.audio_transcribe_handle()
        return

    # handlers below
    def chat_handle(self):
        data = {}
        try:
            content_length = int(self.headers["Content-Length"])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")

        # call open ai
        try:
            response = chat_completions(CLIENT, data, False, FUNCS)
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write(f"{e}".encode())
        return

    def audio_transcribe_handle(self):
        data = {}
        try:
            content_length = int(self.headers["Content-Length"])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

            # Extract audio file path from the request body
            audio_file_path = data.get("file_path")
            if not audio_file_path:
                self.send_error(400, "Missing 'file_path' in request")
                return

            # Perform transcription (assuming your function `transcribe_audio` exists)
            transcript = transcribe_audio(CLIENT, audio_file_path)

            # Send back the transcribed text
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            self.wfile.write(transcript.encode())
        except (json.JSONDecodeError, ValueError):
            self.send_error(400, "Invalid data or missing 'file_path'")
        except Exception as e:
            self.send_response(500)  # InternalServer Error
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write(f"{e}".encode())

    def DALLE3_handle(self):
        data = {}
        try:
            content_length = int(self.headers["Content-Length"])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")

        try:
            response = image_generate_dalle3(CLIENT, data)
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write(f"{e}".encode())
        return

    def func_call_handle(self):
        # todo
        return


def run(
    server_class=HTTPServer,
    handler_class=SimpleHTTPRequestHandler,
    addr="127.0.0.1",
    port=8080,
):
    server_address = (addr, port)
    logging.debug("Starting server setup")

    try:
        httpd = server_class(server_address, handler_class)
        logging.debug(f"Starting httpd server on {addr}:{port}")
        httpd.serve_forever()
    except Exception as e:
        logging.error(f"Error in running server: {e}")
        sys.stderr.write(f"Error: {e}\n")
        sys.stderr.flush()
        return


if __name__ == "__main__":
    logging.debug("Script start")
    run()
