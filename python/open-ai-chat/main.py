from http.server import BaseHTTPRequestHandler, HTTPServer
from chat import chat_completions
from dallE import image_generate_dalle3
from funcall import reminder_example
import json
from openai import OpenAI
import logging
import sys

#logging.basicConfig(level=logging.DEBUG, stream=sys.stdout)

CLIENT = OpenAI()
FUNCS = [reminder_example()]


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()
        message = "Hello, World!"
        self.wfile.write(bytes(message, "utf8"))
        return

    def do_POST(self):
        if self.path == '/chat':
            self.chat_handle()
        elif self.path == '/image_DALLE3':
            self.DALLE3_handle()
        return

    # handlers below
    def chat_handle(self):
        data = {}
        try:
            content_length = int(self.headers['Content-Length'])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")

        # call open ai
        try:
            response = chat_completions(CLIENT, data, False, FUNCS)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write(f'{e}'.encode())
        return

    def DALLE3_handle(self):
        data = {}
        try:
            content_length = int(self.headers['Content-Length'])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")

        try:
            response = image_generate_dalle3(CLIENT, data)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write(f'{e}'.encode())
        return

    def func_call_handle(self):
        # todo
        return


def run(server_class=HTTPServer,
        handler_class=SimpleHTTPRequestHandler,
        addr="127.0.0.1",
        port=8080):
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
