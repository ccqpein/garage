from http.server import BaseHTTPRequestHandler, HTTPServer
from chat import chat_completions
from dallE import image_generate_dalle3
import json
from openai import OpenAI


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        self.client = OpenAI()
        super().__init__(*args, **kwargs)

    def do_POST(self):
        if self.path == '/chat':
            self.chat_handle()
        elif self.path == '/image_DALLE3':
            self.DALLE3_handle()

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
            response = chat_completions(self.client, data)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write(f'{e}'.encode())

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
            response = image_generate_dalle3(self.client, data)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(response.encode())
        except Exception as e:
            self.send_response(500)  # Internal Server Error
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write(f'{e}'.encode())


def run(server_class=HTTPServer,
        handler_class=SimpleHTTPRequestHandler,
        addr="127.0.0.1", port=8080):

    server_address = (addr, port)
    httpd = server_class(server_address, handler_class)
    print(f"Starting httpd server on {addr}:{port}")
    httpd.serve_forever()


if __name__ == "__main__":
    run()
