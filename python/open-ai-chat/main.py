from http.server import BaseHTTPRequestHandler, HTTPServer
from chat import chat_completions
import json


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == '/chat':
            self.chat_handle()
        # self.send_response(200)
        # self.send_header('Content-type', 'text/plain')
        # self.end_headers()
        # self.wfile.write(b"hello")

    def chat_handle(self):
        try:
            content_length = int(self.headers['Content-Length'])
            # Read the data
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)

            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()

            # just assume the data is follow the data struct
            response = chat_completions(data)
            self.wfile.write(response.encode())

        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")


def run(server_class=HTTPServer,
        handler_class=SimpleHTTPRequestHandler,
        addr="127.0.0.1", port=8080):

    server_address = (addr, port)
    httpd = server_class(server_address, handler_class)
    print(f"Starting httpd server on {addr}:{port}")
    httpd.serve_forever()


if __name__ == "__main__":
    run()
