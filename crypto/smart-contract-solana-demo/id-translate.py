byte_array = []

bytes_object = bytes(byte_array)

try:
    decoded_string_utf8 = bytes_object.decode("utf-8")
except UnicodeDecodeError:
    print("Could not decode with UTF-8. Trying latin-1.")
    decoded_string_latin1 = bytes_object.decode("latin-1")
    print(decoded_string_latin1[1])

try:
    ascii_string = bytes_object.decode("ascii")
except UnicodeDecodeError:
    print("Could not decode with ASCII. Some bytes are outside the ASCII range.")
