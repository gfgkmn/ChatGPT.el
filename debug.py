#!/usr/bin/env python

from epc.client import EPCClient
import os
import time
import traceback

# Load configuration
config_path = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
if not os.path.exists(config_path):
    raise FileNotFoundError(f"Configuration file not found at {config_path}")

# Define the EPC server host and port
host = 'localhost'
with open("epc_port.txt", "r") as f:
    port = int(f.read().strip())

# Initialize EPC client
client = EPCClient((host, port))

# Define test parameters
# botname = "harrison"  # Replace with your bot's name in chatgptel.json
botname = "gpt4o"  # Replace with your bot's name in chatgptel.json


# query_text = "Hello! How are you doing today?"

# # Test the `query` function
# print("Testing `query` function...")
# response = client.call_sync('query', [query_text, botname])
# print("Response:", response)

# # Test the `querystream` function
# print("\nTesting `querystream` function...")

# try:
#     # emacs org-id-uuid function for python
#     query_with_id = "b7e8d0b0-1c6b-4c7e-bf6d-4b7b6d4e8c4e"
#     query_with_id = f"{query_with_id}-{query_text}"
#     response_stream = []
#     while True:
#         reply = client.call_sync('querystream', [query_with_id, botname, False])
#         if reply[3] == []:
#             break
#         elif reply[1] == 1:
#             raise Exception("Error in `querystream` test:", reply[3])
#         response_stream.append(reply[3])
#         print("Streamed Reply:", reply[3])
#     print("Complete Streamed Response:", "".join(response_stream))
# except Exception as e:
#     error_message = traceback.format_exc()
#     print("Error in `querystream` test:", error_message)


query_text = "@[[/sshx:cranberry:/home/yuhe/dreamily-v3.5-deploy/app/modeling_dcformer.py]], @[[/sshx:cranberry:/home/yuhe/dreamily-v3.5-deploy/app/test_cuda.py]], how manu files you can see"

# # Test the `query` function
# print("Testing `query` function...")
# response = client.call_sync('query', [query_text, botname])
# print("Response:", response)

# Test the `querystream` function
print("\nTesting `querystream` function...")

try:
    # (org-id-uuid)
    # emacs org-id-uuid function for python
    query_with_id = "dddae2ef-69df-4f94-a3e6-85cbca10785d"
    query_with_id = f"{query_with_id}-{query_text}"
    response_stream = []
    while True:
        reply = client.call_sync('querystream', [query_with_id, botname, False])
        if reply[3] == []:
            break
        elif reply[1] == 1:
            raise Exception("Error in `querystream` test:", reply[3])
        response_stream.append(reply[3])
        print("Streamed Reply:", reply[3])
    print("Complete Streamed Response:", "".join(response_stream))
except Exception as e:
    error_message = traceback.format_exc()
    print("Error in `querystream` test:", error_message)

# Close the client
client.close()

print("\nTests completed.")
