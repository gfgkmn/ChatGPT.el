#!/usr/bin/env python

from epc.client import EPCClient
import os
import time

# Load configuration
config_path = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
if not os.path.exists(config_path):
    raise FileNotFoundError(f"Configuration file not found at {config_path}")

# Define the EPC server host and port
host = 'localhost'
port = 61675

# Initialize EPC client
client = EPCClient((host, port))

# Define test parameters
botname = "harrison"  # Replace with your bot's name in chatgptel.json
query_text = "Hello! How are you doing today?"

# Test the `query` function
print("Testing `query` function...")
response = client.call_sync('query', [query_text, botname])
print("Response:", response)

# Test the `querystream` function
print("\nTesting `querystream` function...")
try:
    query_id = "test-query-1"
    response_stream = []
    while True:
        reply = client.call_sync('querystream', [f"{query_id}-user-{query_text}", botname, False])
        if reply is None:
            break
        response_stream.append(reply)
        print("Streamed Reply:", reply)
    print("Complete Streamed Response:", "".join(response_stream))
except Exception as e:
    print("Error in `querystream` test:", e)

# Close the client
client.close()

print("\nTests completed.")
