# chatgpt.py
import copy
import sys
import json
import os
import traceback
import glob
import re

from epc.server import EPCServer
from revChatGPT.V3 import Chatbot

server = EPCServer(('localhost', 0))

# get from $home/.config/chatgptel.json
configpath = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
bots = json.load(open(configpath, 'r'))

stream_reply = {}

conversations = {}

def read_file_content(file_path):
    """Reads file content safely."""
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            return f.read()
    except Exception as e:
        return f"Error reading {file_path}: {str(e)}"

def get_project_files(directory=".", extensions=[".py", ".json", ".md"]):
    """Gets all files with the given extensions in the directory recursively."""
    files = []
    for ext in extensions:
        files.extend(glob.glob(f"{directory}/**/*{ext}", recursive=True))
    return files

def process_at_syntax(query):
    """Extracts @file and @project content and returns a modified query."""
    file_contents = []

    # Match @file <path1> <path2> and @project
    file_match = re.findall(r"@file\s+([\S ]+?)(?=\s*@|$)", query)
    project_match = re.search(r"@project", query)

    # Process @file command
    if file_match:
        file_paths = file_match[0].split()  # Extract file paths
        for path in file_paths:
            content = read_file_content(path)
            file_contents.append(f"### File: {path} ###\n{content}")

    # Process @project command
    if project_match:
        project_files = get_project_files()
        for path in project_files:
            content = read_file_content(path)
            file_contents.append(f"### File: {path} ###\n{content}")

    # Construct new query with delimiters
    if file_contents:
        extracted_code = "\n\n--- FILE CONTEXT ---\n\n".join(file_contents)
        query = re.sub(r"@file\s+[\S ]+?|@project", "above files", query)  # Remove @file/@project syntax from query
        query = f"{extracted_code}\n\n--- USER QUERY ---\n\n{query.strip()}"

    return query


@server.register_function
def query(query, botname, convo_id='default'):
    global bots
    try:
        if bots[botname]["identity"] == None:
            bots[botname]["identity"] = Chatbot(**bots[botname]["born_setting"])

        # if botname in [maxwell], then reset conversation
        if botname in ['maxwell', 'harrison']:
            bots[botname]["identity"].reset(convo_id=convo_id)

        query = process_at_syntax(query)
        return bots[botname]["identity"].ask(query, convo_id=convo_id, **bots[botname]["gen_setting"])
    except Exception:
        traceback_str = traceback.format_exc()
        return traceback_str


@server.register_function
def querystream(query_with_id, botname, reuse, convo_id='default'):
    # record parameters into a log.txt file
    # Uncomment the logging block if needed:
    # with open('log.txt', 'a') as f:
    #     print(query_with_id, file=f)
    #     print(botname, file=f)
    #     print(reuse, file=f)
    #     print(convo_id, file=f)
    #     print('----------------', file=f)

    global bots
    global stream_reply
    global conversations

    try:
        # Initialize chatbot if not already created
        if bots[botname]["identity"] is None:
            bots[botname]["identity"] = Chatbot(**bots[botname]["born_setting"])

        # Parse query ID and query
        query_with_id = query_with_id.split('-', maxsplit=5)
        query = query_with_id[5]
        query_id = '-'.join(query_with_id[:5])

        if query_id not in stream_reply:
            bots[botname]["identity"].conversation = conversations

            if reuse:
                assert convo_id in conversations
                if bots[botname]["identity"].conversation[convo_id][-1]['role'] == "assistant":
                    bots[botname]["identity"].rollback(2, convo_id=convo_id)
                else:
                    bots[botname]["identity"].rollback(1, convo_id=convo_id)

            if botname in ['maxwell', 'harrison']:
                bots[botname]["identity"].reset(convo_id=convo_id)

            query = process_at_syntax(query)
            stream_reply[query_id] = bots[botname]["identity"].ask_stream(
                query, convo_id=convo_id, **bots[botname]["gen_setting"])

        return {"type": 0, "message": next(stream_reply[query_id])}
    except StopIteration:
        # Handle StopIteration and cleanup
        stream_reply.pop(query_id)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return {"type": 0, "message": None}
    except Exception as e:
        # Handle any other exceptions
        error_info = f"Exception: {str(e)}\n{traceback.format_exc()}"
        return {"type": 1, "message": error_info}

# port = server.server_address[1]  # Get the port number
# with open("epc_port.txt", "w") as f:
#     f.write(str(port))
server.print_port()
server.serve_forever()
