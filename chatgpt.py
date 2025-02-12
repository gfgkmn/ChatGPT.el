# chatgpt.py
import copy
import json
import os
import traceback
import glob
import re
import textwrap

from epc.server import EPCServer
from revChatGPT.V3 import Chatbot

server = EPCServer(('localhost', 0))

# get from $home/.config/chatgptel.json
configpath = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
bots = json.load(open(configpath, 'r'))

stream_reply = {}
conversations = {}

def format_display_files(display_files, width=90):
    """Formats the display_files string to fit within the given width."""
    if not display_files.strip():
        return ""

    header = "Files Processed:\n" + "-" * width
    formatted_files = textwrap.fill(display_files, width=width)
    return f"{header}\n{formatted_files}\n{'-' * width}\n"

def is_valid_file_path(path):
    """Check if the path is a valid file (not a directory) and exists."""
    return os.path.isfile(path)

def is_valid_folder_path(path):
    """Check if the path is a valid directory and exists."""
    return os.path.isdir(path)

def extract_valid_paths(text, path_checker):
    """Extracts only valid file/folder paths, stopping at the first invalid path."""
    paths = text.strip().split()
    valid_paths = []
    extracted_string = ""

    for path in paths:
        cleaned_path = path.strip().rstrip(",.!?")  # Strip trailing punctuation
        if path_checker(cleaned_path):
            valid_paths.append(cleaned_path)
        else:
            break  # Stop at the first invalid path

    extracted_string = " ".join(valid_paths)
    return valid_paths, extracted_string

def read_file_content(file_path):
    """Reads file content safely."""
    with open(file_path, "r", encoding="utf-8") as f:
        return f.read()

def get_files_from_folder(folder_path, extensions=[".py", ".json", ".md"]):
    """Gets all files within a folder that match the specified extensions."""
    files = []
    for ext in extensions:
        files.extend(glob.glob(os.path.join(folder_path, f"**/*{ext}"), recursive=True))
    return files

def process_at_syntax(query):
    """Extracts @file, @folder, and @project content while modifying the query accordingly."""
    file_contents = []
    display_files = ""

    # Match @file, @folder, and @project
    file_match = re.search(r"@file\s+([^\n@]+)", query)
    folder_match = re.search(r"@folder\s+([^\n@]+)", query)
    project_match = re.search(r"@project", query)

    # Process @file command
    if file_match:
        raw_paths = file_match.group(1)
        valid_file_paths, file_extracted_text = extract_valid_paths(raw_paths, is_valid_file_path)

        for path in valid_file_paths:
            content = read_file_content(path)
            file_contents.append(f"### File: {path} ###\n{content}")

        if file_extracted_text:
            query = query.replace(f"@file {file_extracted_text}", "{Files_show_above}").strip()
            display_files += file_extracted_text + " "

    # Process @folder command
    if folder_match:
        raw_paths = folder_match.group(1)
        valid_folder_paths, folder_extracted_text = extract_valid_paths(raw_paths, is_valid_folder_path)

        for folder in valid_folder_paths:
            folder_files = get_files_from_folder(folder)
            for file_path in folder_files:
                content = read_file_content(file_path)
                file_contents.append(f"### File: {file_path} ###\n{content}")

        if folder_extracted_text:
            query = query.replace(f"@folder {folder_extracted_text}", "{Files_show_above}").strip()
            display_files += folder_extracted_text + " "

    # Process @project command
    if project_match:
        project_files = get_files_from_folder(".")  # Assume current directory as project root
        query = query.replace("@project", "{Files_show_above}").strip()

        for path in project_files:
            content = read_file_content(path)
            file_contents.append(f"### File: {path} ###\n{content}")
        display_files += ' '.join(project_files)


    # Construct new query with delimiters
    if file_contents:
        extracted_code = "\n\n--- FILE CONTEXT ---\n\n".join(file_contents)
        query = f"{extracted_code}\n\n--- USER QUERY ---\n\n{query}"

    return query, format_display_files(display_files)


@server.register_function
def query(query, botname, convo_id='default'):
    global bots
    try:
        if bots[botname]["identity"] is None:
            bots[botname]["identity"] = Chatbot(**bots[botname]["born_setting"])

        # Reset conversation for certain bots
        if botname in ['maxwell', 'harrison']:
            bots[botname]["identity"].reset(convo_id=convo_id)

        query, display_files = process_at_syntax(query)
        return display_files + bots[botname]["identity"].ask(query, convo_id=convo_id, **bots[botname]["gen_setting"])

    except Exception:
        return traceback.format_exc()


@server.register_function
def querystream(query_with_id, botname, reuse, convo_id='default'):
    global bots, stream_reply, conversations

    try:
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

            query, display_files = process_at_syntax(query)
            stream_reply[query_id] = bots[botname]["identity"].ask_stream(
                query, convo_id=convo_id, **bots[botname]["gen_setting"])

            return {"type": 0, "message": display_files + next(stream_reply[query_id])}
        else:
            return {"type": 0, "message": next(stream_reply[query_id])}
    except StopIteration:
        stream_reply.pop(query_id)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return {"type": 0, "message": None}
    except Exception as e:
        return {"type": 1, "message": f"Exception: {str(e)}\n{traceback.format_exc()}"}

# port = server.server_address[1]  # Get the port number
# with open("epc_port.txt", "w") as f:
#     f.write(str(port))
server.print_port()
server.serve_forever()
