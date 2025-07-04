# chatgpt.py
import copy
import glob
import json
import os
import re
import textwrap
import traceback

from chatgpt_v3 import Chatbot
from epc.server import EPCServer

import signal
from contextlib import contextmanager

@contextmanager
def timeout_context(seconds):
    def timeout_handler(signum, frame):
        raise TimeoutError(f"Operation timed out after {seconds} seconds")

    # Set the signal handler
    old_handler = signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(seconds)

    try:
        yield
    finally:
        # Restore the old handler and cancel the alarm
        signal.alarm(0)
        signal.signal(signal.SIGALRM, old_handler)

server = EPCServer(('localhost', 0))

# get from $home/.config/chatgptel.json
configpath = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
bots = json.load(open(configpath, 'r'))

stream_reply = {}
conversations = {}


@server.register_function
def clear_streams():
    global stream_reply
    stream_reply = {}
    return True


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


def get_git_root():
    """Find the current Git repository root directory."""
    git_root = os.popen("git rev-parse --show-toplevel 2>/dev/null").read().strip()
    return git_root if os.path.isdir(git_root) else None


def extract_valid_path(text, path_checker):
    """Extracts a valid file/folder path from within @[[path]] or #[[path]]"""
    match = re.match(r"\[\[([^\]]+)\]\]", text)  # Extract path inside [[ ]]
    if match:
        path = match.group(1).strip()
        if path_checker(path):
            return path
    return None


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


def process_syntax(query):
    """Extracts @[[file]], #[[folder]], and @project content while modifying the query."""
    file_contents = []
    display_files = ""

    # Match @[[file_path]], #[[folder_path]], and @project
    file_match = re.search(r"@\[\[([^\]]+)\]\]", query)
    folder_match = re.search(r"#\[\[([^\]]+)\]\]", query)
    git_match = re.search(r"\@project", query)

    # Process @[[file_path]]
    if file_match:
        file_path = file_match.group(1)
        if is_valid_file_path(file_path):
            content = read_file_content(file_path)
            file_contents.append(f"### File: {file_path} ###\n{content}")
            query = query.replace(f"@[[{file_path}]]", "{Files_show_above}").strip()
            display_files += file_path + " "

    # Process #[[folder_path]]
    if folder_match:
        folder_path = folder_match.group(1)
        if is_valid_folder_path(folder_path):
            folder_files = get_files_from_folder(folder_path)
            for file_path in folder_files:
                content = read_file_content(file_path)
                file_contents.append(f"### File: {file_path} ###\n{content}")
            query = query.replace(f"#[[{folder_path}]]", "{Files_show_above}").strip()
            display_files += " ".join(folder_files) + " "

    # Process @project command
    if git_match:
        git_root = get_git_root()
        if git_root:
            project_files = get_files_from_folder(git_root)
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

        query, display_files = process_syntax(query)
        return display_files + bots[botname]["identity"].ask(
            query, convo_id=convo_id, **bots[botname]["gen_setting"])

    except Exception:
        return traceback.format_exc()


@server.register_function
def querystream(query_with_id, botname, reuse, convo_id='default', timeout=5):
    global bots, stream_reply, conversations

    try:
        with timeout_context(timeout):
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
                    if bots[botname]["identity"].conversation[convo_id][-1][
                            'role'] == "assistant":
                        bots[botname]["identity"].rollback(2, convo_id=convo_id)
                    else:
                        bots[botname]["identity"].rollback(1, convo_id=convo_id)

                if botname in ['maxwell', 'harrison']:
                    bots[botname]["identity"].reset(convo_id=convo_id)

                query, display_files = process_syntax(query)
                stream_reply[query_id] = bots[botname]["identity"].ask_stream(
                    query, convo_id=convo_id, **bots[botname]["gen_setting"])

                return {"type": 0, "message": display_files + next(stream_reply[query_id])}
            else:
                return {"type": 0, "message": next(stream_reply[query_id])}
    except StopIteration:
        stream_reply.pop(query_id)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return {"type": 0, "message": None}
    except TimeoutError as e:
        # Clean up on timeout
        if query_id in stream_reply:
            stream_reply.pop(query_id)
        return {"type": 1, "message": f"Timeout: {str(e)}"}
    except Exception as e:
        return {"type": 1, "message": f"Exception: {str(e)}\n{traceback.format_exc()}"}


@server.register_function
def query_manager(query_with_id, botname, reuse, convo_id='default'):

    # Initialize buffer storage if not exists
    if not hasattr(query_manager, 'buffers'):
        query_manager.buffers = {}
    if not hasattr(query_manager, 'invoke_counts'):
        query_manager.invoke_counts = {}

    # Initialize buffer and reuse count for new query_id
    if query_with_id not in query_manager.buffers:
        query_manager.buffers[query_with_id] = []
        query_manager.invoke_counts[query_with_id] = 0
    elif query_manager.invoke_counts[query_with_id] < 0:
        query_manager.buffers.pop(query_with_id, None)
        query_manager.invoke_counts.pop(query_with_id, None)
        return {"type": 0, "message": None}
    else:
        query_manager.invoke_counts[query_with_id] += 1

    if query_manager.invoke_counts[query_with_id] < 40:
        return querystream(query_with_id, botname, reuse, convo_id)
    else:
        current_return = querystream(query_with_id, botname, reuse, convo_id)
        while current_return["type"] == 0 and current_return["message"] is not None:
            query_manager.buffers[query_with_id].append(current_return["message"])
            current_return = querystream(query_with_id, botname, reuse, convo_id)
        if current_return["type"] == 1:
            return current_return
        else:
            query_manager.invoke_counts[query_with_id] = -1
            return {"type": 0, "message": "".join(query_manager.buffers[query_with_id])}

port = server.server_address[1]  # Get the port number
with open("epc_port.txt", "w") as f:
    f.write(str(port))
server.print_port()
server.serve_forever()
