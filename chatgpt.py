# chatgpt.py
import concurrent.futures
import copy
import glob
import json
import os
import re
import subprocess
import textwrap
import traceback

from chatgpt_v3 import Chatbot
from epc.server import EPCServer


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


def is_valid_remote_file_path(path):
    """Check if the remote path is a valid file using ssh."""
    try:
        # Parse /sshx:machine:/path format
        parts = path.split(':', 2)
        if len(parts) != 3 or not parts[0] == '/sshx':
            return False

        machine = parts[1]
        remote_path = parts[2]

        # Use ssh to check if file exists and is a regular file
        cmd = ['ssh', machine, f'test -f "{remote_path}"']
        result = subprocess.run(cmd, capture_output=True, timeout=10)
        return result.returncode == 0
    except (subprocess.TimeoutExpired, subprocess.SubprocessError, IndexError):
        return False


def is_valid_file_path(path):
    """Check if the path is a valid file (not a directory) and exists."""
    # Handle remote paths with /sshx:machine:/path format
    if path.startswith('/sshx:'):
        return is_valid_remote_file_path(path)
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


def read_remote_file_content(file_path):
    """Reads remote file content using ssh."""
    try:
        # Parse /sshx:machine:/path format
        parts = file_path.split(':', 2)
        if len(parts) != 3 or not parts[0] == '/sshx':
            raise ValueError(f"Invalid remote path format: {file_path}")

        machine = parts[1]
        remote_path = parts[2]

        # Use ssh to read file content
        cmd = ['ssh', machine, f'cat "{remote_path}"']
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)

        if result.returncode != 0:
            raise Exception(f"Failed to read remote file: {result.stderr}")

        return result.stdout
    except (subprocess.TimeoutExpired, subprocess.SubprocessError) as e:
        raise Exception(f"Error reading remote file {file_path}: {str(e)}")


def read_file_content(file_path):
    """Reads file content safely."""
    # Handle remote paths with /sshx:machine:/path format
    if file_path.startswith('/sshx:'):
        return read_remote_file_content(file_path)
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

    # Find all matches for @[[file_path]], #[[folder_path]], and @project
    file_matches = re.findall(r"@\[\[([^\]]+)\]\]", query)
    folder_matches = re.findall(r"#\[\[([^\]]+)\]\]", query)
    git_match = re.search(r"\@project", query)

    # Process each @[[file_path]]
    for file_path in file_matches:
        if is_valid_file_path(file_path):
            content = read_file_content(file_path)
            file_contents.append(f"### File: {file_path} ###\n{content}")
            query = query.replace(f"@[[{file_path}]]", f"{file_path}").strip()
            display_files += file_path + " "

    # Process each #[[folder_path]]
    for folder_path in folder_matches:
        if is_valid_folder_path(folder_path):
            folder_files = get_files_from_folder(folder_path)
            for file_path in folder_files:
                content = read_file_content(file_path)
                file_contents.append(f"### File: {file_path} ###\n{content}")
            query = query.replace(f"#[[{folder_path}]]", f"{folder_path}").strip()
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
        extracted_code = "--- FILE CONTEXT ---\n\n" + "\n\n--- FILE CONTEXT ---\n\n".join(
            file_contents)
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
def querystream(query_with_id, botname, reuse, convo_id='default', timeout=6):
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

            if reuse and convo_id in conversations:
                if bots[botname]["identity"].conversation[convo_id][-1][
                        'role'] == "assistant":
                    bots[botname]["identity"].rollback(2, convo_id=convo_id)
                else:
                    bots[botname]["identity"].rollback(1, convo_id=convo_id)

            if botname in ['maxwell', 'harrison']:
                bots[botname]["identity"].reset(convo_id=convo_id)

            # Apply timeout to stream creation
            def create_stream():
                query_processed, display_files = process_syntax(query)
                stream = bots[botname]["identity"].ask_stream(
                    query_processed, convo_id=convo_id, **bots[botname]["gen_setting"])
                return stream, display_files

            stream, display_files = create_stream()
            stream_reply[query_id] = stream

            # Get first message with timeout
            first_message = next(stream)
            return {"type": 0, "message": display_files + first_message}
        else:
            # Get next message with timeout
            next_message = next(stream_reply[query_id])
            return {"type": 0, "message": next_message}

    except StopIteration:
        stream_reply.pop(query_id, None)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return {"type": 0, "message": None}
    except TimeoutError as e:
        # Clean up on timeout
        if query_id in stream_reply:
            stream_reply.pop(query_id, None)
        return {"type": 1, "message": f"Timeout: {str(e)}"}
    except Exception as e:
        if query_id in stream_reply:
            stream_reply.pop(query_id, None)
        return {"type": 1, "message": f"Exception: {str(e)}\n{traceback.format_exc()}"}


port = server.server_address[1]  # Get the port number
with open("epc_port.txt", "w") as f:
    f.write(str(port))
server.print_port()
server.serve_forever()
