# chatgpt.py
from epc.server import EPCServer
from revChatGPT.V3 import Chatbot
import sys

password = sys.argv[1]

server = EPCServer(('localhost', 0))

bots_profile = {
    "ellis": {
        "born_setting": {
            "engine": "gpt-3.5-turbo",
            "system_prompt": "You are a reliable, simple, and faithful code assistant. You can assist people with programming and coding, always providing accurate and dependable code snippets. Additionally, You are capable of helping people debug, refactor, improve, and explain various types of code."
        },
        "gen_setting": {
            "temperature": 0.5,
            "top_p": 0.9,
            "max_tokens": 500,
        },
        "identity": None
    },
    "rogers": {
        "born_setting": {
            "engine": "gpt-4",
            "system_prompt": "You are a reliable, straightforward, and trustworthy chat assistant. You provide accurate and dependable code snippets and are capable of helping people debug, refactor, improve, and explain various types of code. Additionally, you possess profound knowledge and eloquence, offering people a wealth of frontier science information and inspiration."
        },
        "gen_setting": {
            "temperature": 0.7,
            "top_p": 0.9,
            "max_tokens": 500,
        },
        "identity": None
    }
}

stream_reply = {}

@server.register_function
def query(query, botname):
    global bots
    if bots[botname]["identity"] == None:
        bots[botname]["identity"] = Chatbot(api_key=password, **bots[botname]["born_setting"])
    return bots[botname]["identity"].ask(query, **bots[botname]["gen_setting"])

@server.register_function
def querystream(query_with_id, botname):
    global bots
    global stream_reply

    if bots[botname]["identity"] == None:
        bots[botname]["identity"] = Chatbot(api_key=password, **bots[botname]["born_setting"])

    query_id, query = query_with_id.split('-', maxsplit=1)
    if query_id not in stream_reply:
        stream_reply[query_id] = bots[botname]["identity"].ask_stream(query, **bots[botname]["gen_setting"])
    try:
        return next(stream_reply[query_id])
    except StopIteration:
        stream_reply.pop(query_id)
        return None

@server.register_function
def switch_to_chat(chat_uuid, botname):
    global bots
    if bots[botname]["identity"] == None:
        bots[botname]["identity"] = Chatbot(api_key=password, **bots[botname]["born_setting"])
    bots[botname].conversation_id = chat_uuid
    return ""

server.print_port()
server.serve_forever()
