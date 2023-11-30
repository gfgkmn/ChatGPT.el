# chatgpt.py
import copy
import sys

from epc.server import EPCServer
from revChatGPT.V3 import Chatbot

password = sys.argv[1]

server = EPCServer(('localhost', 0))

bots = {
    "ellis": {
        "born_setting": {
            "engine": "gpt-3.5-turbo",
            "system_prompt": "You are a reliable, simple, and faithful code assistant. You can assist people with programming and coding, always providing accurate and dependable code snippets. Additionally, You are capable of helping people debug, refactor, improve, and explain various types of code. When providing code output, please use the ```language=xxx syntax to indicate the language of the code."
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
            "engine": "gpt-4-1106-preview",
            "system_prompt": "You are a reliable, straightforward, and trustworthy chat assistant. You provide accurate and dependable code snippets and are capable of helping people debug, refactor, improve, and explain various types of code. Additionally, you possess profound knowledge and eloquence, offering people a wealth of frontier science information and inspiration.When providing code output, please use the ```language=xxx syntax to indicate the language of the code."
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

conversations = {}


@server.register_function
def query(query, botname):
    global bots
    if bots[botname]["identity"] == None:
        bots[botname]["identity"] = Chatbot(api_key=password,
                                            **bots[botname]["born_setting"])
    return bots[botname]["identity"].ask(query, **bots[botname]["gen_setting"])


@server.register_function
def querystream(query_with_id, botname, reuse, convo_id='default'):
    #  record parameters into a log.txt file

    # with open('log.txt', 'a') as f:
    #     print(query_with_id, file=f)
    #     print(botname, file=f)
    #     print(reuse, file=f)
    #     print(convo_id, file=f)
    #     print('----------------', file=f)

    global bots
    global stream_reply
    global conversations

    if bots[botname]["identity"] == None:
        bots[botname]["identity"] = Chatbot(api_key=password,
                                            **bots[botname]["born_setting"])

    query_with_id = query_with_id.split('-', maxsplit=5)
    query = query_with_id[5]
    query_id = '-'.join(query_with_id[:5])
    if query_id not in stream_reply:

        bots[botname]["identity"].conversation = conversations

        if reuse == True:
            assert convo_id in conversations
            if bots[botname]["identity"].conversation[convo_id][-1][
                    'role'] == "assistant":
                bots[botname]["identity"].rollback(2, convo_id=convo_id)
            else:
                bots[botname]["identity"].rollback(1, convo_id=convo_id)

        stream_reply[query_id] = bots[botname]["identity"].ask_stream(
            query, convo_id=convo_id, **bots[botname]["gen_setting"])
    try:
        return next(stream_reply[query_id])
    except StopIteration:
        stream_reply.pop(query_id)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return None


server.print_port()
server.serve_forever()
