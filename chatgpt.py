# chatgpt.py
import copy
import sys
import json
import os
import traceback

from epc.server import EPCServer
from revChatGPT.V3 import Chatbot

server = EPCServer(('localhost', 0))

# get from $home/.config/chatgptel.json
configpath = os.path.join(os.getenv('HOME'), '.config', 'chatgptel.json')
bots = json.load(open(configpath, 'r'))

stream_reply = {}

conversations = {}


@server.register_function
def query(query, botname, convo_id='default'):
    global bots
    try:
        if bots[botname]["identity"] == None:
            bots[botname]["identity"] = Chatbot(**bots[botname]["born_setting"])

        # if botname in [maxwell], then reset conversation
        if botname in ['maxwell', 'harrison']:
            bots[botname]["identity"].reset(convo_id=convo_id)
        return bots[botname]["identity"].ask(query, convo_id=convo_id, **bots[botname]["gen_setting"])
    except Exception:
        traceback_str = traceback.format_exc()
        return traceback_str


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

    try:
        if bots[botname]["identity"] == None:
            bots[botname]["identity"] = Chatbot(**bots[botname]["born_setting"])

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


            if botname in ['maxwell', 'harrison']:
                bots[botname]["identity"].reset(convo_id=convo_id)

            stream_reply[query_id] = bots[botname]["identity"].ask_stream(
                query, convo_id=convo_id, **bots[botname]["gen_setting"])
        return next(stream_reply[query_id])
    except StopIteration:
        stream_reply.pop(query_id)
        conversations = copy.deepcopy(bots[botname]["identity"].conversation)
        return None
    except Exception:
        traceback_str = traceback.format_exc()
        return traceback_str


server.print_port()
server.serve_forever()
