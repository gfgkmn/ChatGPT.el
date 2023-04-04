# chatgpt.py
from epc.server import EPCServer
from revChatGPT.V3 import Chatbot
import sys

password = sys.argv[1]

server = EPCServer(('localhost', 0))

bots = {"gpt-3.5-turbo": None, "gpt-4": None}
stream_reply = {}

@server.register_function
def query(query, gptmodel):
    global bots
    if bots[gptmodel] == None:
        bots[gptmodel] = Chatbot(api_key=password, engine=gptmodel)
    return bots[gptmodel].ask(query, temperature=0.7, top_p=0.9)

@server.register_function
def querystream(query_with_id, gptmodel):
    global bots
    global stream_reply

    if bots[gptmodel] == None:
        bots[gptmodel] = Chatbot(api_key=password, engine=gptmodel)

    query_id, query = query_with_id.split('-', maxsplit=1)
    if query_id not in stream_reply:
        stream_reply[query_id] = bots[gptmodel].ask_stream(query, temperature=0.7, top_p=0.9)
    try:
        return next(stream_reply[query_id])
    except StopIteration:
        stream_reply.pop(query_id)
        return None

@server.register_function
def switch_to_chat(chat_uuid):
    global bot
    if bot == None:
        bot = Chatbot(api_key=password, engine=gptmodel)
    bot.conversation_id = chat_uuid
    return ""

server.print_port()
server.serve_forever()
