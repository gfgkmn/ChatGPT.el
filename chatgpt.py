# chatgpt.py
from epc.server import EPCServer
from revChatGPT.V3 import Chatbot
import sys

password = sys.argv[1]

server = EPCServer(('localhost', 0))

bot = None
stream_reply = None

@server.register_function
def query(query):
    global bot
    if bot == None:
        bot = Chatbot(api_key=password)
    return bot.ask(query)

@server.register_function
def querystream(query):
    global bot
    global stream_reply
    if bot == None:
        bot = Chatbot(api_key=password)
    if stream_reply is None or stream_reply["query"] != query:
        stream_reply = {
            "query": query,
            "generator": bot.ask_stream(query)
        }
    try:
        return next(bot.ask_stream(query))
    except StopIteration:
        stream_reply = None
        return None

@server.register_function
def switch_to_chat(chat_uuid):
    global bot
    if bot == None:
        bot = Chatbot(api_key=password)
    bot.conversation_id = chat_uuid
    return ""

server.print_port()
server.serve_forever()
