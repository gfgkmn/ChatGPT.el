# chatgpt.py

from epc.server import EPCServer
from revChatGPT.V3 import Chatbot
import sys

password = sys.argv[1]

server = EPCServer(('localhost', 0))

bot = None

@server.register_function
def query(query):
    global bot
    if bot == None:
        bot = Chatbot(api_key=password)
    return bot.ask(query)

server.print_port()
server.serve_forever()
