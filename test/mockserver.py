#!/usr/bin/env python

# Largely based on:
# http://www.codeproject.com/Articles/462525/Simple-HTTP-Server-and-Client-in-Python

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os

class StatusCodeMockery (BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(400)
        self.end_headers()

def run():
    print('starting server')
    server_address = ('127.0.0.1', 8080)
    httpd = HTTPServer(server_address, StatusCodeMockery)
    print('server is running')
    httpd.serve_forever()

if __name__ == '__main__':
    run()

#import sys
#import BaseHTTPServer
#from SimpleHTTPServer import SimpleHTTPRequestHandler
# HandlerClass = SimpleHTTPRequestHandler
# ServerClass  = BaseHTTPServer.HTTPServer
# Protocol     = "HTTP/1.0"
# 
# if sys.argv[1:]:
#     port = int(sys.argv[1])
# else:
#     port = 8000
# server_address = ('127.0.0.1', port)
# 
# HandlerClass.protocol_version = Protocol
# httpd = ServerClass(server_address, HandlerClass)
# httpd.send_error(400)
# 
# sa = httpd.socket.getsockname()
# print "Serving HTTP on", sa[0], "port", sa[1], "..."
# httpd.serve_forever
