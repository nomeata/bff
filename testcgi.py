#!/usr/bin/python

from BaseHTTPServer import HTTPServer
from CGIHTTPServer import CGIHTTPRequestHandler
import sys

class MyRequestHandler(CGIHTTPRequestHandler):
	def is_cgi(self):
		self.cgi_info = ("","")
		return True

	def translate_path(self, path):
		return sys.argv[1]


server_address = ('', 8000)
http  = HTTPServer(server_address, MyRequestHandler)
http.serve_forever()
