''' github.com/jessicakay '''

import os
import re
import requests

home = os.path.expanduser('~')
dtop=''.join([home,"/Desktop/"])

def listread(reportList):
	engine = input("use wget [w] or curl [c]:")
	os.chdir(''.join([home,"/Downloads/"]))
	with open(reportList, 'r') as lst:
		reports=lst.readlines()
	for i in reports:
		i=i.rstrip()
		x=i[25:len(i)-9]
		print(x)
		z=''.join(re.findall(r'[A-Za-z0-9]+',x))
		print("downloading... ",z)
		if engine=="w" or engine=="":
			os.system(''.join(["wget -O",z," \"",i,"\""]))
		elif engine=="c":
			os.system(''.join(["curl -O -J -L ", i]))
		d=input("next?")
		if d == 'q':
			quit()

def urlgrab(reportlist):
	print("loading...")


reportlist = input("url list location:")
if "http://" in reportlist or "https://" in reportlist:
	urlgrab(reportlist)

elif ".txt" in reportlist:
	listread(reportlist)
