''' github.com/jessicakay '''

import os
import re

home = os.path.expanduser('~')
dtop=''.join([home,"/Desktop/"])

def grabber():
	reportlist = input("url list location:")
	if "https://" in reportlist:
		print("url")
	elif ".txt" in reportlist:
		listread(reportlist)

def listread(reportList):
	engine = input("use wget [w] or curl [c]:")
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

grabber()