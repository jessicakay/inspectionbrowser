''' github.com/jessicakay '''

import os
import re
import requests
import tabula

home = os.path.expanduser('~')
dtop=''.join([home,"/Desktop/"])

def listread(reportList):
	os.chdir(''.join([home, "/Downloads/"]))
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

def urlgrab(reportlist):
	os.chdir(''.join([home, "/Downloads/"]))
	targetfile=requests.get(reportlist)

def pdfgrab(reportlist):
	targfile=tabula.read_pdf(reportlist,stream=True,pages="all")
	print(len(targfile)," tables read into memory")

reportlist = input("url list location:")
if "http://" in reportlist or "https://" in reportlist:
	urlgrab(reportlist)
elif ".txt" in reportlist:
	listread(reportlist)
if ".pdf" in reportlist:
	pdfgrab(reportlist)

quit()