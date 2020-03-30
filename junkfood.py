import os
import csv
import re
import docx
import pandas

def gettext(report_files,sel):
    global logCSV, alltext
    if sel=='a':
        print("loading...",prt)
        fileName = prt
    else:
        fileName = report_files[int(sel) - 1]
    fullPath = ''.join([filePath, fileName])
    doc = []
    report = docx.Document(fullPath)
    for paragraph in report.paragraphs:
        doc.append(paragraph.text)

    strongs = re.findall(r'[0-9.-]+', str(doc))
    print(str(len(str(strongs))) + " strings evaluated")
    logCSV = ''.join([filePath, "codearchive.csv"])
    alltext = []
    for strn in strongs:
        if "." in strn and re.search("[0-9]", strn):
            alltext.append(strn)
    getColHead(fullPath,alltext)

def getColHead(fullPath,alltext):
    doc = []
    report = docx.Document(fullPath)
    title  = report.core_properties.title
    print("the default header is ", title)
    print("the create data is: ", str(report.core_properties.created))
    thename=input("use this? ")
    if thename == "y":
        title=re.sub(r"\s", "_", title)
        titleHead=[]
        for character in title:
            if character.isalnum()==True or character=='_':
                titleHead.append(str(character))
        titleHead=''.join(titleHead)
        head=re.sub("/","_",titleHead)
        head=str(head)
    else:
        newname=input("new column name: ")
        head=str(newname)
    print("using: ",head)
    writeSheet(head,fullPath,alltext)

def loader():
    global switch
    global i, data, fullPath, logCSV
    global desktopfiles, alltext, doc, strongs, home, filePath, report_files, sel
    switch=0
    home = (os.path.expanduser('~'))
    filePath = ''.join([home, "/Downloads/South Bay/"])
    os.chdir(filePath)
    desktopfiles = os.listdir(filePath)
    i = 1
    report_files = []
    for file in desktopfiles:
        if "~" not in file and "docx" in file:
            print(i, " - ", str(file))
            report_files.append(str(file))
            i += 1
    sel = input("number: ")
    if sel == 'u':
        print("coming soon!")
    if sel == 'a':
        allports(report_files)
    elif sel.isdigit()==True:
        if (int(sel)-1) < int(len(report_files)):
            gettext(report_files,sel)
        else:
            prompt=''.join(["number between 1 and ",str(len(report_files))])
            print(prompt)
            loader()
    else:
        print("invalid option")
        quit()

def allports(report_files):
    global prt
    fileNum = 0
    for prt in report_files:
        fullPath=prt
        sel="a"
        gettext(prt,sel)

def writeSheet(head,fullPath,alltext):
    global styledata, data
    global freq
    global report_name, create_date
    data=[]
    styledata = open(fullPath, 'rb')
    document = docx.Document(styledata)
    core_properties = document.core_properties
    create_date = str(core_properties.created)
    report_name = str(core_properties.title)
    data=pandas.read_csv(logCSV,error_bad_lines=False)
    numCols=len(data.columns)
    newCol=pandas.Series([])
    i=1
    for i in range(len(alltext)):
        newCol[i]=alltext[i]
    data.insert(numCols,head,newCol,allow_duplicates=True)
    with open(logCSV,'r'):
        data.to_csv(logCSV,index=False,float_format='%g',header=True)
    print("Report created: " + create_date+" "+report_name+"\n")
    print(str(len(alltext))+" code violations found")
    print(str(numCols+1), " samples in dataset\ntop values: \n")
    styledata.close()
    freq=(newCol.value_counts([]))
    print(freq.head(5))
    freq=pandas.Series.to_frame(freq)
    buildDB()

def buildDB():
    freqT = pandas.DataFrame.transpose(freq)
    freqT.insert(1,"name",report_name)
    freqT.insert(1,"date",create_date)
    print("\nfreq table extracted:\t\n", freqT)
    newSheet = ''.join([home,"/Downloads/South Bay/","sheet.csv"])
    ns_exists=os.path.exists(newSheet)
    if ns_exists==True:
        ns_file=pandas.read_csv('sheet.csv',sep=',')
        print("dataset loaded.")
        new=pandas.merge(ns_file,freqT,how='outer')
        new.to_csv('sheet.csv')
    elif ns_exists==False:
        print("no such file")
        freqT.to_csv('sheet.csv',index=False)

loader()