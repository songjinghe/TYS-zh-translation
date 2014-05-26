#!/usr/bin/python3
import sys

def MyCount(string):
    tmpStr = ""
    for c in string:
        if ord(c)>=255 and (c not in "，。！￥（）《》【】『』、？～·：“”‘’；"): tmpStr += c
    return len(tmpStr)

if __name__=="__main__":
    # print(sys.argv);exit();
    count=0
    if len(sys.argv)>0:
        for fileName in sys.argv[1:] :
            f=open(fileName,'r')
            count += MyCount(f.read())
            f.close()
    print(count)
    # print(len(s))






