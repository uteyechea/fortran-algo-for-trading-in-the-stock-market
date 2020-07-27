import urllib2,time
from datetime import datetime
from decimal import *
import time
import math
import csv
import os
import arrow

num_days_back=30000
end_date = arrow.now().format("YYYY-MM-DD")
start_date = arrow.now()
start_date = start_date.replace(days=(num_days_back*-1)).format("YYYY-MM-DD")

now=datetime.now()
print '%s/%s/%s %s:%s:%s' % (now.month,now.day,now.year,now.hour,now.minute,now.second)
country='NYX'
workingdirexchanges='/mnt/c/vyuex-argonaut/Data/Exchanges/' 
workingdirequities='/mnt/c/vyuex-argonaut/Data/Equities/'

symbolList = []
stockTrue = True

def toE(number):
    return '%.3f' % Decimal(number)
##    return '%s' % number

def Data(symbol):
    global stockTrue
    #url="https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="+symbol+"&outputsize=full&apikey=FJMWDK5YMP77HO71&datatype=csv"
    url="https://www.quandl.com/api/v3/datasets/WIKI/"+symbol+".csv?start_date="+start_date+"&end_date="+end_date+"&order=asd&api_key=Y69yeHKxBA5FNbxvV3ZF"
    
##    url="https://www.quandl.com/api/v3/datatables/"+symbol+".csv?start_date="+start_date+"&end_date="+end_date+"&order=asc&api_key=Y69yeHKxBA5FNbxvV3ZF"

##    url="https://www.quandl.com/api/v3/datasets/EOD/"+symbol+".csv?api_key=Y69yeHKxBA5FNbxvV3ZF"

    
    print url
      
    try:
        response = urllib2.urlopen(url)
        CSV = csv.reader(response)
        mainDatum = []
        for row in CSV:
            newRow = []
            for element in row:
                newRow.append(str(element))
            mainDatum.append(newRow)
    except:
        stockTrue = False
        print 'No data for '+symbol
        return
            
    return mainDatum

      

if __name__ == '__main__':
      
    TotalT = time.clock()

    textFile = open(workingdirexchanges+country+'.txt', 'r')
    del symbolList[:]

    for line in textFile:
        symbolList.append(line)

    textFile.close()

    textSuc = open(workingdirexchanges+country, 'w')
    textFailed = open(workingdirexchanges+country+"ErrorRate.txt", 'w')

    symFailed=0
    symTotal =0

      
    for sym in symbolList:
        start = time.clock()
        symTotal +=1
        
        exch = sym[0:2]
        sym = sym[3:len(sym)-1]
        
        print "Downloading data for "+sym
        q = Data(sym)#'%s-%s-%s' % (now.year,now.month,now.day) ) #One month less gives the correct month

          ####write data into file
        if(stockTrue == True):
            newSym = ""
            for letter in sym:
                if(letter != "."):
                    newSym += letter
                else:
                    break
                            
            textSuc.write(newSym+"\n")     

            textFile = open(workingdirequities+country+"/"+newSym, 'w')
            lenght = int(len(list(q)))-1
            i = 1

            while(i <= lenght):
                #print(i,lenght,q[i][0:5])
                date = ""
                for c in range(10):
                    if(q[i][0][c] != '-'):
                        date += q[i][0][c]
                    
                
##                date = toE(q[i][0])
                open_ = toE(q[i][1])
                high = toE(q[i][2])
                low = toE(q[i][3])
                close = toE(q[i][4])
                volume = toE(q[i][5])
                
                textFile.write(date+","+high+","+low+","+close+","+open_+","+volume+"\n")
                i+=1

            textFile.close()
            end = time.clock()
            timeT = (end-start)
            print "Completed download ... "+str(timeT)+"\n"
              
        else:
            symFailed+=1
            stockTrue = True

    textSuc.close()
    textFailed.write(str(symFailed*100/symTotal)+"%")
    textFailed.close()
            
            
    endT = time.clock()
    tT = (endT-TotalT)
    print tT

