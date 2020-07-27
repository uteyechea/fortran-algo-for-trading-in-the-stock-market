import urllib2,time
from datetime import datetime
from decimal import *
import time
import math
import csv
import os

now=datetime.now()
print '%s/%s/%s %s:%s:%s' % (now.month,now.day,now.year,now.hour,now.minute,now.second)
country='EAO'
symbolList = []
stockTrue = True

def toE(number):
      return '%.7e' % Decimal(number)

def yahooData(symbol,fromDate,toDate):
      global stockTrue

      fYear=fMonth=fDay=tYear=tMonth=tDay = ""

      fYear = fromDate[0:4] 
      fMonth = fromDate[6:7] 
      fDay = fromDate[8:10]  

      tYear = toDate[0:4] 
      tMonth = toDate[6:7]  
      tDay = toDate[8:10]  
            
      url = "http://ichart.finance.yahoo.com/table.csv?s="+symbol+"&a="+fMonth+"&b="+fDay+"&c="+fYear+"&d="+tMonth+"&e="+tDay+"&f="+tYear+"&ignore=.csv" 
      print url
      try:
            response = urllib2.urlopen(url)
      except:
            stockTrue = False
            print 'Data Download Error For '+symbol
            return
      
      CSV = csv.reader(response)
      mainDatum = []
      
      for row in CSV:
            newRow = []
            for element in row:
                  newRow.append(str(element))
                  
            mainDatum.append(newRow)
            
      return mainDatum
      


if __name__ == '__main__':
    
    TotalT = time.clock()

    
    textFile = open('/home/ut/GTT/Data/'+country+'.txt', 'r')
    del symbolList[:]

    for line in textFile:  
        symbolList.append(line)
    
    textFile.close()

    textSuc = open('/home/ut/GTT/Data/'+country, 'w')
    textFailed = open('/home/ut/GTT/Data/'+country+"ErrorRate.txt", 'w')

    symFailed=0
    symTotal =0


    for sym in symbolList:
                  start = time.clock()
                  symTotal +=1
                  
                  exch = sym[0:2]
                  sym = sym[3:len(sym)-1]
                  
                  print "Downloading data for "+sym
                  q = yahooData(sym,'2005-01-01','%s-%s-%s' % (now.year,now.month,now.day) ) #One month less gives the correct month

                  ####write data into file
                  if(stockTrue == True):
                        newSym = ""
                        for letter in sym:
                              if(letter != "."):
                                    newSym += letter
                              else:
                                    break
                                    
                        
                        textSuc.write(newSym+"\n")     

                        textFile = open('/home/ut/GTT/Securities/'+country+"/"+newSym, 'w')
                        lenght = int(len(list(q)))-1
                        i = 1

                        while(i <= lenght):
                               date = ""
                               for c in range(10):
                                     if(q[i][0][c] != '-'):
                                           date += q[i][0][c]

                               open_ = toE(q[i][1])
                               high = toE(q[i][2])
                               low = toE(q[i][3])
                               close = toE(q[i][4])
                               volume = toE(q[i][5])

                               textFile.write(date+"   "+high+"   "+low+"   "+close+"   "+open_+"   "+volume+"   "+"\n")
                               i=i+1


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

