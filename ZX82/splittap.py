import time
import sys



def loadFile():
    fr = open('c:\\Data\\zxspectrum\\ZXspin\\sudo.tap', 'rb')
    
    fileCnt=1
    while(True):
        byteL=fr.read(1)
        if(not byteL):
            return
        byteM=fr.read(1)
        if(not byteM):
            return
        
        fw = open('c:\\Data\\zxspectrum\\ZXspin\\sudo_'+"{:02d}".format(fileCnt)+'.tap', 'wb')

        fw.write(byteL)
        fw.write(byteM)

        bytesToRead=int.from_bytes(byteL,byteorder=sys.byteorder)+int.from_bytes(byteM,byteorder=sys.byteorder)*256
        print(bytesToRead)

        while(bytesToRead>0):
            byte=fr.read(1)
            fw.write(byte)
            bytesToRead-=1

        fileCnt+=1
        
loadFile()
    
