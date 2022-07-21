#A script that will continuously pull and send over serial up-to-date water temperatures from the Scripps Institute pier in Lajolla, Ca.
from datetime import datetime,timezone,timedelta
import pandas as pd
import serial
import time


#Change the path to path of your arduino board
ser = serial.Serial('/dev/tty.usbmodem14101', 9600)
deltaT = timedelta(minutes=60)
currTemp = 18.0

while True:
   now = datetime.now(timezone.utc)
   before = now - deltaT
   after = now
   URL = ("https://erddap.sccoos.org/erddap/tabledap/autoss.htmlTable?time%2Ctemperature&time%3E="
      + str(before.year) + "-" + str(before.month) + "-" + str(before.day) + "T" + str(before.hour) 
      + "%3A" + str(before.minute) + "%3A00Z&time%3C=" + str(after.year) + "-" + str(after.month) + "-" + str(after.day) 
      + "T" + str(after.hour) + "%3A" + str(after.minute) + "%3A00Z")
   try:
     tempdat = pd.read_html(URL)
     currTemp = float(tempdat[-1].iloc[-1,-1])
     sendStr = str('<' + str(currTemp) + '>')
     ser.write(sendStr.encode('utf-8'))
     time.sleep(60)
   except:
     print("No Connection"+str(datetime.now()))
     time.sleep(5)

