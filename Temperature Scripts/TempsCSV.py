#This script reads temperatures from a .csv file and sends them over serial at defined interval (default 1 minute)
import csv
import serial
import time

#Edit these variables to your .csv file, serial device address, and interval (in seconds)
tempFile = "scripps.csv"
serAdd = '/dev/tty.usbmodem14101'
interval = 60

#Setting up serial connection  
ser = serial.Serial(serAdd, 9600)

with open(tempFile, "r") as file:
    csvReader = csv.reader(file)
    header = next(csvReader)
    tempIndex = header.index("temp")
    
    for row in csvReader:
        currTemp = row[tempIndex]
        sendStr = str('<' + str(currTemp) + '>')
        ser.write(sendStr.encode('utf-8'))
        time.sleep(interval)
