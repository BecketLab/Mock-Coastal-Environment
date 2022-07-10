//Version 2 of Mock Coastal Envionment code for Arduino Mega 2560
//Requires a DS18b20 type thermometer wired to digital pin 7, a 5v relay controlling cooling device set to digital pin 6, and two 5v relays controlling heating devices set to digital pins 8 and 9. Serial input provided by usb port.

// include the libraries:
#include <LiquidCrystal_I2C.h>
#include <OneWire.h>
#include <DallasTemperature.h>

//initialise the numbers of the interface pins (pin 3 for IR LED) 
#define ONE_WIRE_BUS 7
#define RELAYH1 8
#define RELAYH2 9
#define RELAYC  6

const unsigned long intervalTime = 120000;  //intervals in millisecs 600000 for ten minutes (now at 2 minutes)

const unsigned long intervalCool = 30000;   //Time buffer between last time chiller turned on (currently 30 secs)

float setTemp = 18.4;
int intervalSteps = 1;
static float tempC = -1;
unsigned long startTime = 0;
unsigned long startCool = 0;
const byte numChars = 32;
char receivedChars[numChars];
char tempChars[numChars];        // temporary array for use when parsing
boolean newData = false;

//Define degree symbol for lcd display
byte Degree[] = {
  B00111,
  B00101,
  B00111,
  B00000,
  B00000,
  B00000,
  B00000,
  B00000
};

LiquidCrystal_I2C lcd(0x27, 20, 4);

// Create a new instance of the oneWire class to communicate with any OneWire device:
OneWire oneWire(ONE_WIRE_BUS);

// Pass the oneWire reference to DallasTemperature library:
DallasTemperature sensors(&oneWire);
 
void setup() {  //Initializes wiring pins

  Serial.begin(9600);
  startTime = millis();
  pinMode(RELAYH1, OUTPUT);
  pinMode(RELAYH2, OUTPUT);
  pinMode(RELAYC, OUTPUT);
  
  // Start up the Thermometer:
  sensors.begin(); 
  // set up the LCD's inital readout 
  lcd.init();
  lcd.backlight();
  lcd.setCursor(0, 0);
  lcd.print("Pearl v2");
  lcd.setCursor(11, 0);
  lcd.print("Step: ");
  lcd.setCursor(17, 0);
  lcd.print(intervalSteps); 
  lcd.createChar(0, Degree); //Create degree symbol
  lcd.setCursor(0, 1);
  lcd.print("Set Temp: ");
  lcd.print(setTemp);
  lcd.write(byte(0)); //print the degree symbol
  lcd.print("C");
  lcd.setCursor(0, 2);
  lcd.print("Temp: ");
  lcd.print(tempC);
  lcd.write(byte(0)); 
  
} 
 
void loop() {  //Main loop looks for incoming temperatures, maintains temperature

  recvWithStartEndMarkers();
  if (newData == true) {
        strcpy(tempChars, receivedChars);
            // this temporary copy is necessary to protect the original data
            //   because strtok() used in parseData() replaces the commas with \0
        parseData();
        //showParsedData();
        newData = false;
  } 
  GetTemp();
  AdjustTemp();
  
}


void GetTemp() {                    // Gets temp from sensors, stores in tempC, updates display
  sensors.requestTemperatures();
  tempC = sensors.getTempCByIndex(0);
  lcd.setCursor(6, 2);    
  lcd.print(tempC); 
  lcd.write(byte(0)); // print the custom character
  lcd.print("C ");
}


void AdjustTemp() {
  unsigned long curCool = millis();
  unsigned long elapsedCool = curCool - startCool;
  
  if(tempC < (setTemp-0.75)) {
    digitalWrite(RELAYH1, 0);
    digitalWrite(RELAYH2, 0);
    digitalWrite(RELAYC, 1);
    lcd.setCursor(0, 3);
    lcd.print("      Heating");
    while (tempC < (setTemp-0.6)) {
     GetTemp();
     //CheckInterval();
    }
  }
  else if(tempC > (setTemp+0.75) && elapsedCool >= intervalCool) {
    startCool = millis();
    lcd.setCursor(0, 3);
    lcd.print("      Cooling ");
    digitalWrite(RELAYH1, 1);
    digitalWrite(RELAYH2, 1);
    digitalWrite(RELAYC, 0);
    while (tempC > (setTemp+0.5)) {
     GetTemp();
     //CheckInterval();
    }
  }
  else {
    digitalWrite(RELAYH1, 1);
    digitalWrite(RELAYH2, 1);
    digitalWrite(RELAYC, 1);
    lcd.setCursor(0, 3);
    lcd.print("                    ");
  }
}

//============

void recvWithStartEndMarkers() {
    static boolean recvInProgress = false;
    static byte ndx = 0;
    char startMarker = '<';
    char endMarker = '>';
    char rc;

    while (Serial.available() > 0 && newData == false) {
        rc = Serial.read();

        if (recvInProgress == true) {
            if (rc != endMarker) {
                receivedChars[ndx] = rc;
                ndx++;
                if (ndx >= numChars) {
                    ndx = numChars - 1;
                }
            }
            else {
                receivedChars[ndx] = '\0'; // terminate the string
                recvInProgress = false;
                ndx = 0;
                newData = true;
            }
        }

        else if (rc == startMarker) {
            recvInProgress = true;
        }
    }
}

//============

void parseData() {      // split the data into its parts

    //char * strtokIndx; // this is used by strtok() as an index

    //strtokIndx = strtok(tempChars,",");      // get the first part - the string
    //strcpy(messageFromPC, strtokIndx); // copy it to messageFromPC
 
    //strtokIndx = strtok(NULL, ","); // this continues where the previous call left off
    //integerFromPC = atoi(strtokIndx);     // convert this part to an integer

    //strtokIndx = strtok(NULL, ",");
    setTemp = atof(tempChars);     // convert this part to a float
    lcd.setCursor(10,0);
    lcd.print(setTemp);
    

}

//============
