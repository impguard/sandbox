Raspberry Pi
============

Hardware Devices powered by a computer by Python.

GPIO connectors wired directly into the Raspberry Pi that hold a value in code.
You can hook up a lot of different things to the GPIO.

GPIO requirements
-----------------

* Component must operate at 3.3V
    - Traditionally, computers are 0 to 5V
    - Read specifications to find out (tutorials will tell you sometimes)
* Component must be digital
    - No analog voltage readings on Raspberry Pi
* Interpret signals from the component into something useful
    - Software driver hopefully exists to interpret data component is sending.

Temp Sensor
-----------

 _____
|     |
|_____|
 | | |
 | | |
 | | |

Maxim/Dallas DS18B20

* Operates at 3.3. V
    - Supports 3V to 5.5V
* Digital
    - Serial (digital) protocol (send a sequence of 1's 0's)
* Driver
    - Built into the Raspbian distribution

* Two sides, keep in mind the orientation so that the diagram matches
* Two wires are for power usually (power and ground)
* One wire for sending output

Other tools
-----------

Breadboard, wires, resistors etc.

Redis
-----

* Stores data in RAM
* Can replicate the data to another computer
* Limited and configurable writes to SD card to prevent breaking
