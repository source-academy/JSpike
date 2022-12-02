
# LEGO type:standard slot:7 autostart

from spike import PrimeHub, LightMatrix, Button, StatusLight, ForceSensor, MotionSensor, Speaker, ColorSensor, App, DistanceSensor, Motor, MotorPair
from spike.control import wait_for_seconds, wait_until, Timer
from uos import listdir
from json import load
from math import *

hub = PrimeHub()

LightMatrix.set_pixel("sfd",0,4)
hub.light_matrix.set_pixel(2, 2, 80)
hub.light_matrix.set_pixel(3, 3, 80)
hub.light_matrix.set_pixel(2, 3, 80)
hub.light_matrix.set_pixel(3, 2, 80)
hub.light_matrix.set_pixel(4, 4, 80)
hub.light_matrix.set_pixel(0, 0, 80)

print("bla")
print(listdir())
f = open("projects/myjson.json", "r")
#print(f.read())
data = load(f)
print(data)
