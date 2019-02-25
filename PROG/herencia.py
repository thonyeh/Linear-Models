# -*- coding: utf-8 -*-
"""
Created on Sat Feb 16 08:21:27 2019

@author: saban
"""

from modulopersona import *

class trabajador(persona):
    def __init__(self,user,ed,suel):
        self.usuario = user
        self.edad = ed
        self.sueldo = suel
    def mostrar_datos2(self):
        print("Sueldo: ",self.sueldo)