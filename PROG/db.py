# -*- coding: utf-8 -*-
"""
Created on Sat Feb 16 08:38:32 2019

@author: saban
"""

#import sys
import os
import pandas as pd
import numpy as np
#import warnings
#import savReaderWriter as spss

os.chdir('C:\\Users\\saban\\Documents\\PUCP\\2017 - 2\\ANALISIS DE DATOS CATEGORICOS\\Bases de datos')

credito = pd.read_csv('morosidad.csv')

print(credito.head(20))




#type(credito)

#data = spss.SavReader('SUSALUD.sav')

#data = pandas.DataFrame(list(data))


    

