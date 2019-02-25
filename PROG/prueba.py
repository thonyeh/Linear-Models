# -*- coding: utf-8 -*-
"""
Created on Sat Feb 16 10:27:43 2019

@author: saban
"""

import sys
print('Python: {}'.format(sys.version))
import numpy
import pandas
#from pandas.tools.plotting import scatter_matrix
import matplotlib.pyplot as plt
import numpy as np
import os
import warnings
import savReaderWriter as spss
import pyreadstat

os.chdir('C:\\Users\\saban\\Documents\\PUCP\\2017 - 2\\ANALISIS DE DATOS CATEGORICOS\\Bases de datos')

datos, meta = pyreadstat.read_sav("SUSALUD.sav")
#datos = spss.SavReader('SUSALUD.sav',)


datos.ID = datos.ID.astype(int)

datos.head()
datos.info()
datos.shape
datos.describe()
datos.C1INDICERIQUEZA.mean()
datos.C1INDICERIQUEZA.plot(kind='box')

datos[1,:]
list(datos)
datos[datos['C1INDICERIQUEZA']>2]['C1P40']


boxplot(datos.C1INDICERIQUEZA)
print(meta)
