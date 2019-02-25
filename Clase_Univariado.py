# -*- coding: utf-8 -*-
"""
Created on Mon Feb 25 00:54:51 2019

@author: saban
"""
import pandas as pd
import os        
os.chdir('C:\\Users\\saban\\Documents\\Falabella')

data = pd.read_csv('cs-training.csv',sep=',')
###### CREANDO MODULO DE ANALISIS UNIVARIADO

class analisis_univariado:
    def U_numerico(self,data):
        descriptivo = pd.DataFrame([])
        ConteoMissing = []
        PorcentajeMissing = []
        for col in data.columns:
            colNa = 0
            if (data[col].dtypes == 'float64') | (data[col].dtypes == 'int64'):
                descriptivo = descriptivo.append(data[col].describe())
            for val in data[col].isnull():
                if val == True:
                    colNa += 1
            ConteoMissing.append(colNa)
            PorcentajeMissing.append('{:0.2f}%'.format((float(colNa)/len(data))*100))
        descriptivo['Conteo de Missing'] = ConteoMissing
        descriptivo['Porcentaje de Missing'] = PorcentajeMissing
        return descriptivo