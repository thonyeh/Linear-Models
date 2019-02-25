# -*- coding: utf-8 -*-
# superclase
class persona:
    usuario = ""
    edad = 0
    __contrasenia =""
    def __init__(self,user,ed):
        self.usuario = user
        self.edad = ed
    def setcontrasenia(self,passw):
        self.__contrasenia = passw
    def getcontrasenia(self):
        return self.__contrasenia
    def mostrar_datos(self):
        print("Usuario: ",self.usuario)
        print("Edad: ",self.edad)
        print("Constrasenia: ",self.getcontrasenia())