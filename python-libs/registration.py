#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
"""

import time
import hashlib
import subprocess
from os.path import exists

from PyQt5.QtCore import QTimer, QDateTime, Qt, QSize
from PyQt5.QtWidgets import (QMainWindow, QLabel, QVBoxLayout, QWidget,
                             QPushButton, QLineEdit, QMessageBox)

# The function that reads the registrationDay file and parses the date
def parse_registrationDay():
    file_name = "data/registrationDay"
    file_exists = exists(file_name)
    
    if file_exists:
        file = open(file_name, "r")
        file_content = file.read()
        
        pad = "Haskell1"
        def string2Int(my_string):
            codeInteger = []
            for code in my_string:
                codeInteger.append(ord(code))
            return codeInteger
        
        padInteger = string2Int(pad)
        codeInteger = string2Int(file_content)
        
        decodedCode = ""
        for count in range(len(padInteger)):
            decodedInt = codeInteger[count] ^ padInteger[count]
            decodedCode += chr(decodedInt)
        
        file.close()
        return decodedCode
    else:
        return "0"

# Window for displaying register text
class Register_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        global registered_global
        self.checksumed = False
        self.registered = False
        
        # If 15min pass an unregistered software gets terminated
        self.timer=QTimer()
        self.timer.timeout.connect(self.license_timer)
        time_seconds = 900
        self.timer.start(time_seconds*1000)
        
        self.setMinimumSize(QSize(350, 80))        
        self.setWindowTitle("Register window")

        self.textbox = QLineEdit()
        self.label = QLabel("Input the registration code:")
        self.button = QPushButton("Register")
        self.button.pressed.connect(self.register)
        
        file_name = "data/registrationDay"
        file_exists = exists(file_name)
        
        if file_exists:
            decoded_date = parse_registrationDay()
            
            # Checks the md5 checksum of the registrationDay file
            file = open("data/registrationDay","rb")
            file_content = file.read()
            md5sum_actual = hashlib.md5(file_content).hexdigest()
            file.close()
            
            file = open("data/registrationDayCheck","r")
            md5sum = file.read()
            file.close()
            
            if md5sum.strip("\n") == md5sum_actual:
                self.checksumed = True
        else:
            decoded_date = "0"

        # Gets the current date
        current_time = QDateTime.currentDateTime()
        current_date = current_time.toString('yyyyMMdd')
        
        # Checkes if the current date is less then the date in the registrationDay file
        self.registration_date = "0"
        if current_date <= decoded_date and self.checksumed:
            self.registration_date = decoded_date[0:4] + \
                                     "-" + decoded_date[4:6] + \
                                     "-" + decoded_date[6:]
            sentence = "Status: Registered until " + self.registration_date
            self.status_label = QLabel(sentence)
            self.registered = True
            registered_global = True
        else:
            self.status_label = QLabel("Status: Not registered")
        
        with open("data/registered_global", "w") as file:
            file.write(str(registered_global))
        
        self.textbox.setAlignment(Qt.AlignCenter)
        self.label.setAlignment(Qt.AlignCenter)
        self.status_label.setAlignment((Qt.AlignCenter))
        
        # Adds widgets to the page layout
        pagelayout = QVBoxLayout()
        pagelayout.addWidget(self.label)
        pagelayout.addWidget(self.textbox)
        pagelayout.addWidget(self.button)
        pagelayout.addWidget(self.status_label)

        container = QWidget()
        container.setLayout(pagelayout)

        # Set the central widget of the Window.
        self.setCentralWidget(container)
        
    # Stops the timer and calls the close_window funtion
    def license_timer(self):
        if not self.registered:
            self.timer.stop()
            self.close_window()
    
    # Closes all possibly opened windows
    def close_window(self):
        reply = QMessageBox.warning(
            self, "Message",
            "The unregistered application terminates after 15 min.",
            QMessageBox.Close)
        
        if reply == QMessageBox.Close:
            main_window.close()
            license_window.close()
            tutorial_window.close()
            faq_window.close()
            register_window.close()
            startup_window.close()
            if current_calc != "":
                current_calc.close()
        # Close windows in any case after the reply
        else:
            main_window.close()
            license_window.close()
            tutorial_window.close()
            faq_window.close()
            register_window.close()
            startup_window.close()
            if current_calc != "":
                current_calc.close()
    
    # Function that registers the software product
    def register(self):
        global registered_global
        
        if not self.registered:
            registration_code = self.textbox.text()
            
            # Calls the haskell code that makes a call to a REST API for registering the software product
            response = subprocess.Popen(["../haskell-libs/Main", "HTTPRequest", registration_code], cwd="./data", stdout=subprocess.PIPE)            
            output = response.communicate()[0]
            
            # If the response from the haskell code is 200 (Ok) then display a Ok message
            if "200" in output.decode("utf-8"):
                date_compact = parse_registrationDay()
                self.registration_date = date_compact[0:4] + \
                                          "-" + date_compact[4:6] + \
                                          "-" + date_compact[6:]
                sentence = "Status: Registered until " + self.registration_date
                self.status_label.setText(sentence)
                
                self.registered = True
                registered_global = True
                
                with open("data/registered_global", "w") as file:
                    file.write(str(registered_global))

                if language == "English":
                    dialog = QMessageBox.information(
                    self,
                    "Registered.",
                    "Registration was successful. Software is registered until " + self.registration_date,
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
                elif language == "German":
                    dialog = QMessageBox.information(
                    self,
                    "Registriert.",
                    "Die Registrierung war erfolgreich. Software ist registriert bis " + self.registration_date,
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
            # If the response code is 404 deisplay a fail message
            elif ("404" in output.decode("utf-8")) or ("Invalid code" in output.decode("utf-8")):
                if language == "English":
                    dialog = QMessageBox.warning(
                    self,
                    "Registration failed.",
                    "Registration was unsuccessful.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
                elif language == "German":
                    dialog = QMessageBox.warning(
                    self,
                    "Registrierung fehlgeschlagen.",
                    "Die Registrierung war nicht erfolgreich.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
            # If the response is ConnectionFailure deisplay a fail message
            elif "ConnectionFailure" in output.decode("utf-8"):
                if language == "English":
                    dialog = QMessageBox.warning(
                    self,
                    "Registration failed.",
                    "Connection issue. Try to connect to www.eurocode-calculator.com. If you are successful contact the developer for help.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
                elif language == "German":
                    dialog = QMessageBox.warning(
                    self,
                    "Registrierung fehlgeschlagen.",
                    "Verbindungsproblem. Versuchen Sie, eine Verbindung zu www.eurocode-calculator.com herzustellen. Wenn Sie erfolgreich sind, wenden Sie sich an den Entwickler, um Hilfe zu erhalten.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
        # If the software is allready registered display a notification message
        else:
            if language == "English":
                dialog = QMessageBox.information(
                self,
                "Registered.",
                "Software is allready registered until " + self.registration_date,
                buttons=QMessageBox.Ok,
                defaultButton=QMessageBox.Ok,
                )
            elif language == "German":
                dialog = QMessageBox.information(
                self,
                "Registriert.",
                "Software ist bereits registriert bis " + self.registration_date,
                buttons=QMessageBox.Ok,
                defaultButton=QMessageBox.Ok,
                )

# Global parameters
language = "English"
registered_global = False
