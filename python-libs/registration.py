#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 15 2023

@author: Luka Kurnjek
@license: MIT License
"""

import time
import hashlib
from os.path import exists

from PyQt5.QtCore import QTimer, QDateTime, Qt, QSize
from PyQt5.QtWidgets import (QMainWindow, QLabel, QVBoxLayout, QWidget,
                             QPushButton, QLineEdit, QMessageBox)

# Window for displaying register text
class Register_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        global registered_global
        self.checksumed = False
        self.registered = False
        
        self.timer=QTimer()
        self.timer.timeout.connect(self.license_timer)
        time_seconds = 900 #TODO: change to 15min = 900sec
        self.timer.start(time_seconds*1000)
        
        self.setMinimumSize(QSize(350, 80))        
        self.setWindowTitle("Register window")

        self.textbox = QLineEdit()
        self.label = QLabel("Input the registration code:")
        self.button = QPushButton("Register")
        self.button.pressed.connect(self.register)
        
        decoded_date = "0"
        file_name = "data/data1"
        file_exists = exists(file_name)
        
        codec = {"a": "0", "s": "1", "d": "2", "f": "3", "g": "4", \
                 "h": "5", "j": "6", "k": "7", "l": "8", "e": "9"}
        def apply_decoder(code):
            encoded_date = ""
            
            for count in range(len(code)):
                letter = code[count]
                number = codec[letter]
                encoded_date += number
        
            return encoded_date
        
        if file_exists:
            file = open("data/data1","r")
            file_content = file.read()
            decoded_date = apply_decoder(file_content)
            file.close()
            
            file = open("data/data1","rb")
            file_content = file.read()
            md5sum_actual = hashlib.md5(file_content).hexdigest()
            file.close()
            
            file = open("data/data2","r")
            md5sum = file.read()
            file.close()
            
            if md5sum.strip("\n") == md5sum_actual:
                self.checksumed = True
        
        current_time = QDateTime.currentDateTime()
        current_date = current_time.toString('yyyyMMdd')
        
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
        
        self.textbox.setAlignment(Qt.AlignCenter)
        self.label.setAlignment(Qt.AlignCenter)
        self.status_label.setAlignment((Qt.AlignCenter))
        
        pagelayout = QVBoxLayout()
        pagelayout.addWidget(self.label)
        pagelayout.addWidget(self.textbox)
        pagelayout.addWidget(self.button)
        pagelayout.addWidget(self.status_label)

        container = QWidget()
        container.setLayout(pagelayout)

        # Set the central widget of the Window.
        self.setCentralWidget(container)
        
        with open("data/hide_startup", "r") as file:
            hide_startup_txt = file.read()
        
        if not registered_global and "False" in hide_startup_txt:
            startup_window.show()
        
    def license_timer(self):
        if not self.registered:
            self.timer.stop()
            self.close_window()
            #self.closeEvent()
            
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
        else:
            main_window.close()
            license_window.close()
            tutorial_window.close()
            faq_window.close()
            register_window.close()
            startup_window.close()
            if current_calc != "":
                current_calc.close()
    
    def register(self):
        global registered_global
        
        if not self.registered:
            registration_code = self.textbox.text()
            
            try:
                #TODO: change IP to eurocode-calculator.com
                #api_url = "http://193.77.151.238:3000/keys/" + registration_code
                api_url = "http://www.eurocode-calculator.com:3000/keys/" + registration_code
                response = requests.get(api_url)
                
                codec = {"0": "a", "1": "s", "2": "d", "3": "f", "4": "g", \
                         "5": "h", "6": "j", "7": "k", "8": "l", "9": "e"}
                def apply_encoder(code):
                    encoded_code = ""
                    
                    for count in range(len(code)):
                        number = code[count]
                        letter = codec[number]
                        encoded_code += letter
                
                    return encoded_code
    
                if response.status_code == 200 and \
                registration_code in response.json():
                    current_time = QDateTime.currentDateTime()
                    current_date = current_time.toString('yyyyMMdd')
                    
                    current_year = int(current_date[0:4])
                    new_year = current_year + 1
                    new_date = str(new_year) + current_date[4:]
                    new_date_encoded = apply_encoder(new_date)
                    
                    file = open("data/data1","w")
                    file.write(new_date_encoded)
                    file.close()
                    
                    file = open("data/data1","rb")
                    file_content = file.read()
                    md5sum = hashlib.md5(file_content).hexdigest()
                    file.close()
                    
                    file = open("data/data2","w")
                    file.write(md5sum)
                    file.close()
                    
                    self.registration_date = new_date[0:4] + \
                                             "-" + new_date[4:6] + \
                                             "-" + new_date[6:]
                    sentence = "Status: Registered until " + self.registration_date
                    self.status_label.setText(sentence)
                    
                    self.registered = True
                    registered_global = True
                    response = requests.delete(api_url)
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
                    
                else:
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
            except:
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
