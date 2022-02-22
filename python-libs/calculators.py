#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
"""

import json
import subprocess
from os.path import exists
from math import sin, cos, exp, sqrt, pi, tan, log10, acos, e

from PyQt5.QtCore import Qt, QSize
from PyQt5.QtWidgets import (QMainWindow, QLabel, QHBoxLayout, QVBoxLayout,
                             QPushButton, QWidget, QGridLayout, QLineEdit, 
                             QMessageBox, QTextEdit)

# Notebook window
class Notebook_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.setWindowTitle("Notebook")
        self.setMinimumSize(QSize(560, 300))
        
        self.notebook = QTextEdit()
        self.save_button = QPushButton("Save")
        self.save_button.pressed.connect(self.save_text)
        pagelayout = QVBoxLayout()
        
        self.notebook.setMinimumWidth(500)
        self.notebook.setMinimumHeight(240)

        pagelayout.addWidget(self.notebook)
        pagelayout.addWidget(self.save_button)
        
        container = QWidget()
        container.setLayout(pagelayout)

        # Set the central widget of the Window.
        self.setCentralWidget(container)
        
    # Saves the text from the notebook window to the notebook file in a JSON object
    def save_text(self):
        file_name = "data/notebook"
        file_exists = exists(file_name)
        
        if file_exists:
            with open(file_name, "r") as file:
                json_string = file.read()
            notebook_dict = json.loads(json_string)
        else:
            notebook_dict = dict()
        
        with open(file_name, "w") as file:
            notebook_dict[calculator_name] = self.notebook.toPlainText()
            file.write(json.dumps(notebook_dict))

# Class for calculator window
class Calculator(QWidget):
    def __init__(self, parameter_count, calc_name, \
                 calc_function, result_name, result_units, \
                 x1_name = "--", x2_name = "--", \
                 x3_name = "--", x4_name = "--", \
                 x5_name = "--", x6_name = "--", \
                 x7_name = "--", x8_name = "--", \
                 x9_name = "--", x10_name = "--", \
                 x11_name = "--", x12_name = "--", \
                 non_equation = False, result_name2 = "", \
                 calc_function2 = lambda x: x, \
                 left_right_naming = False, na_text = ""):
        super().__init__()
        
        self.parameter_count = parameter_count
        self.function = calc_function
        self.function2 = calc_function2
        self.calculator_name = calc_name
        self.non_equation = non_equation
        self.left_right_naming = left_right_naming
        
        # Setting window title and size
        self.setMinimumSize(QSize(580, 230))
        
        # Setting main layouts
        pagelayout = QVBoxLayout()
        grid_layout = QGridLayout()
        na_layout = QHBoxLayout()
        button_layout = QHBoxLayout()
        result_layout = QHBoxLayout()
        
        # Defining parameter text boxes and labels for units
        self.input0_1 = QLineEdit()
        self.label0_1 = QLabel(x1_name)
        self.input0_2 = QLineEdit()
        self.label0_2 = QLabel(x2_name)
        self.input0_3 = QLineEdit()
        self.label0_3 = QLabel(x3_name)
        self.input1_1 = QLineEdit()
        self.label1_1 = QLabel(x4_name)
        self.input1_2 = QLineEdit()
        self.label1_2 = QLabel(x5_name)
        self.input1_3 = QLineEdit()
        self.label1_3 = QLabel(x6_name)
        self.input2_1 = QLineEdit()
        self.label2_1 = QLabel(x7_name)
        self.input2_2 = QLineEdit()
        self.label2_2 = QLabel(x8_name)
        self.input2_3 = QLineEdit()
        self.label2_3 = QLabel(x9_name)
        self.input3_1 = QLineEdit()
        self.label3_1 = QLabel(x10_name)
        self.input3_2 = QLineEdit()
        self.label3_2 = QLabel(x11_name)
        self.input3_3 = QLineEdit()
        self.label3_3 = QLabel(x12_name)
        
        self.input0_1.setMaximumWidth(70)
        self.input0_2.setMaximumWidth(70)
        self.input0_3.setMaximumWidth(70)
        self.input1_1.setMaximumWidth(70)
        self.input1_2.setMaximumWidth(70)
        self.input1_3.setMaximumWidth(70)
        self.input2_1.setMaximumWidth(70)
        self.input2_2.setMaximumWidth(70)
        self.input2_3.setMaximumWidth(70)
        self.input3_1.setMaximumWidth(70)
        self.input3_2.setMaximumWidth(70)
        self.input3_3.setMaximumWidth(70)
        
        # Writing to text boxes (reading from JSON file in Haskell code)
        response = subprocess.Popen(["../haskell-libs/Main", "ParseJSON", "readJSON", self.calculator_name], cwd="./data", stdout=subprocess.PIPE)
        output = response.communicate()[0]
        
        parameters = output.decode("utf-8")
        parameters = parameters.strip("[\"]\n")
        parameters = parameters.split("|")
        
        self.input0_1.setText(parameters[0])
        self.input0_2.setText(parameters[1])
        self.input0_3.setText(parameters[2])
        self.input1_1.setText(parameters[3])
        self.input1_2.setText(parameters[4])
        self.input1_3.setText(parameters[5])
        self.input2_1.setText(parameters[6])
        self.input2_2.setText(parameters[7])
        self.input2_3.setText(parameters[8])
        self.input3_1.setText(parameters[9])
        self.input3_2.setText(parameters[10])
        self.input3_3.setText(parameters[11])
                            
        # Setting grid layout
        grid_layout.addWidget(self.input0_1, 0, 0)
        grid_layout.addWidget(self.label0_1, 0, 1)
        grid_layout.addWidget(self.input0_2, 0, 2)
        grid_layout.addWidget(self.label0_2, 0, 3)
        grid_layout.addWidget(self.input0_3, 0, 4)
        grid_layout.addWidget(self.label0_3, 0, 5)
        
        grid_layout.addWidget(self.input1_1, 1, 0)
        grid_layout.addWidget(self.label1_1, 1, 1)
        grid_layout.addWidget(self.input1_2, 1, 2)
        grid_layout.addWidget(self.label1_2, 1, 3)
        grid_layout.addWidget(self.input1_3, 1, 4)
        grid_layout.addWidget(self.label1_3, 1, 5)
        
        grid_layout.addWidget(self.input2_1, 2, 0)
        grid_layout.addWidget(self.label2_1, 2, 1)
        grid_layout.addWidget(self.input2_2, 2, 2)
        grid_layout.addWidget(self.label2_2, 2, 3)
        grid_layout.addWidget(self.input2_3, 2, 4)
        grid_layout.addWidget(self.label2_3, 2, 5)
        
        grid_layout.addWidget(self.input3_1, 3, 0)
        grid_layout.addWidget(self.label3_1, 3, 1)
        grid_layout.addWidget(self.input3_2, 3, 2)
        grid_layout.addWidget(self.label3_2, 3, 3)
        grid_layout.addWidget(self.input3_3, 3, 4)
        grid_layout.addWidget(self.label3_3, 3, 5)
        
        # Defining National Annex label
        if na_text == "":
            na_content = "National Annex: --"
        else:
            na_content = "National Annex:\n" + na_text
            
        self.na_label = QLabel(na_content)
        self.na_label.setFixedHeight(60)
        na_layout.addWidget(self.na_label)
        
        # Defining calculate button
        self.calculate_btn = QPushButton("Calculate")
        self.calculate_btn.pressed.connect(self.calculate_eq)
        
        self.notebook_btn = QPushButton("Open notebook")
        self.notebook_btn.pressed.connect(self.open_notebook)
        
        button_layout.addWidget(self.calculate_btn)
        button_layout.addWidget(self.notebook_btn)
        
        # Setting result layout
        self.result_parameter_name1 = QLabel(result_name)
        self.result_parameter_name1.setAlignment(Qt.AlignRight)
        
        self.result_parameter_name2 = QLabel(result_name2)
        self.result_parameter_name2.setAlignment(Qt.AlignRight)
        
        self.result_label1 = QLabel("0")
        self.result_label1.setAlignment(Qt.AlignRight)
        
        units_label1 = QLabel(result_units)
        
        if non_equation:
            self.result_label2 = QLabel("0")
            units_label2 = QLabel(result_units)
        else:
            self.result_label2 = QLabel("")
            units_label2 = QLabel("")
        self.result_label2.setAlignment(Qt.AlignRight)
        
        # Adding widgets to the result layout
        result_layout.addWidget(self.result_parameter_name2)
        result_layout.addWidget(self.result_label2)
        result_layout.addWidget(units_label2)
        result_layout.addWidget(self.result_parameter_name1)
        result_layout.addWidget(self.result_label1)
        result_layout.addWidget(units_label1)
        
        # Adding layouts to page layout
        pagelayout.addLayout(grid_layout)
        pagelayout.addLayout(na_layout)
        pagelayout.addLayout(button_layout)
        pagelayout.addLayout(result_layout)
        
        # Adding layouts to container
        self.setLayout(pagelayout)
        
    # Function for opening the notebook window
    def open_notebook(self):
        file_name = "data/notebook"
        file_exists = exists(file_name)
        
        if file_exists:
            with open(file_name, "r") as file:
                json_string = file.read()
            notebook_dict = json.loads(json_string)
            if calculator_name in notebook_dict.keys():
                text = notebook_dict[calculator_name]
                notebook_window.notebook.setPlainText(text)
            else:
                notebook_window.notebook.setPlainText("")
        else:
            notebook_window.notebook.setPlainText("")
            
        notebook_window.show()
        
    # Function for calculating the equation
    def calculate_eq(self):
        inputs = [self.input0_1, self.input0_2, self.input0_3, \
                  self.input1_1, self.input1_2, self.input1_3, \
                  self.input2_1, self.input2_2, self.input2_3, \
                  self.input3_1, self.input3_2, self.input3_3]

        error1 = False
        error2 = False
        xs = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

        # Lopping only through the number of parameters that are not 0
        for count in range(self.parameter_count):
            try:
                xs[count] = float(inputs[count].text())
            except:
                error1 = True
                # Displaying error message if invalid text is entered from the user
                if language == "English":
                    dialog = QMessageBox.critical(
                    self,
                    "Wrong input.",
                    "Invalid number input in text box " + str(count+1) + ". Use only numbers and use the dot '.' seperator for decimal numbers.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
                elif language == "German":
                    dialog = QMessageBox.critical(
                    self,
                    "Falsche Eingabe.",
                    "Ungültige Zahleneingabe in text Box " + str(count+1) + ". Verwenden sie nur Zahlen und verwenden Sie einen Punkt '.' als Trennzeichen für Dezimalzahlen.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )

        if not error1:
            try:
                result_orig = self.function(xs[0], xs[1], xs[2], xs[3], xs[4], xs[5],\
                                            xs[6], xs[7], xs[8], xs[9], xs[10], xs[11])
            except:
                error2 = True
                # Displaying error message if the result does not make mathematical sense
                if language == "English":
                    dialog = QMessageBox.critical(
                    self,
                    "Wrong input.",
                    "Dividing by zero or negative square root. Check parameter input.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    )
                elif language == "German":
                    dialog = QMessageBox.critical(
                    self,
                    "Falsche Eingabe.",
                    "Dividieren durch Null oder negative Quadratwurzel. Überprüfen Sie die Parametereingabe.",
                    buttons=QMessageBox.Ok,
                    defaultButton=QMessageBox.Ok,
                    ) 

            if not error2:
                try:
                    round_index_final = 1
                    result = round(result_orig, round_index_final)
                    if result == 0:
                        for round_index in range(2,10):
                            round_index_final = round_index
                            result = round(result_orig, round_index)
                            if result != 0:
                                break
                    
                    round_index_final += 3
                    result = round(result_orig, round_index_final)
                    self.result_label1.setText(str(result))
                    
                    if self.non_equation == True:
                        result_orig2 = self.function2(xs[0], xs[1], xs[2], xs[3], xs[4], xs[5],\
                                                      xs[6], xs[7], xs[8], xs[9], xs[10], xs[11])
                        round_index_final = 1
                        result2 = round(result_orig2, round_index_final)
                        if result2 == 0:
                            for round_index in range(2,10):
                                round_index_final = round_index
                                result2 = round(result_orig2, round_index)
                                if result2 != 0:
                                    break
                        
                        round_index_final += 3
                        result2 = round(result_orig2, round_index_final)
                        self.result_label2.setText(str(result2))
                    
                    parameters_string = str(xs[0]) + "|" + str(xs[1]) + "|" + \
                                        str(xs[2]) + "|" + str(xs[3]) + "|" + \
                                        str(xs[4]) + "|" + str(xs[5]) + "|" + \
                                        str(xs[6]) + "|" + str(xs[7]) + "|" + \
                                        str(xs[8]) + "|" + str(xs[9]) + "|" + \
                                        str(xs[10]) + "|" + str(xs[11])
                    
                    # Saving input parameters to the JSON saved_parameters file
                    response = subprocess.Popen(["../haskell-libs/Main", "ParseJSON", "writeJSON", \
                                                 self.calculator_name, parameters_string], \
                                                 cwd="./data", stdout=subprocess.PIPE)
                except:
                    # Displaying error message if a negative square root is present
                    if language == "English":
                        dialog = QMessageBox.critical(
                        self,
                        "Wrong input.",
                        "Negative square root. Check parameter input.",
                        buttons=QMessageBox.Ok,
                        defaultButton=QMessageBox.Ok,
                        )
                    elif language == "German":
                        dialog = QMessageBox.critical(
                        self,
                        "Falsche Eingabe.",
                        "Negative Quadratwurzel. Überprüfen Sie die Parametereingabe.",
                        buttons=QMessageBox.Ok,
                        defaultButton=QMessageBox.Ok,
                        ) 

# Calculator window classes
class EN_1992_1_1__3_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    # Populates text-boxes with updated values
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 * x2
        result_name = "f_cm(t) = "
        result_units = "kN/cm2"
        x1_name = "β_cc(t)"
        x2_name = "f_cm [kN/cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        exp(x1*(1 - (28/x2)**0.5))
        result_name = "β_cc(t) = "
        result_units = ""
        x1_name = "s"
        x2_name = "t [days]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__3_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "f_cd = "
        result_units = "kN/cm2"
        x1_name = "α_cc"
        x2_name = "f_ck [kN/cm2]"
        x3_name = "γ_c"
        na_text = "α_cc = 0.85 (Germany)\n"
        na_text += "α_cc = 1 (<=C50/60), α_cc = 0.9 (>C50/60) (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__3_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "f_ctd = "
        result_units = "kN/cm2"
        x1_name = "α_ct"
        x2_name = "f_ctk,0.05 [kN/cm2]"
        x3_name = "γ_c"
        na_text = "α_ct = 0.85 or 1 (Germany)\n"
        na_text += "α_ct = 1 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_3__7_122(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/2.9*x3/(10*(x3 - x4))
        result_name = "Φ_s = "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "f_ct,eff"
        x3_name = "h"
        x4_name = "d"
        na_text = "Φ*_s = w_k*3.48*10^6/σ_s^2 (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_3__K_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2**(2/3)
        result_name = "f_ctx = "
        result_units = ""
        x1_name = "α"
        x2_name = "f_ckT"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

