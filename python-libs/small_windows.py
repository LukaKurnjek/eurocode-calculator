#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
"""

from PyQt5.QtCore import Qt, QSize
from PyQt5.QtWidgets import (QMainWindow, QLabel, QVBoxLayout, QWidget,
                             QScrollArea)

# Class for scrollable label
class ScrollLabel(QScrollArea):
    # Constructor
    def __init__(self, *args, **kwargs):
        QScrollArea.__init__(self, *args, **kwargs)
 
        # making widget resizable
        self.setWidgetResizable(True)
        
        # making qwidget object
        content = QWidget(self)
        self.setWidget(content)
 
        # vertical box layout
        lay = QVBoxLayout(content)
 
        # creating label
        self.label = QLabel(content)
 
        # setting alignment to the text
        self.label.setAlignment(Qt.AlignLeft | Qt.AlignTop)
 
        # making label multi-line
        self.label.setWordWrap(True)
 
        # adding label to the layout
        lay.addWidget(self.label)
 
    # Sets the text to the label
    def setText(self, text):
        self.label.setText(text)

# Window for displaying license text
class License_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.setWindowTitle("License")
        self.setMinimumSize(QSize(600, 600))
        
        with open("LICENSE", "r") as file:
            license_text = file.read()
        license_label = ScrollLabel(self)
        license_label.setText(license_text)
        
        self.setCentralWidget(license_label)

# Window for displaying FAQ text
class Faq_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.setMinimumSize(QSize(600, 600))
        self.windowTitleChanged.connect(self.display_faq)
        
        self.setWindowTitle("Frequently asked questions")
        
    def display_faq(self):
        if language == "English":
            with open("language/FAQ_english.txt", "r") as file:
                faq_text = file.read()
        elif language == "German":
            with open("language/FAQ_german.txt", "r") as file:
                faq_text = file.read()
            
        faq_label = ScrollLabel(self)
        faq_label.setText(faq_text)
        
        self.setCentralWidget(faq_label)

# Window for displaying tutorial text
class Tutorial_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.setMinimumSize(QSize(600, 600))
        self.windowTitleChanged.connect(self.display_tutorial)
        
        self.setWindowTitle("Tutorial")
        
    def display_tutorial(self):
        if language == "English":
            with open("language/tutorial_english.txt", "r") as file:
                tutorial_text = file.read()
        elif language == "German":
            with open("language/tutorial_german.txt", "r") as file:
                tutorial_text = file.read()
            
        tutorial_label = ScrollLabel(self)
        tutorial_label.setText(tutorial_text)
        
        self.setCentralWidget(tutorial_label)
        
# Global parameters
language = "English"