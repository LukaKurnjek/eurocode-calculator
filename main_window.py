#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 15 2023

@author: Luka Kurnjek
@license: MIT License
"""

import sys
sys.path.insert(1, './python-libs')

from calculators import *
from small_windows import *

import json
import time
import hashlib
import requests
from os.path import exists
from math import sin, cos, exp, sqrt, pi, tan, log10, acos, e

import xml.etree.ElementTree as ET
from xml.etree.ElementTree import ElementTree
from xml.etree.ElementTree import Element

from PyQt5.QtCore import QTimer, QDateTime, Qt, QSize
from PyQt5.QtGui import QPixmap, QFont, QColor, QIcon
from PyQt5.Qt import QStandardItem, QStandardItemModel
from PyQt5.QtWidgets import (QApplication, QMainWindow, QAction,
                             QLabel, QHBoxLayout, QVBoxLayout, QWidget,
                             QPushButton, QTreeWidget, QTreeWidgetItem,
                             QStackedLayout, QTabWidget, QGridLayout,
                             QLineEdit, QMessageBox, QScrollArea,
                             QTextEdit, QCheckBox)

# Class for closing a QTabWidget
class QCustomTabWidget (QTabWidget):
    def __init__ (self, parent = None):
        super(QCustomTabWidget, self).__init__(parent)
        self.setTabsClosable(True)
        # connect to method to close
        self.tabCloseRequested.connect(self.closeTab)

    # Function for closing the tab
    def closeTab (self, currentIndex):
        global current_tab, opened_tabs
        
        # One QTreeWidget should always stay opened
        if len(opened_tabs) > 1:
            opened_tabs.pop(currentIndex)
            current_tab = max(opened_tabs)
        
            currentQWidget = self.widget(currentIndex)
            currentQWidget.deleteLater()
            self.removeTab(currentIndex)

# Main window class
class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        # Settings for main window
        self.setWindowTitle("Eurocode calculator")
        self.setMinimumSize(QSize(1350, 600))        
        self.setStyleSheet("background-color: lightgray;")

        # Create the applications menu
        self.app_menu = self.menuBar()
        license_menu = self.app_menu.addMenu("&License")
        language_menu = self.app_menu.addMenu("&Language / Sprache")
        help_menu = self.app_menu.addMenu("&Help")
        
        # Adding file menu buttons
        license_action = QAction("&License", self)
        license_action.triggered.connect(self.open_license)
        
        license_menu.addAction(license_action)
        
        # Adding language menu buttons
        english_action = QAction(QIcon("language/en.png"), "&English", self)
        english_action.triggered.connect(self.set_english)
        
        german_action = QAction(QIcon("language/de.png"), "&Deutsch", self)
        german_action.triggered.connect(self.set_german)
        
        language_menu.addAction(english_action)
        language_menu.addSeparator()
        language_menu.addAction(german_action)
        
        # Adding help menu buttons
        faq_action = QAction("&FAQ", self)
        faq_action.triggered.connect(self.open_faq)
        
        tutorial_action = QAction("&Tutorial", self)
        tutorial_action.triggered.connect(self.open_tutorial)

        help_menu.addAction(faq_action)
        help_menu.addSeparator()
        help_menu.addAction(tutorial_action)

        # Creating the left side menu QTreeWidget
        self.menu = QTreeWidget()
        self.menu.setHeaderLabels(("EC equations",))
        
        # Populating the left side menu widget
        with open("xml_trees/menu_tree.xml", "r") as file:
            xmlFileString = file.read()
        self.populate_tree_widget(self.menu, xmlFileString)
        
        # Setting size and connecting to itemClick signal of menu widget
        self.menu.setMinimumWidth(190)
        self.menu.setMaximumWidth(190)
        self.menu.itemClicked.connect(self.menu_clicked_item)
        
        # Creating the equation widget that displays the .png equations
        self.equation = ScrollPicture(self)
        self.equation.setPicture("language/start_text_English.png")

        # Creating 20 bookmark widgets
        self.bookmark1 = QTreeWidget()
        self.bookmark2 = QTreeWidget()
        self.bookmark3 = QTreeWidget()
        self.bookmark4 = QTreeWidget()
        self.bookmark5 = QTreeWidget()
        self.bookmark6 = QTreeWidget()
        self.bookmark7 = QTreeWidget()
        self.bookmark8 = QTreeWidget()
        self.bookmark9 = QTreeWidget()
        self.bookmark10 = QTreeWidget()
        self.bookmark11 = QTreeWidget()
        self.bookmark12 = QTreeWidget()
        self.bookmark13 = QTreeWidget()
        self.bookmark14 = QTreeWidget()
        self.bookmark15 = QTreeWidget()
        self.bookmark16 = QTreeWidget()
        self.bookmark17 = QTreeWidget()
        self.bookmark18 = QTreeWidget()
        self.bookmark19 = QTreeWidget()
        self.bookmark20 = QTreeWidget()
        
        self.bookmark_widgets = {1: self.bookmark1,
                                 2: self.bookmark2,
                                 3: self.bookmark3,
                                 4: self.bookmark4,
                                 5: self.bookmark5,
                                 6: self.bookmark6,
                                 7: self.bookmark7,
                                 8: self.bookmark8,
                                 9: self.bookmark9,
                                 10: self.bookmark10,
                                 11: self.bookmark11,
                                 12: self.bookmark12,
                                 13: self.bookmark13,
                                 14: self.bookmark14,
                                 15: self.bookmark15,
                                 16: self.bookmark16,
                                 17: self.bookmark17,
                                 18: self.bookmark18,
                                 19: self.bookmark19,
                                 20: self.bookmark20}
        
        # By default the 1st tab is always opened at startup
        file_name = "xml_trees/bookmark_tree1.xml"
        file_exists = exists(file_name)
        
        if file_exists:
            fp = open(file_name, "r")
            file_content = fp.read()
            fp.close()
        
        if not file_exists or file_content == "":
            fp = open(file_name, 'w')
            fp.write("<EN_1992>\n</EN_1992>")
            fp.close()
        
        # Creating button widgets
        self.bookmark_btn = QPushButton("Bookmark equation")
        self.bookmark_btn.pressed.connect(self.bookmark_eq)  
        
        self.open_calculator_btn = QPushButton("Open calculator")
        self.open_calculator_btn.pressed.connect(self.show_calculator)         
        
        self.delete_btn = QPushButton("Delete equation")
        self.delete_btn.pressed.connect(self.delete_eq)
        
        self.open_tab_btn = QPushButton("Create tab")
        self.open_tab_btn.pressed.connect(self.create_tab)
        
        # Creating layouts
        pagelayout = QHBoxLayout()
        menu_layout = QVBoxLayout()
        equation_layout = QVBoxLayout()
        bookmark_layout = QVBoxLayout()
        button_layout1 = QHBoxLayout()
        button_layout2 = QHBoxLayout()
        
        # Adding layouts to page layout
        layout_list = [menu_layout, equation_layout, bookmark_layout]
        for layout in layout_list:
            pagelayout.addLayout(layout)
        
        # Populating three main layouts
        menu_layout.addWidget(self.menu)
        #menu_layout.addWidget(self.bookmark_btn)
        
        equation_layout.addWidget(self.equation)
        equation_layout.addLayout(button_layout1)
        equation_layout.addLayout(button_layout2)
        
        button_layout1.addWidget(self.open_calculator_btn)
        button_layout1.addWidget(self.open_tab_btn)
        button_layout2.addWidget(self.bookmark_btn)
        button_layout2.addWidget(self.delete_btn)

        # Creating tabs widget
        self.tabs = QCustomTabWidget()        
        self.tabs.setTabPosition(QTabWidget.East)
        self.tabs.setMovable(True)
        
        # Adding tab widget to bookmark layout
        bookmark_layout.addWidget(self.tabs)
        #bookmark_layout.addWidget(self.delete_btn)
        
        # By default the bookmark tab 1 gets always displayed
        self.display_bookmark_window_at_startup(1)
        
        # Checks if populated bookmark XML files exist and if yes displays them
        for bookmark_key in range(2,21):
            file_name = "xml_trees/bookmark_tree" + str(bookmark_key) + ".xml"
            file_exists = exists(file_name)
            
            if file_exists:
                fp = open(file_name, "r")
                file_content = fp.read()
                fp.close()
            
            if file_exists and file_content != "" and \
            file_content != '<EN_1992>\n</EN_1992>': #TODO: change for other codes
                self.display_bookmark_window_at_startup(bookmark_key)

        # Has to come after for loop so the opened_tabs is allready updated
        self.tabs.tabBarClicked.connect(self.update_current_tab)
        self.tabs.currentChanged.connect(self.update_current_tab)

        # Adding layouts to container
        container = QWidget()
        container.setLayout(pagelayout)

        # Set the central widget of the Window.
        self.setCentralWidget(container)
            
    # Stops the timer when the main window closes.
    def closeEvent(self, s):
        pass

    # Defining tab functions        
    def open_faq(self):
        faq_window.show()

    def open_tutorial(self):
        tutorial_window.show()
        
    def open_license(self):
        license_window.show()
        
    # Sets the text of the menu, main window and calculator buttons to english
    def set_english(self):
        global language
        language = "English"
        
        faq_window.setWindowTitle("Frequently asked questions")
        tutorial_window.setWindowTitle("Tutorial")
        
        self.bookmark_btn.setText("Bookmark equation")
        self.open_calculator_btn.setText("Open calculator")
        self.delete_btn.setText("Delete equation")
        self.open_tab_btn.setText("Create tab")
        
        self.setWindowTitle("Eurocode calculator")
        self.menu.setHeaderLabels(("EC equations",))
        self.bookmark_widgets[selected_tab].setHeaderLabels(("Bookmarked equations",))

        if tree_widget_selection == "menu":
            current_selection = self.menu.currentItem().text(0)
            if "EN_1992" == current_selection:
                self.equation.setPicture("language/start_text_English.png")
            else:
                self.menu_clicked_item()
        elif tree_widget_selection == "bookmark":
            current_selection = self.bookmark_widgets[selected_tab].currentItem().text(0)
            if "EN_1992" == current_selection:
                self.equation.setPicture("language/start_text_English.png")
            else:
                self.bookmark_clicked_item()
        
    # Displays the german equation
    def set_german(self):
        global language
        language = "German"
        
        faq_window.setWindowTitle("Häufig gestellte Fragen")
        tutorial_window.setWindowTitle("Tutorium")
        
        self.bookmark_btn.setText("Gleichung verzeichnen")
        self.open_calculator_btn.setText("Rechner öffnen")
        self.delete_btn.setText("Gleichung löschen")
        self.open_tab_btn.setText("Tab erstellen")
        
        self.setWindowTitle("Eurocode Rechner")
        self.menu.setHeaderLabels(("EC Gleichungen",))
        self.bookmark_widgets[selected_tab].setHeaderLabels(("Lesezeichen Gleichungen",))
        
        if tree_widget_selection == "menu":
            current_selection = self.menu.currentItem().text(0)
            if "EN_1992" == current_selection:
                self.equation.setPicture("language/start_text_German.png")
            else:
                self.menu_clicked_item()
        elif tree_widget_selection == "bookmark":
            current_selection = self.bookmark_widgets[selected_tab].currentItem().text(0)
            if "EN_1992" == current_selection:
                self.equation.setPicture("language/start_text_German.png")
            else:
                self.bookmark_clicked_item()
    
    # Creates a bookmark tab and adds a QTreeWidget to the stacked widget
    def display_bookmark_window_at_startup(self, bookmark_key):
        global current_tab, opened_tabs
        
        #self.bookmark_widgets[bookmark_key] = QTreeWidget()
        self.bookmark_widgets[bookmark_key].setHeaderLabels(("Bookmarked equations",))
        self.bookmark_widgets[bookmark_key].setMinimumWidth(190)
        self.bookmark_widgets[bookmark_key].setMaximumWidth(190) #TEST
        self.tabs.addTab(self.bookmark_widgets[bookmark_key], "Tab " + str(bookmark_key))
        self.bookmark_widgets[bookmark_key].itemClicked.connect(self.bookmark_clicked_item)
        
        current_tab = bookmark_key
        opened_tabs.append(bookmark_key)
        
        file_name = "xml_trees/bookmark_tree" + str(bookmark_key) + ".xml"
        with open(file_name, "r") as file:
            xmlFileString = file.read()
        self.populate_tree_widget(self.bookmark_widgets[bookmark_key], xmlFileString)
        
    # Prints the chapter tree structure from XML string to the given widget
    def populate_tree_widget(self, widget, xmlFileString):
        tree = ET.fromstring(xmlFileString)
        widget.setColumnCount(1)
        tree_item = QTreeWidgetItem([tree.tag])
        widget.addTopLevelItem(tree_item)
        
        def display_tree(tree_item,tree_string):
            for child in tree_string:
                branch = QTreeWidgetItem([child.tag])
                tree_item.addChild(branch)
                display_tree(branch, child)
        
        display_tree(tree_item, tree)
        
    # Displaying the EC equation when clicking on a equation in menu window
    def menu_clicked_item(self):
        global tree_widget_selection
        tree_widget_selection = "menu"
        
        selected_tree_widget_item = self.menu.currentItem()
        self.display_equation(selected_tree_widget_item)

    # Displaying the EC equation when clicking on a equation in bookmark window
    def bookmark_clicked_item(self):
        global tree_widget_selection
        tree_widget_selection = "bookmark"
        
        selected_tree_widget_item = self.bookmark_widgets[selected_tab].currentItem()
        self.display_equation(selected_tree_widget_item)

    # Displays the selected equation
    def display_equation(self, selected_tree_widget_item):
        current_text = selected_tree_widget_item.text(0)
        
        global menu_window_text
        menu_window_text = current_text
        
        # Condition for sub-chapter selection
        if "Eq" in current_text:
            tree_path_elements = self.get_parent_path(selected_tree_widget_item)
            tree_path_elements = tree_path_elements.split("/")
            
            EC_title = tree_path_elements[1]
            
            modified_window_text = current_text.replace('Eq_','')
            modified_window_text = modified_window_text.split("_")[0]
            modified_window_text = modified_window_text.replace('.','_')
            
            #TODO: change "English" to language
            #TODO: handle slashes for windows
            self.equation.setPicture("equations/" \
                                     + "English" + "/" \
                                     + EC_title + "/" \
                                     + modified_window_text + ".png")

    # Returns the path (as string) of a selected item in the tree window
    def get_parent_path(self, selected_item):
        def get_parent(selected_item, outstring):
            if selected_item.parent() is None:
                return outstring
            outstring = selected_item.parent().text(0) + "/" + outstring
            return get_parent(selected_item.parent(), outstring)
        
        output = get_parent(selected_item, selected_item.text(0))
        return output

    # Pins the equation from the menu to the bookmark window
    def bookmark_eq(self):
        selected_menu_item = self.menu.currentItem()
        tree_path_elements = self.get_parent_path(selected_menu_item)
        tree_path_elements = tree_path_elements.split("/")
        
        if "Eq" in tree_path_elements[-1]:
            self.parse_bookmark_xml(tree_path_elements[1], tree_path_elements[2], 
                                    tree_path_elements[3], selected_tab)
        
        # Avoids duplicated entry into bookmark widget
        root = self.bookmark_widgets[selected_tab].invisibleRootItem()
        root.removeChild(self.bookmark_widgets[selected_tab].topLevelItem(0))
        
        # Populates the bookmark tree widget with data from file
        file_name = "xml_trees/bookmark_tree" + str(selected_tab) + ".xml"
        with open(file_name, "r") as file:
            xmlFileString = file.read()
        self.populate_tree_widget(self.bookmark_widgets[selected_tab], xmlFileString) 
        
    # Parses the bookmark.xml file and inserts the selected equation
    def parse_bookmark_xml(self, EN, chapter, equation, provided_tab):
        EN_element = Element(EN)
        chapter_element = Element(chapter)
        equation_element = Element(equation)
        
        file_name = "xml_trees/bookmark_tree" + str(provided_tab) + ".xml"
        tree = ET.parse(file_name)
        root = tree.getroot()
        
        EN_tree_element = root.find(EN)
        if EN_tree_element == None:
            insert_index = 0
            for single_EN in root:
                if single_EN.tag > EN:
                    break
                insert_index += 1
            root.insert(insert_index,EN_element)
            EN_element.append(chapter_element)
            chapter_element.append(equation_element)
        else:
            chapter_tree_element = EN_tree_element.find(chapter)
            if chapter_tree_element == None:
                insert_index = 0
                for single_chapter in EN_tree_element:
                    if single_chapter.tag > chapter:
                        break
                    insert_index += 1
                EN_tree_element.insert(insert_index, chapter_element)
                chapter_element.append(equation_element)
            else:
                equation_tree_element = chapter_tree_element.find(equation)
                if equation_tree_element == None:
                    insert_index = 0
                    for single_equation in chapter_tree_element:
                        if single_equation.tag > equation:
                            break
                        insert_index += 1
                    chapter_tree_element.insert(insert_index, equation_element)
                else:
                    equation_name = equation_tree_element.tag
                    new_equation_name = ""
                    equation_before = ""
                    for single_equation in chapter_tree_element:
                        single_equation_txt = single_equation.tag
                        if equation_name in single_equation_txt:
                            if equation_before != "":
                                equation_before_strip = equation_before.replace("Eq_", "")
                                if "_" not in equation_before_strip:
                                    if "_1" in single_equation_txt:
                                        new_equation_name = self.strip_number_before_underscore(single_equation_txt) + "2"
                                    else:
                                        new_equation_name = equation_before + "_1"
                                        break
                                else:
                                    number_before = self.get_number_before_underscore(equation_before)
                                    number_current = self.get_number_before_underscore(single_equation_txt)
                                    if int(number_before) + 1 == int(number_current):                    
                                        increment = self.get_number_before_underscore(single_equation_txt)
                                        increment = int(increment) + 1
                                        new_equation_name = self.strip_number_before_underscore(single_equation_txt) + str(increment)
                                    else:
                                        increment = int(number_before) + 1
                                        new_equation_name = self.strip_number_before_underscore(equation_before)
                                        new_equation_name += str(increment)
                                        break
                            else:
                                strip_text = single_equation_txt.replace("Eq_", "")
                                if "_" in strip_text:
                                    new_equation_name = equation_name
                                    break
                                else:
                                    new_equation_name = equation_name + "_1"

                            equation_before = single_equation_txt
                    
                    new_equation_element = Element(new_equation_name)
                    insert_index = 0
                    for single_equation in chapter_tree_element:
                        if single_equation.tag > new_equation_name:
                            break
                        insert_index += 1
                    chapter_tree_element.insert(insert_index, new_equation_element)
        
        tree.write(file_name)
        
    # Extracts the number at the end until an underscore apears
    def get_number_before_underscore(self, processed_text):
        extracted_elements = ""
        while "_" not in processed_text[-1]:
            extracted_elements += processed_text[-1]
            processed_text = processed_text[:-1]
        
        return extracted_elements[::-1]
        
    # Deletes numbers before an undescore appears
    def strip_number_before_underscore(self, processed_text):
        while "_" not in processed_text[-1]:
            processed_text = processed_text[:-1]
        
        return processed_text
        
    # Opens the calculator window
    def show_calculator(self):
        global current_calc, calculator_name
        
        # Shows the calculator window only if an equation is selected
        if "Eq" in menu_window_text:
            if tree_widget_selection == "menu":
                selected_menu_item = self.menu.currentItem()
                tree_path_elements = self.get_parent_path(selected_menu_item)
                tree_path_elements = tree_path_elements.split("/")
                
                modified_window_text = menu_window_text.replace('Eq','')
                modified_window_text = tree_path_elements[1] + modified_window_text
                
                calculator_name = modified_window_text + "_menu"
                current_calc = Calcs[modified_window_text]
                Calcs[modified_window_text].show()
                Calcs[modified_window_text].setWindowTitle(calculator_name)
                
                if language == "English":
                    Calcs[modified_window_text].widget.calculate_btn.setText("Calculate")
                    Calcs[modified_window_text].widget.notebook_btn.setText("Open notebook")
                if language == "German":
                    Calcs[modified_window_text].widget.calculate_btn.setText("Berechnen")
                    Calcs[modified_window_text].widget.notebook_btn.setText("Notizbuch öffnen")
                
                if Calcs[modified_window_text].widget.non_equation == True and \
                Calcs[modified_window_text].widget.left_right_naming == True:
                    if language == "English":
                        Calcs[modified_window_text].widget.result_parameter_name1.setText("Right side: ")
                        Calcs[modified_window_text].widget.result_parameter_name2.setText("Left side: ")
                    if language == "German":
                        Calcs[modified_window_text].widget.result_parameter_name1.setText("Rechte Seite: ")
                        Calcs[modified_window_text].widget.result_parameter_name2.setText("Linke Seite: ")
                
            elif tree_widget_selection == "bookmark":
                selected_menu_item = self.bookmark_widgets[selected_tab].currentItem()
                tree_path_elements = self.get_parent_path(selected_menu_item)
                tree_path_elements = tree_path_elements.split("/")
                
                modified_window_text = menu_window_text.replace('Eq','')
                modified_window_text = tree_path_elements[1] + modified_window_text
                orginal_text = modified_window_text
                
                # Code for removing the "_1" in e.g. "EN_1992-1-1_Eq_3.3_1"
                length_to_remove = 0
                count = 1
                while "_" not in modified_window_text[-count]:
                    if "." in modified_window_text[-count]:
                        length_to_remove = 0
                        break
                    length_to_remove += 1
                    count += 1
                    
                if length_to_remove != 0:
                    modified_window_text = modified_window_text[:-length_to_remove - 1]
                
                calculator_name = orginal_text + "_tab" + str(selected_tab)
                current_calc = Calcs[modified_window_text]
                Calcs[modified_window_text].show()
                Calcs[modified_window_text].setWindowTitle(calculator_name)
                
                if language == "English":
                    Calcs[modified_window_text].widget.calculate_btn.setText("Calculate")
                    Calcs[modified_window_text].widget.notebook_btn.setText("Open notebook")
                    
                if language == "German":
                    Calcs[modified_window_text].widget.calculate_btn.setText("Berechnen")
                    Calcs[modified_window_text].widget.notebook_btn.setText("Notizbuch öffnen")
            
                if Calcs[modified_window_text].widget.non_equation == True and \
                Calcs[modified_window_text].widget.left_right_naming == True:
                    if language == "English":
                        Calcs[modified_window_text].widget.result_parameter_name1.setText("Right side: ")
                        Calcs[modified_window_text].widget.result_parameter_name2.setText("Left side: ")
                    if language == "German":
                        Calcs[modified_window_text].widget.result_parameter_name1.setText("Rechte Seite: ")
                        Calcs[modified_window_text].widget.result_parameter_name2.setText("Linke Seite: ")
            
    # Delets the equation from the bookmark window and the xml file
    def delete_eq(self):
        selected_bookmark_item = self.bookmark_widgets[selected_tab].currentItem()
        tree_path_elements = self.get_parent_path(selected_bookmark_item)
        tree_path_elements = tree_path_elements.split("/")
        
        root = self.bookmark_widgets[selected_tab].invisibleRootItem()
        item = self.bookmark_widgets[selected_tab].selectedItems()
        current_element_to_delete = item[0].text(0)
        
        # Gets the root and removes all children if any are present
        if current_element_to_delete != "EN_1992": #TODO: change for other EC
            (item[0].parent() or root).removeChild(item[0])
        
        # The if statements delete content from XML file
        if "Eq" in current_element_to_delete:
            EC_chapter = tree_path_elements[1]
            EC_chapter = "<" + EC_chapter + ">"
        
            file_name = "xml_trees/bookmark_tree" + str(selected_tab) + ".xml"
            fp = open(file_name, "r")
            bookmark_file_lines = fp.read()
            fp.close()
            
            bookmark_parts = bookmark_file_lines.split(EC_chapter)
            
            string_to_delete = "<" + current_element_to_delete + " />"
            start_position = bookmark_parts[1].find(string_to_delete)
            
            string_to_delete_length = len(string_to_delete)
            end_position = start_position + string_to_delete_length

            bookmark_parts_edited = bookmark_parts[1][:start_position] + \
                                    bookmark_parts[1][end_position:]
                                    
            bookmark_file_lines = bookmark_parts[0] + EC_chapter + \
                                  bookmark_parts_edited
            fp = open(file_name, "w")
            fp.write(bookmark_file_lines)
            fp.close()
            
        elif "Chapter" in current_element_to_delete:
            EC_chapter = tree_path_elements[1]
            EC_chapter = "<" + EC_chapter + ">"
        
            file_name = "xml_trees/bookmark_tree" + str(selected_tab) + ".xml"
            fp = open(file_name, "r")
            bookmark_file_lines = fp.read()
            fp.close()
            
            bookmark_parts = bookmark_file_lines.split(EC_chapter)
            
            string_to_delete1 = "<" + current_element_to_delete + ">"
            string_to_delete2 = "</" + current_element_to_delete + ">"
            
            string_to_delete_length2 = len(string_to_delete2)
            start_position = bookmark_parts[1].find(string_to_delete1)
            end_position = bookmark_parts[1].find(string_to_delete2) + \
                           string_to_delete_length2
            
            bookmark_parts_edited = bookmark_parts[1][:start_position] + \
                                    bookmark_parts[1][end_position:]
                                    
            bookmark_file_lines = bookmark_parts[0] + EC_chapter + \
                                  bookmark_parts_edited
            fp = open(file_name, "w")
            fp.write(bookmark_file_lines)
            fp.close()
        
        elif "EN" in current_element_to_delete and "-" in current_element_to_delete:
            file_name = "xml_trees/bookmark_tree" + str(selected_tab) + ".xml"
            fp = open(file_name, "r")
            bookmark_file_lines = fp.read()
            fp.close()
            
            string_to_delete1 = "<" + current_element_to_delete + ">"
            string_to_delete2 = "</" + current_element_to_delete + ">"
            
            string_to_delete_length2 = len(string_to_delete2)
            start_position = bookmark_file_lines.find(string_to_delete1)
            end_position = bookmark_file_lines.find(string_to_delete2) + \
                           string_to_delete_length2
                           
            bookmark_file_lines = bookmark_file_lines[:start_position] + \
                                  bookmark_file_lines[end_position:]
                                  
            fp = open(file_name, "w")
            fp.write(bookmark_file_lines)
            fp.close()
                        
    # Creates a new tab depending on the structure of opened tabs
    def create_tab(self):
        global current_tab, opened_tabs
        tabs_are_orderd = True
        opened_tabs_sorted = opened_tabs[:]
        opened_tabs_sorted.sort()
        
        # Handles the case that tabs have numbers between them
        for index in range(len(opened_tabs) - 1):
            if opened_tabs_sorted[index] == opened_tabs_sorted[index+1] - 1:
                pass
            else:
                tabs_are_orderd = False
                if current_tab <= 20:
                    current_tab = opened_tabs_sorted[index] + 1
                    opened_tabs.append(current_tab)
                break

        # Handels the case if tabs are ordered
        if 1 not in opened_tabs and tabs_are_orderd:
            current_tab = 1
            opened_tabs.append(current_tab)
        # Makes a boundary for max 20 opened tabs
        elif tabs_are_orderd and current_tab <= 19:
            current_tab = max(opened_tabs) + 1
            if current_tab > 20:
                current_tab = max(opened_tabs)
                return
            else:
                opened_tabs.append(current_tab)
        # Does nothing if 20 tabs are opened
        elif tabs_are_orderd:
            return
        
        # Creates a new QTreeWidget in the stacked widget
        self.bookmark_widgets[current_tab] = QTreeWidget()        
        if language == "English":
            self.bookmark_widgets[current_tab].setHeaderLabels(("Bookmarked equations",))
        elif language == "German":
            self.bookmark_widgets[current_tab].setHeaderLabels(("Lesezeichen Gleichungen",))
        
        self.bookmark_widgets[current_tab].setMinimumWidth(190)
        self.bookmark_widgets[current_tab].setMaximumWidth(190) #TEST
        self.tabs.addTab(self.bookmark_widgets[current_tab], "Tab " + str(current_tab))
        self.bookmark_widgets[current_tab].itemClicked.connect(self.bookmark_clicked_item)
        
        file_name = "xml_trees/bookmark_tree" + str(current_tab) + ".xml"
        file_exists = exists(file_name)
        
        if file_exists:
            fp = open(file_name, "r")
            file_content = fp.read()
            fp.close()
        
        if not file_exists or file_content == "":
            fp = open(file_name, 'w')
            fp.write("<EN_1992>\n</EN_1992>")
            fp.close()
            
            with open(file_name, "r") as file:
                xmlFileString = file.read()
            self.populate_tree_widget(self.bookmark_widgets[current_tab], xmlFileString)
        else:
            with open(file_name, "r") as file:
                xmlFileString = file.read()
            self.populate_tree_widget(self.bookmark_widgets[current_tab], xmlFileString)

    # Updates the globbal selected_tab parameter
    def update_current_tab(self, tab_index):
        global selected_tab
        selected_tab = opened_tabs[tab_index]
        
        if language == "English":
            self.bookmark_widgets[selected_tab].setHeaderLabels(("Bookmarked equations",))
        elif language == "German":
            self.bookmark_widgets[selected_tab].setHeaderLabels(("Lesezeichen Gleichungen",))

# Class for scrollable label containing picture
class ScrollPicture(QScrollArea):
    # Constructor
    def __init__(self, *args, **kwargs):
        QScrollArea.__init__(self, *args, **kwargs)
 
        # making widget resizable
        self.setWidgetResizable(True)
        self.setMinimumSize(QSize(900, 400))
                
        # making qwidget object
        content = QWidget(self)
        self.setWidget(content)
 
        # vertical box layout
        lay = QVBoxLayout(content)
 
        # creating label
        self.label = QLabel(content)

        # setting alignment to the text
        self.label.setAlignment(Qt.AlignCenter)
 
        # adding label to the layout
        lay.addWidget(self.label)
        
    # Sets the picture to the label
    def setPicture(self, path):
        self.label.setPixmap(QPixmap(path))

# Global variables
menu_window_text = "empty"
current_tab = 1
selected_tab = 1
opened_tabs = []
language = "English"
tree_widget_selection = "menu"
current_calc = ""
calculator_name = ""
# registered_global = False

# Creates the application handler. Has to be first line in code out of classes.
app = QApplication(sys.argv)

Calcs = dict()
Calcs = {  "EN_1992-1-1_3.1": EN_1992_1_1__3_1()
         , "EN_1992-1-1_3.2": EN_1992_1_1__3_2()
         , "EN_1992-1-1_3.3": EN_1992_1_1__3_3()
         , "EN_1992-1-1_3.4": EN_1992_1_1__3_4()
         , "EN_1992-1-1_3.5": EN_1992_1_1__3_5()
         , "EN_1992-1-1_3.6": EN_1992_1_1__3_6()
         , "EN_1992-1-1_3.7": EN_1992_1_1__3_7()
         , "EN_1992-1-1_3.8": EN_1992_1_1__3_8()
         , "EN_1992-1-1_3.9": EN_1992_1_1__3_9()
         , "EN_1992-1-1_3.10": EN_1992_1_1__3_10()
         , "EN_1992-1-1_3.11": EN_1992_1_1__3_11()
         , "EN_1992-1-1_3.12": EN_1992_1_1__3_12()
         , "EN_1992-1-1_3.13": EN_1992_1_1__3_13()
         , "EN_1992-1-1_3.14": EN_1992_1_1__3_14()
         , "EN_1992-1-1_3.15": EN_1992_1_1__3_15()
         , "EN_1992-1-1_3.16": EN_1992_1_1__3_16()
         , "EN_1992-1-1_3.17": EN_1992_1_1__3_17()
         , "EN_1992-1-1_3.19": EN_1992_1_1__3_19()
         , "EN_1992-1-1_3.21": EN_1992_1_1__3_21()
         , "EN_1992-1-1_3.23": EN_1992_1_1__3_23()
         , "EN_1992-1-1_3.24": EN_1992_1_1__3_24()
         , "EN_1992-1-1_3.25": EN_1992_1_1__3_25()
         , "EN_1992-1-1_3.26": EN_1992_1_1__3_26()
         , "EN_1992-1-1_3.27": EN_1992_1_1__3_27()
         , "EN_1992-1-1_3.28": EN_1992_1_1__3_28()
         , "EN_1992-1-1_3.29": EN_1992_1_1__3_29()
         , "EN_1992-1-1_3.30": EN_1992_1_1__3_30()
         , "EN_1992-1-1_4.1": EN_1992_1_1__4_1()
         , "EN_1992-1-1_4.2": EN_1992_1_1__4_2()
         , "EN_1992-1-1_5.1": EN_1992_1_1__5_1()
         , "EN_1992-1-1_5.2": EN_1992_1_1__5_2()
         , "EN_1992-1-1_5.3a": EN_1992_1_1__5_3a()
         , "EN_1992-1-1_5.3b": EN_1992_1_1__5_3b()
         , "EN_1992-1-1_5.4": EN_1992_1_1__5_4()
         , "EN_1992-1-1_5.5": EN_1992_1_1__5_5()
         , "EN_1992-1-1_5.6": EN_1992_1_1__5_6()
         , "EN_1992-1-1_5.7": EN_1992_1_1__5_7()
         , "EN_1992-1-1_5.7a": EN_1992_1_1__5_7a()
         , "EN_1992-1-1_5.8": EN_1992_1_1__5_8()
         , "EN_1992-1-1_5.9": EN_1992_1_1__5_9()
         , "EN_1992-1-1_5.10a": EN_1992_1_1__5_10a()
         , "EN_1992-1-1_5.10b": EN_1992_1_1__5_10b()
         , "EN_1992-1-1_5.11": EN_1992_1_1__5_11()
         , "EN_1992-1-1_5.12": EN_1992_1_1__5_12()
         , "EN_1992-1-1_5.13": EN_1992_1_1__5_13()
         , "EN_1992-1-1_5.14": EN_1992_1_1__5_14()
         , "EN_1992-1-1_5.15": EN_1992_1_1__5_15()
         , "EN_1992-1-1_5.16": EN_1992_1_1__5_16()
         , "EN_1992-1-1_5.17": EN_1992_1_1__5_17()
         , "EN_1992-1-1_5.18": EN_1992_1_1__5_18()
         , "EN_1992-1-1_5.19": EN_1992_1_1__5_19()
         , "EN_1992-1-1_5.20": EN_1992_1_1__5_20()
         , "EN_1992-1-1_5.21": EN_1992_1_1__5_21()
         , "EN_1992-1-1_5.22": EN_1992_1_1__5_22()
         , "EN_1992-1-1_5.26": EN_1992_1_1__5_26()
         , "EN_1992-1-1_5.27": EN_1992_1_1__5_27()
         , "EN_1992-1-1_5.28": EN_1992_1_1__5_28()
         , "EN_1992-1-1_5.29": EN_1992_1_1__5_29()
         , "EN_1992-1-1_5.30": EN_1992_1_1__5_30()
         , "EN_1992-1-1_5.31": EN_1992_1_1__5_31()
         , "EN_1992-1-1_5.32": EN_1992_1_1__5_32()
         , "EN_1992-1-1_5.33": EN_1992_1_1__5_33()
         , "EN_1992-1-1_5.34": EN_1992_1_1__5_34()
         , "EN_1992-1-1_5.35": EN_1992_1_1__5_35()
         , "EN_1992-1-1_5.36": EN_1992_1_1__5_36()
         , "EN_1992-1-1_5.37": EN_1992_1_1__5_37()
         , "EN_1992-1-1_5.38b": EN_1992_1_1__5_38b()
         , "EN_1992-1-1_5.39": EN_1992_1_1__5_39()
         , "EN_1992-1-1_5.40a": EN_1992_1_1__5_40a()
         , "EN_1992-1-1_5.40b": EN_1992_1_1__5_40b()
         , "EN_1992-1-1_5.41": EN_1992_1_1__5_41()
         , "EN_1992-1-1_5.42": EN_1992_1_1__5_42()
         , "EN_1992-1-1_5.43": EN_1992_1_1__5_43()
         , "EN_1992-1-1_5.44": EN_1992_1_1__5_44()
         , "EN_1992-1-1_5.45": EN_1992_1_1__5_45()
         , "EN_1992-1-1_5.46": EN_1992_1_1__5_46()
         , "EN_1992-1-1_5.47": EN_1992_1_1__5_47()
         , "EN_1992-1-1_6.1": EN_1992_1_1__6_1()
         , "EN_1992-1-1_6.2a": EN_1992_1_1__6_2a()
         , "EN_1992-1-1_6.2b": EN_1992_1_1__6_2b()
         , "EN_1992-1-1_6.3": EN_1992_1_1__6_3()
         , "EN_1992-1-1_6.4": EN_1992_1_1__6_4()
         , "EN_1992-1-1_6.5": EN_1992_1_1__6_5()
         , "EN_1992-1-1_6.6": EN_1992_1_1__6_6()
         , "EN_1992-1-1_6.7a": EN_1992_1_1__6_7a()
         , "EN_1992-1-1_6.7b": EN_1992_1_1__6_7b()
         , "EN_1992-1-1_6.8": EN_1992_1_1__6_8()
         , "EN_1992-1-1_6.9": EN_1992_1_1__6_9()
         , "EN_1992-1-1_6.10": EN_1992_1_1__6_10()
         , "EN_1992-1-1_6.11a": EN_1992_1_1__6_11a()
         , "EN_1992-1-1_6.11c": EN_1992_1_1__6_11c()
         , "EN_1992-1-1_6.12": EN_1992_1_1__6_12()
         , "EN_1992-1-1_6.13": EN_1992_1_1__6_13()
         , "EN_1992-1-1_6.14": EN_1992_1_1__6_14()
         , "EN_1992-1-1_6.15": EN_1992_1_1__6_15()
         , "EN_1992-1-1_6.16": EN_1992_1_1__6_16()
         , "EN_1992-1-1_6.17": EN_1992_1_1__6_17()
         , "EN_1992-1-1_6.18": EN_1992_1_1__6_18()
         , "EN_1992-1-1_6.19": EN_1992_1_1__6_19()
         , "EN_1992-1-1_6.20": EN_1992_1_1__6_20()
         , "EN_1992-1-1_6.21": EN_1992_1_1__6_21()
         , "EN_1992-1-1_6.22": EN_1992_1_1__6_22()
         , "EN_1992-1-1_6.24": EN_1992_1_1__6_24()
         , "EN_1992-1-1_6.25": EN_1992_1_1__6_25()
         , "EN_1992-1-1_6.26": EN_1992_1_1__6_26()
         , "EN_1992-1-1_6.27": EN_1992_1_1__6_27()
         , "EN_1992-1-1_6.28": EN_1992_1_1__6_28()
         , "EN_1992-1-1_6.29": EN_1992_1_1__6_29()
         , "EN_1992-1-1_6.30": EN_1992_1_1__6_30()
         , "EN_1992-1-1_6.31": EN_1992_1_1__6_31()
         , "EN_1992-1-1_6.32": EN_1992_1_1__6_32()
         , "EN_1992-1-1_6.33": EN_1992_1_1__6_33()
         , "EN_1992-1-1_6.34": EN_1992_1_1__6_34()
         , "EN_1992-1-1_6.34": EN_1992_1_1__6_34()
         , "EN_1992-1-1_6.35": EN_1992_1_1__6_35()
         , "EN_1992-1-1_6.36": EN_1992_1_1__6_36()
         , "EN_1992-1-1_6.37": EN_1992_1_1__6_37()
         , "EN_1992-1-1_6.38": EN_1992_1_1__6_38()
         , "EN_1992-1-1_6.39": EN_1992_1_1__6_39()
         , "EN_1992-1-1_6.41": EN_1992_1_1__6_41()
         , "EN_1992-1-1_6.42": EN_1992_1_1__6_42()
         , "EN_1992-1-1_6.43": EN_1992_1_1__6_43()
         , "EN_1992-1-1_6.43.AT": EN_1992_1_1__6_43_AT()
         , "EN_1992-1-1_6.44": EN_1992_1_1__6_44()
         , "EN_1992-1-1_6.45": EN_1992_1_1__6_45()
         , "EN_1992-1-1_6.46": EN_1992_1_1__6_46()
         , "EN_1992-1-1_6.47": EN_1992_1_1__6_47()
         , "EN_1992-1-1_6.48": EN_1992_1_1__6_48()
         , "EN_1992-1-1_6.49": EN_1992_1_1__6_49()
         , "EN_1992-1-1_6.50": EN_1992_1_1__6_50()
         , "EN_1992-1-1_6.51": EN_1992_1_1__6_51()
         , "EN_1992-1-1_6.52": EN_1992_1_1__6_52()
         , "EN_1992-1-1_6.53": EN_1992_1_1__6_53()
         , "EN_1992-1-1_6.54": EN_1992_1_1__6_54()
         , "EN_1992-1-1_6.56": EN_1992_1_1__6_56()
         , "EN_1992-1-1_6.57": EN_1992_1_1__6_57()
         , "EN_1992-1-1_6.58": EN_1992_1_1__6_58()
         , "EN_1992-1-1_6.59": EN_1992_1_1__6_59()
         , "EN_1992-1-1_6.60": EN_1992_1_1__6_60()
         , "EN_1992-1-1_6.61": EN_1992_1_1__6_61()
         , "EN_1992-1-1_6.62": EN_1992_1_1__6_62()
         , "EN_1992-1-1_6.63": EN_1992_1_1__6_63()
         , "EN_1992-1-1_6.64": EN_1992_1_1__6_64()
         , "EN_1992-1-1_6.65": EN_1992_1_1__6_65()
         , "EN_1992-1-1_6.67": EN_1992_1_1__6_67()
         , "EN_1992-1-1_6.69": EN_1992_1_1__6_69()
         , "EN_1992-1-1_6.71": EN_1992_1_1__6_71()
         , "EN_1992-1-1_6.72": EN_1992_1_1__6_72()
         , "EN_1992-1-1_6.76": EN_1992_1_1__6_76()
         , "EN_1992-1-1_6.77": EN_1992_1_1__6_77()
         , "EN_1992-1-1_6.78": EN_1992_1_1__6_78()
         , "EN_1992-1-1_6.79": EN_1992_1_1__6_79()
         , "EN_1992-1-1_7.1": EN_1992_1_1__7_1()
         , "EN_1992-1-1_7.2": EN_1992_1_1__7_2()
         , "EN_1992-1-1_7.3": EN_1992_1_1__7_3()
         , "EN_1992-1-1_7.4": EN_1992_1_1__7_4()
         , "EN_1992-1-1_7.5": EN_1992_1_1__7_5()
         , "EN_1992-1-1_7.6": EN_1992_1_1__7_6()
         , "EN_1992-1-1_7.7": EN_1992_1_1__7_7()
         , "EN_1992-1-1_7.7.1.AT": EN_1992_1_1__7_7_1_AT()
         , "EN_1992-1-1_7.7.2.AT": EN_1992_1_1__7_7_2_AT()
         , "EN_1992-1-1_7.7.3.AT": EN_1992_1_1__7_7_3_AT()
         , "EN_1992-1-1_7.8": EN_1992_1_1__7_8()
         , "EN_1992-1-1_7.9": EN_1992_1_1__7_9()
         , "EN_1992-1-1_7.10": EN_1992_1_1__7_10()
         , "EN_1992-1-1_7.11": EN_1992_1_1__7_11()
         , "EN_1992-1-1_7.11.DE": EN_1992_1_1__7_11_DE()
         , "EN_1992-1-1_7.11.AT": EN_1992_1_1__7_11_AT()
         , "EN_1992-1-1_7.12": EN_1992_1_1__7_12()
         , "EN_1992-1-1_7.13": EN_1992_1_1__7_13()
         , "EN_1992-1-1_7.14": EN_1992_1_1__7_14()
         , "EN_1992-1-1_7.15": EN_1992_1_1__7_15()
         , "EN_1992-1-1_7.16a": EN_1992_1_1__7_16a()
         , "EN_1992-1-1_7.16b": EN_1992_1_1__7_16b()
         , "EN_1992-1-1_7.17": EN_1992_1_1__7_17()
         , "EN_1992-1-1_7.18": EN_1992_1_1__7_18()
         , "EN_1992-1-1_7.19": EN_1992_1_1__7_19()
         , "EN_1992-1-1_7.20": EN_1992_1_1__7_20()
         , "EN_1992-1-1_7.21": EN_1992_1_1__7_21()
         , "EN_1992-1-1_8.1": EN_1992_1_1__8_1()
         , "EN_1992-1-1_8.2": EN_1992_1_1__8_2()
         , "EN_1992-1-1_8.3": EN_1992_1_1__8_3()
         , "EN_1992-1-1_8.4": EN_1992_1_1__8_4()
         , "EN_1992-1-1_8.5": EN_1992_1_1__8_5()
         , "EN_1992-1-1_8.6": EN_1992_1_1__8_6()
         , "EN_1992-1-1_8.7": EN_1992_1_1__8_7()
         , "EN_1992-1-1_8.8": EN_1992_1_1__8_8()
         , "EN_1992-1-1_8.9": EN_1992_1_1__8_9()
         , "EN_1992-1-1_8.10": EN_1992_1_1__8_10()
         , "EN_1992-1-1_8.11": EN_1992_1_1__8_11()
         , "EN_1992-1-1_8.12": EN_1992_1_1__8_12()
         , "EN_1992-1-1_8.13": EN_1992_1_1__8_13()
         , "EN_1992-1-1_8.14": EN_1992_1_1__8_14()
         , "EN_1992-1-1_8.15": EN_1992_1_1__8_15()
         , "EN_1992-1-1_8.16": EN_1992_1_1__8_16()
         , "EN_1992-1-1_8.17": EN_1992_1_1__8_17()
         , "EN_1992-1-1_8.18": EN_1992_1_1__8_18()
         , "EN_1992-1-1_8.19": EN_1992_1_1__8_19()
         , "EN_1992-1-1_8.20": EN_1992_1_1__8_20()
         , "EN_1992-1-1_8.21": EN_1992_1_1__8_21()
         , "EN_1992-1-1_9.1": EN_1992_1_1__9_1()
         , "EN_1992-1-1_9.2": EN_1992_1_1__9_2()
         , "EN_1992-1-1_9.3": EN_1992_1_1__9_3()
         , "EN_1992-1-1_9.4": EN_1992_1_1__9_4()
         , "EN_1992-1-1_9.5": EN_1992_1_1__9_5()
         , "EN_1992-1-1_9.5.DE": EN_1992_1_1__9_5_DE()
         , "EN_1992-1-1_9.5.AT": EN_1992_1_1__9_5_AT()
         , "EN_1992-1-1_9.6": EN_1992_1_1__9_6()
         , "EN_1992-1-1_9.7": EN_1992_1_1__9_7()
         , "EN_1992-1-1_9.7.DE": EN_1992_1_1__9_7_DE()
         , "EN_1992-1-1_9.7.AT": EN_1992_1_1__9_7_AT()
         , "EN_1992-1-1_9.8": EN_1992_1_1__9_8()
         , "EN_1992-1-1_9.9": EN_1992_1_1__9_9()
         , "EN_1992-1-1_9.10": EN_1992_1_1__9_10()
         , "EN_1992-1-1_9.11": EN_1992_1_1__9_11()
         , "EN_1992-1-1_9.11.DE": EN_1992_1_1__9_11_DE()
         , "EN_1992-1-1_9.12": EN_1992_1_1__9_12()
         , "EN_1992-1-1_9.12.DE": EN_1992_1_1__9_12_DE()
         , "EN_1992-1-1_9.12.AT": EN_1992_1_1__9_12_AT()
         , "EN_1992-1-1_9.12.SI": EN_1992_1_1__9_12_SI()
         , "EN_1992-1-1_9.13": EN_1992_1_1__9_13()
         , "EN_1992-1-1_9.14": EN_1992_1_1__9_14()
         , "EN_1992-1-1_9.15": EN_1992_1_1__9_15()
         , "EN_1992-1-1_9.16": EN_1992_1_1__9_16()
         , "EN_1992-1-1_10.1": EN_1992_1_1__10_1()
         , "EN_1992-1-1_10.2": EN_1992_1_1__10_2()
         , "EN_1992-1-1_10.3": EN_1992_1_1__10_3()
         , "EN_1992-1-1_10.4": EN_1992_1_1__10_4()
         , "EN_1992-1-1_10.5": EN_1992_1_1__10_5()
         , "EN_1992-1-1_10.6": EN_1992_1_1__10_6()
         , "EN_1992-1-1_11.1": EN_1992_1_1__11_1()
         , "EN_1992-1-1_11.2": EN_1992_1_1__11_2()
         , "EN_1992-1-1_11.3.15": EN_1992_1_1__11_3_15()
         , "EN_1992-1-1_11.3.16": EN_1992_1_1__11_3_16()
         , "EN_1992-1-1_11.3.24": EN_1992_1_1__11_3_24()
         , "EN_1992-1-1_11.3.26": EN_1992_1_1__11_3_26()
         , "EN_1992-1-1_11.3.27": EN_1992_1_1__11_3_27()
         , "EN_1992-1-1_11.6.2": EN_1992_1_1__11_6_2()
         , "EN_1992-1-1_11.6.5": EN_1992_1_1__11_6_5()
         , "EN_1992-1-1_11.6.6N": EN_1992_1_1__11_6_6N()
         , "EN_1992-1-1_11.6.47": EN_1992_1_1__11_6_47()
         , "EN_1992-1-1_11.6.50": EN_1992_1_1__11_6_50()
         , "EN_1992-1-1_11.6.52": EN_1992_1_1__11_6_52()
         , "EN_1992-1-1_11.6.53": EN_1992_1_1__11_6_53()
         , "EN_1992-1-1_11.6.63": EN_1992_1_1__11_6_63()
         , "EN_1992-1-1_12.1": EN_1992_1_1__12_1()
         , "EN_1992-1-1_12.1.AT": EN_1992_1_1__12_1_AT()
         , "EN_1992-1-1_12.2": EN_1992_1_1__12_2()
         , "EN_1992-1-1_12.3": EN_1992_1_1__12_3()
         , "EN_1992-1-1_12.4": EN_1992_1_1__12_4()
         , "EN_1992-1-1_12.4.DE": EN_1992_1_1__12_4_DE()
         , "EN_1992-1-1_12.5": EN_1992_1_1__12_5()
         , "EN_1992-1-1_12.6": EN_1992_1_1__12_6()
         , "EN_1992-1-1_12.7": EN_1992_1_1__12_7()
         , "EN_1992-1-1_12.8": EN_1992_1_1__12_8()
         , "EN_1992-1-1_12.9": EN_1992_1_1__12_9()
         , "EN_1992-1-1_12.10": EN_1992_1_1__12_10()
         , "EN_1992-1-1_12.11": EN_1992_1_1__12_11()
         , "EN_1992-1-1_12.12": EN_1992_1_1__12_12()
         , "EN_1992-1-1_12.13": EN_1992_1_1__12_13()
         , "EN_1992-1-1_B.1": EN_1992_1_1__B_1()
         , "EN_1992-1-1_B.2": EN_1992_1_1__B_2()
         , "EN_1992-1-1_B.3a": EN_1992_1_1__B_3a()
         , "EN_1992-1-1_B.3b": EN_1992_1_1__B_3b()
         , "EN_1992-1-1_B.4": EN_1992_1_1__B_4()
         , "EN_1992-1-1_B.5": EN_1992_1_1__B_5()
         , "EN_1992-1-1_B.6": EN_1992_1_1__B_6()
         , "EN_1992-1-1_B.7": EN_1992_1_1__B_7()
         , "EN_1992-1-1_B.8a": EN_1992_1_1__B_8a()
         , "EN_1992-1-1_B.8b": EN_1992_1_1__B_8b()
         , "EN_1992-1-1_B.8c": EN_1992_1_1__B_8c()
         , "EN_1992-1-1_B.9": EN_1992_1_1__B_9()
         , "EN_1992-1-1_B.11": EN_1992_1_1__B_11()
         , "EN_1992-1-1_B.12": EN_1992_1_1__B_12()
         , "EN_1992-1-1_C.1": EN_1992_1_1__C_1()
         , "EN_1992-1-1_C.2": EN_1992_1_1__C_2()
         , "EN_1992-1-1_C.3": EN_1992_1_1__C_3()
         , "EN_1992-1-1_D.1": EN_1992_1_1__D_1()
         , "EN_1992-1-1_D.2": EN_1992_1_1__D_2()
         , "EN_1992-1-1_F.1": EN_1992_1_1__F_1()
         , "EN_1992-1-1_F.2": EN_1992_1_1__F_2()
         , "EN_1992-1-1_F.3": EN_1992_1_1__F_3()
         , "EN_1992-1-1_F.4": EN_1992_1_1__F_4()
         , "EN_1992-1-1_F.6": EN_1992_1_1__F_6()
         , "EN_1992-1-1_F.7": EN_1992_1_1__F_7()
         , "EN_1992-1-1_F.8": EN_1992_1_1__F_8()
         , "EN_1992-1-1_F.9": EN_1992_1_1__F_9()
         , "EN_1992-1-1_F.10": EN_1992_1_1__F_10()
         , "EN_1992-1-1_G.1": EN_1992_1_1__G_1()
         , "EN_1992-1-1_H.1": EN_1992_1_1__H_1()
         , "EN_1992-1-1_H.2": EN_1992_1_1__H_2()
         , "EN_1992-1-1_H.3": EN_1992_1_1__H_3()
         , "EN_1992-1-1_H.4": EN_1992_1_1__H_4()
         , "EN_1992-1-1_H.5": EN_1992_1_1__H_5()
         , "EN_1992-1-1_H.6": EN_1992_1_1__H_6()
         , "EN_1992-1-1_H.7": EN_1992_1_1__H_7()
         , "EN_1992-1-1_H.8": EN_1992_1_1__H_8()
         , "EN_1992-1-1_I.1": EN_1992_1_1__I_1()
         , "EN_1992-1-2_2.1": EN_1992_1_2__2_1()
         , "EN_1992-1-2_2.2a": EN_1992_1_2__2_2a()
         , "EN_1992-1-2_2.2b": EN_1992_1_2__2_2b()
         , "EN_1992-1-2_2.4": EN_1992_1_2__2_4()
         , "EN_1992-1-2_2.5": EN_1992_1_2__2_5()
         , "EN_1992-1-2_2.5a": EN_1992_1_2__2_5a()
         , "EN_1992-1-2_2.5b": EN_1992_1_2__2_5b()
         , "EN_1992-1-2_3.1": EN_1992_1_2__3_1()
         , "EN_1992-1-2_3.2": EN_1992_1_2__3_2()
         , "EN_1992-1-2_4.15": EN_1992_1_2__4_15()
         , "EN_1992-1-2_5.1": EN_1992_1_2__5_1()
         , "EN_1992-1-2_5.2": EN_1992_1_2__5_2()
         , "EN_1992-1-2_5.3": EN_1992_1_2__5_3()
         , "EN_1992-1-2_5.4": EN_1992_1_2__5_4()
         , "EN_1992-1-2_5.5": EN_1992_1_2__5_5()
         , "EN_1992-1-2_5.6": EN_1992_1_2__5_6()
         , "EN_1992-1-2_5.7": EN_1992_1_2__5_7()
         , "EN_1992-1-2_5.8a": EN_1992_1_2__5_8a()
         , "EN_1992-1-2_5.8b": EN_1992_1_2__5_8b()
         , "EN_1992-1-2_5.8c": EN_1992_1_2__5_8c()
         , "EN_1992-1-2_5.9": EN_1992_1_2__5_9()
         , "EN_1992-1-2_5.10": EN_1992_1_2__5_10()
         , "EN_1992-1-2_5.11": EN_1992_1_2__5_11()
         , "EN_1992-1-2_6.5": EN_1992_1_2__6_5()
         , "EN_1992-1-2_B.1": EN_1992_1_2__B_1()
         , "EN_1992-1-2_B.2": EN_1992_1_2__B_2()
         , "EN_1992-1-2_B.3": EN_1992_1_2__B_3()
         , "EN_1992-1-2_B.4": EN_1992_1_2__B_4()
         , "EN_1992-1-2_B.5": EN_1992_1_2__B_5()
         , "EN_1992-1-2_B.6": EN_1992_1_2__B_6()
         , "EN_1992-1-2_B.7": EN_1992_1_2__B_7()
         , "EN_1992-1-2_B.8": EN_1992_1_2__B_8()
         , "EN_1992-1-2_B.9": EN_1992_1_2__B_9()
         , "EN_1992-1-2_B.10": EN_1992_1_2__B_10()
         , "EN_1992-1-2_B.11": EN_1992_1_2__B_11()
         , "EN_1992-1-2_B.12": EN_1992_1_2__B_12()
         , "EN_1992-1-2_B.13": EN_1992_1_2__B_13()
         , "EN_1992-1-2_B.14": EN_1992_1_2__B_14()
         , "EN_1992-1-2_E.2": EN_1992_1_2__E_2()
         , "EN_1992-1-2_E.3": EN_1992_1_2__E_3()
         , "EN_1992-1-2_E.4": EN_1992_1_2__E_4()
         , "EN_1992-1-2_E.5": EN_1992_1_2__E_5()
         , "EN_1992-1-2_AA.2.DE": EN_1992_1_2__AA_2_DE()
         , "EN_1992-1-2_AA.3.DE": EN_1992_1_2__AA_3_DE()
         , "EN_1992-1-2_AA.4.DE": EN_1992_1_2__AA_4_DE()
         , "EN_1992-1-2_AA.5.DE": EN_1992_1_2__AA_5_DE()
         , "EN_1992-1-2_AA.6.DE": EN_1992_1_2__AA_6_DE()
         , "EN_1992-1-2_AA.7.DE": EN_1992_1_2__AA_7_DE()
         , "EN_1992-1-2_AA.8.DE": EN_1992_1_2__AA_8_DE()
         , "EN_1992-1-2_AA.9.DE": EN_1992_1_2__AA_9_DE()
         , "EN_1992-1-2_AA.10.DE": EN_1992_1_2__AA_10_DE()
         , "EN_1992-1-2_AA.11.DE": EN_1992_1_2__AA_11_DE()
         , "EN_1992-1-2_AA.12.DE": EN_1992_1_2__AA_12_DE()
         , "EN_1992-1-2_AA.13.DE": EN_1992_1_2__AA_13_DE()
         , "EN_1992-1-2_AA.14.DE": EN_1992_1_2__AA_14_DE()
         , "EN_1992-1-2_AA.15.DE": EN_1992_1_2__AA_15_DE()
         , "EN_1992-2_3.15": EN_1992_2__3_15()
         , "EN_1992-2_3.16": EN_1992_2__3_16()
         , "EN_1992-2_5.9": EN_1992_2__5_9()
         , "EN_1992-2_5.10a": EN_1992_2__5_10a()
         , "EN_1992-2_5.10b": EN_1992_2__5_10b()
         , "EN_1992-2_5.11N.DE": EN_1992_2__5_11N_DE()
         , "EN_1992-2_5.12N.DE": EN_1992_2__5_12N_DE()
         , "EN_1992-2_5.13b.DE": EN_1992_2__5_13b_DE()
         , "EN_1992-2_5.41.DE": EN_1992_2__5_41_DE()
         , "EN_1992-2_5.101": EN_1992_2__5_101()
         , "EN_1992-2_5.102a": EN_1992_2__5_102a()
         , "EN_1992-2_5.102b": EN_1992_2__5_102b()
         , "EN_1992-2_5.102c": EN_1992_2__5_102c()
         , "EN_1992-2_6.2a": EN_1992_2__6_2a()
         , "EN_1992-2_6.2b": EN_1992_2__6_2b()
         , "EN_1992-2_6.3": EN_1992_2__6_3()
         , "EN_1992-2_6.7b.DE": EN_1992_2__6_7b_DE()
         , "EN_1992-2_6.8": EN_1992_2__6_8()
         , "EN_1992-2_6.9": EN_1992_2__6_9()
         , "EN_1992-2_6.10b": EN_1992_2__6_10b()
         , "EN_1992-2_6.11a": EN_1992_2__6_11a()
         , "EN_1992-2_6.11c": EN_1992_2__6_11c()
         , "EN_1992-2_6.12": EN_1992_2__6_12()
         , "EN_1992-2_6.18": EN_1992_2__6_18()
         , "EN_1992-2_6.20": EN_1992_2__6_20()
         , "EN_1992-2_6.22.DE": EN_1992_2__6_22_DE()
         , "EN_1992-2_6.27.DE": EN_1992_2__6_27_DE()
         , "EN_1992-2_6.28": EN_1992_2__6_28()
         , "EN_1992-2_6.29": EN_1992_2__6_29()
         , "EN_1992-2_6.30": EN_1992_2__6_30()
         , "EN_1992-2_6.31.1.DE": EN_1992_2__6_31_1_DE()
         , "EN_1992-2_6.31.2.DE": EN_1992_2__6_31_2_DE()
         , "EN_1992-2_6.33.DE": EN_1992_2__6_33_DE()
         , "EN_1992-2_6.34.DE": EN_1992_2__6_34_DE()
         , "EN_1992-2_6.35.DE": EN_1992_2__6_35_DE()
         , "EN_1992-2_6.39.DE": EN_1992_2__6_39_DE()
         , "EN_1992-2_106.50.DE": EN_1992_2__106_50_DE()
         , "EN_1992-2_6.51.DE": EN_1992_2__6_51_DE()
         , "EN_1992-2_6.52.1.DE": EN_1992_2__6_52_1_DE()
         , "EN_1992-2_6.52.2.DE": EN_1992_2__6_52_2_DE()
         , "EN_1992-2_6.53.DE": EN_1992_2__6_53_DE()
         , "EN_1992-2_6.54.DE": EN_1992_2__6_54_DE()
         , "EN_1992-2_6.59.DE": EN_1992_2__6_59_DE()
         , "EN_1992-2_6.76": EN_1992_2__6_76()
         , "EN_1992-2_6.101a": EN_1992_2__6_101a()
         , "EN_1992-2_6.101b": EN_1992_2__6_101b()
         , "EN_1992-2_6.102": EN_1992_2__6_102()
         , "EN_1992-2_6.103": EN_1992_2__6_103()
         , "EN_1992-2_6.104": EN_1992_2__6_104()
         , "EN_1992-2_6.106": EN_1992_2__6_106()
         , "EN_1992-2_6.107a.DE": EN_1992_2__6_107a_DE()
         , "EN_1992-2_6.110": EN_1992_2__6_110()
         , "EN_1992-2_6.111": EN_1992_2__6_111()
         , "EN_1992-2_6.112": EN_1992_2__6_112()
         , "EN_1992-2_7.1": EN_1992_2__7_1()
         , "EN_1992-2_7.2": EN_1992_2__7_2()
         , "EN_1992-2_7.3": EN_1992_2__7_3()
         , "EN_1992-2_7.4": EN_1992_2__7_4()
         , "EN_1992-2_7.5.1.DE": EN_1992_2__7_5_1_DE()
         , "EN_1992-2_7.5.2.DE": EN_1992_2__7_5_2_DE()
         , "EN_1992-2_7.5.3.DE": EN_1992_2__7_5_3_DE()
         , "EN_1992-2_7.5.4.DE": EN_1992_2__7_5_4_DE()
         , "EN_1992-2_7.6.DE": EN_1992_2__7_6_DE()
         , "EN_1992-2_7.7.DE": EN_1992_2__7_7_DE()
         , "EN_1992-2_7.7.1.DE": EN_1992_2__7_7_1_DE()
         , "EN_1992-2_7.8.DE": EN_1992_2__7_8_DE()
         , "EN_1992-2_7.105.1.DE": EN_1992_2__7_105_1_DE()
         , "EN_1992-2_7.105.2.DE": EN_1992_2__7_105_2_DE()
         , "EN_1992-2_8.19.DE": EN_1992_2__8_19_DE()
         , "EN_1992-2_8.21.DE": EN_1992_2__8_21_DE()
         , "EN_1992-2_9.3.DE": EN_1992_2__9_3_DE()
         , "EN_1992-2_9.5a.DE": EN_1992_2__9_5a_DE()
         , "EN_1992-2_9.5b.DE": EN_1992_2__9_5b_DE()
         , "EN_1992-2_9.7.DE": EN_1992_2__9_7_DE()
         , "EN_1992-2_9.11.DE": EN_1992_2__9_11_DE()
         , "EN_1992-2_9.12.DE": EN_1992_2__9_12_DE()
         , "EN_1992-2_B.113": EN_1992_2__B_113()
         , "EN_1992-2_B.114": EN_1992_2__B_114()
         , "EN_1992-2_B.115": EN_1992_2__B_115()
         , "EN_1992-2_B.116": EN_1992_2__B_116()
         , "EN_1992-2_B.117": EN_1992_2__B_117()
         , "EN_1992-2_B.118": EN_1992_2__B_118()
         , "EN_1992-2_B.119": EN_1992_2__B_119()
         , "EN_1992-2_B.120": EN_1992_2__B_120()
         , "EN_1992-2_B.121": EN_1992_2__B_121()
         , "EN_1992-2_B.122": EN_1992_2__B_122()
         , "EN_1992-2_B.123": EN_1992_2__B_123()
         , "EN_1992-2_B.124": EN_1992_2__B_124()
         , "EN_1992-2_B.125": EN_1992_2__B_125()
         , "EN_1992-2_B.126": EN_1992_2__B_126()
         , "EN_1992-2_B.127": EN_1992_2__B_127()
         , "EN_1992-2_B.128": EN_1992_2__B_128()
         , "EN_1992-2_F.2": EN_1992_2__F_2()
         , "EN_1992-2_F.3": EN_1992_2__F_3()
         , "EN_1992-2_F.4": EN_1992_2__F_4()
         , "EN_1992-2_F.6": EN_1992_2__F_6()
         , "EN_1992-2_F.7": EN_1992_2__F_7()
         , "EN_1992-2_F.8": EN_1992_2__F_8()
         , "EN_1992-2_F.9": EN_1992_2__F_9()
         , "EN_1992-2_F.10": EN_1992_2__F_10()
         , "EN_1992-2_J.101": EN_1992_2__J_101()
         , "EN_1992-2_J.102": EN_1992_2__J_102()
         , "EN_1992-2_KK.101": EN_1992_2__KK_101()
         , "EN_1992-2_KK.102": EN_1992_2__KK_102()
         , "EN_1992-2_KK.103": EN_1992_2__KK_103()
         , "EN_1992-2_KK.104": EN_1992_2__KK_104()
         , "EN_1992-2_KK.105": EN_1992_2__KK_105()
         , "EN_1992-2_KK.106": EN_1992_2__KK_106()
         , "EN_1992-2_KK.111": EN_1992_2__KK_111()
         , "EN_1992-2_KK.114": EN_1992_2__KK_114()
         , "EN_1992-2_KK.119": EN_1992_2__KK_119()
         , "EN_1992-2_LL.101": EN_1992_2__LL_101()
         , "EN_1992-2_LL.102": EN_1992_2__LL_102()
         , "EN_1992-2_LL.103": EN_1992_2__LL_103()
         , "EN_1992-2_LL.104": EN_1992_2__LL_104()
         , "EN_1992-2_LL.105": EN_1992_2__LL_105()
         , "EN_1992-2_LL.106": EN_1992_2__LL_106()
         , "EN_1992-2_LL.107": EN_1992_2__LL_107()
         , "EN_1992-2_LL.108": EN_1992_2__LL_108()
         , "EN_1992-2_LL.109": EN_1992_2__LL_109()
         , "EN_1992-2_LL.110": EN_1992_2__LL_110()
         , "EN_1992-2_LL.110": EN_1992_2__LL_110()
         , "EN_1992-2_LL.111": EN_1992_2__LL_111()
         , "EN_1992-2_LL.112": EN_1992_2__LL_112()
         , "EN_1992-2_LL.113": EN_1992_2__LL_113()
         , "EN_1992-2_LL.114": EN_1992_2__LL_114()
         , "EN_1992-2_LL.115": EN_1992_2__LL_115()
         , "EN_1992-2_LL.116": EN_1992_2__LL_116()
         , "EN_1992-2_LL.117": EN_1992_2__LL_117()
         , "EN_1992-2_LL.118": EN_1992_2__LL_118()
         , "EN_1992-2_LL.119": EN_1992_2__LL_119()
         , "EN_1992-2_LL.120": EN_1992_2__LL_120()
         , "EN_1992-2_LL.121": EN_1992_2__LL_121()
         , "EN_1992-2_LL.122": EN_1992_2__LL_122()
         , "EN_1992-2_LL.123": EN_1992_2__LL_123()
         , "EN_1992-2_LL.124": EN_1992_2__LL_124()
         , "EN_1992-2_LL.125": EN_1992_2__LL_125()
         , "EN_1992-2_LL.126": EN_1992_2__LL_126()
         , "EN_1992-2_LL.127": EN_1992_2__LL_127()
         , "EN_1992-2_LL.137": EN_1992_2__LL_137()
         , "EN_1992-2_LL.138": EN_1992_2__LL_138()
         , "EN_1992-2_LL.139": EN_1992_2__LL_139()
         , "EN_1992-2_LL.140": EN_1992_2__LL_140()
         , "EN_1992-2_LL.141": EN_1992_2__LL_141()
         , "EN_1992-2_LL.142": EN_1992_2__LL_142()
         , "EN_1992-2_LL.143": EN_1992_2__LL_143()
         , "EN_1992-2_LL.144": EN_1992_2__LL_144()
         , "EN_1992-2_LL.145": EN_1992_2__LL_145()
         , "EN_1992-2_LL.146": EN_1992_2__LL_146()
         , "EN_1992-2_LL.147": EN_1992_2__LL_147()
         , "EN_1992-2_LL.148": EN_1992_2__LL_148()
         , "EN_1992-2_LL.149": EN_1992_2__LL_149()
         , "EN_1992-2_LL.150": EN_1992_2__LL_150()
         , "EN_1992-2_MM.101": EN_1992_2__MM_101()
         , "EN_1992-2_MM.102": EN_1992_2__MM_102()
         , "EN_1992-2_MM.103": EN_1992_2__MM_103()
         , "EN_1992-2_MM.104": EN_1992_2__MM_104()
         , "EN_1992-2_MM.105": EN_1992_2__MM_105()
         , "EN_1992-2_MM.106": EN_1992_2__MM_106()
         , "EN_1992-2_NN.1.DE": EN_1992_2__NN_1_DE()
         , "EN_1992-2_NN.2.DE": EN_1992_2__NN_2_DE()
         , "EN_1992-2_NN.3.DE": EN_1992_2__NN_3_DE()
         , "EN_1992-2_NN.4.DE": EN_1992_2__NN_4_DE()
         , "EN_1992-2_NN.5.DE": EN_1992_2__NN_5_DE()
         , "EN_1992-2_NN.6.DE": EN_1992_2__NN_6_DE()
         , "EN_1992-2_NN.7.DE": EN_1992_2__NN_7_DE()
         , "EN_1992-2_NN.8.DE": EN_1992_2__NN_8_DE()
         , "EN_1992-2_NN.9.DE": EN_1992_2__NN_9_DE()
         , "EN_1992-2_NN.10.DE": EN_1992_2__NN_10_DE()
         , "EN_1992-2_NN.11.DE": EN_1992_2__NN_11_DE()
         , "EN_1992-2_NN.12.DE": EN_1992_2__NN_12_DE()
         , "EN_1992-2_NN.14.DE": EN_1992_2__NN_14_DE()
         , "EN_1992-2_NN.15.DE": EN_1992_2__NN_15_DE()
         , "EN_1992-2_NN.16.DE": EN_1992_2__NN_16_DE()
         , "EN_1992-2_NN.17.DE": EN_1992_2__NN_17_DE()
         , "EN_1992-2_NN.18.DE": EN_1992_2__NN_18_DE()
         , "EN_1992-2_NN.19.DE": EN_1992_2__NN_19_DE()
         , "EN_1992-2_NN.20.DE": EN_1992_2__NN_20_DE()
         , "EN_1992-2_NN.101": EN_1992_2__NN_101()
         , "EN_1992-2_NN.102": EN_1992_2__NN_102()
         , "EN_1992-2_NN.103": EN_1992_2__NN_103()
         , "EN_1992-2_NN.104": EN_1992_2__NN_104()
         , "EN_1992-2_NN.105": EN_1992_2__NN_105()
         , "EN_1992-2_NN.106": EN_1992_2__NN_106()
         , "EN_1992-2_NN.107": EN_1992_2__NN_107()
         , "EN_1992-2_NN.108": EN_1992_2__NN_108()
         , "EN_1992-2_NN.109": EN_1992_2__NN_109()
         , "EN_1992-2_NN.110": EN_1992_2__NN_110()
         , "EN_1992-2_NN.111": EN_1992_2__NN_111()
         , "EN_1992-2_NN.112": EN_1992_2__NN_112()
         , "EN_1992-2_NN.113": EN_1992_2__NN_113()
         , "EN_1992-2_NN.114": EN_1992_2__NN_114()
         , "EN_1992-2_NN.115": EN_1992_2__NN_115()
         , "EN_1992-2_NN.116": EN_1992_2__NN_116()
         , "EN_1992-2_NN.117": EN_1992_2__NN_117()
         , "EN_1992-2_NN.118": EN_1992_2__NN_118()
         , "EN_1992-2_QQ.101": EN_1992_2__QQ_101()
         , "EN_1992-3_7.122": EN_1992_3__7_122()
         , "EN_1992-3_K.1": EN_1992_3__K_1()
         , "EN_1992-3_K.2": EN_1992_3__K_2()
         , "EN_1992-3_L.1": EN_1992_3__L_1()
         , "EN_1992-3_L.2": EN_1992_3__L_2()
         , "EN_1992-3_M.1": EN_1992_3__M_1()
         , "EN_1992-3_M.2": EN_1992_3__M_2()
         , "EN_1992-3_M.3": EN_1992_3__M_3()
         , "EN_1992-4_4.3": EN_1992_4__4_3()
         , "EN_1992-4_4.4": EN_1992_4__4_4()
         , "EN_1992-4_6.1": EN_1992_4__6_1()
         , "EN_1992-4_6.2": EN_1992_4__6_2()
         , "EN_1992-4_6.3": EN_1992_4__6_3()
         , "EN_1992-4_6.4": EN_1992_4__6_4()
         , "EN_1992-4_6.5": EN_1992_4__6_5()
         , "EN_1992-4_6.6": EN_1992_4__6_6()
         , "EN_1992-4_7.1": EN_1992_4__7_1()
         , "EN_1992-4_7.2": EN_1992_4__7_2()
         , "EN_1992-4_7.3": EN_1992_4__7_3()
         , "EN_1992-4_7.4": EN_1992_4__7_4()
         , "EN_1992-4_7.5": EN_1992_4__7_5()
         , "EN_1992-4_7.6": EN_1992_4__7_6()
         , "EN_1992-4_7.7": EN_1992_4__7_7()
         , "EN_1992-4_7.8": EN_1992_4__7_8()
         , "EN_1992-4_7.9": EN_1992_4__7_9()
         , "EN_1992-4_7.10": EN_1992_4__7_10()
         , "EN_1992-4_7.11": EN_1992_4__7_11()
         , "EN_1992-4_7.12": EN_1992_4__7_12()
         , "EN_1992-4_7.13": EN_1992_4__7_13()
         , "EN_1992-4_7.14": EN_1992_4__7_14()
         , "EN_1992-4_7.14ab": EN_1992_4__7_14ab()
         , "EN_1992-4_7.15": EN_1992_4__7_15()
         , "EN_1992-4_7.16": EN_1992_4__7_16()
         , "EN_1992-4_7.17": EN_1992_4__7_17()
         , "EN_1992-4_7.18": EN_1992_4__7_18()
         , "EN_1992-4_7.19": EN_1992_4__7_19()
         , "EN_1992-4_7.20": EN_1992_4__7_20()
         , "EN_1992-4_7.21": EN_1992_4__7_21()
         , "EN_1992-4_7.22": EN_1992_4__7_22()
         , "EN_1992-4_7.23": EN_1992_4__7_23()
         , "EN_1992-4_7.24": EN_1992_4__7_24()
         , "EN_1992-4_7.25": EN_1992_4__7_25()
         , "EN_1992-4_7.26": EN_1992_4__7_26()
         , "EN_1992-4_7.27": EN_1992_4__7_27()
         , "EN_1992-4_7.28": EN_1992_4__7_28()
         , "EN_1992-4_7.29": EN_1992_4__7_29()
         , "EN_1992-4_7.30": EN_1992_4__7_30()
         , "EN_1992-4_7.31": EN_1992_4__7_31()
         , "EN_1992-4_7.33": EN_1992_4__7_33()
         , "EN_1992-4_7.34": EN_1992_4__7_34()
         , "EN_1992-4_7.35": EN_1992_4__7_35()
         , "EN_1992-4_7.36": EN_1992_4__7_36()
         , "EN_1992-4_7.37": EN_1992_4__7_37()
         , "EN_1992-4_7.38": EN_1992_4__7_38()
         , "EN_1992-4_7.39a": EN_1992_4__7_39a()
         , "EN_1992-4_7.39b": EN_1992_4__7_39b()
         , "EN_1992-4_7.39c": EN_1992_4__7_39c()
         , "EN_1992-4_7.39d": EN_1992_4__7_39d()
         , "EN_1992-4_7.40": EN_1992_4__7_40()
         , "EN_1992-4_7.41": EN_1992_4__7_41()
         , "EN_1992-4_7.42": EN_1992_4__7_42()
         , "EN_1992-4_7.43": EN_1992_4__7_43()
         , "EN_1992-4_7.44": EN_1992_4__7_44()
         , "EN_1992-4_7.45": EN_1992_4__7_45()
         , "EN_1992-4_7.46": EN_1992_4__7_46()
         , "EN_1992-4_7.47": EN_1992_4__7_47()
         , "EN_1992-4_7.48": EN_1992_4__7_48()
         , "EN_1992-4_7.49": EN_1992_4__7_49()
         , "EN_1992-4_7.50": EN_1992_4__7_50()
         , "EN_1992-4_7.51": EN_1992_4__7_51()
         , "EN_1992-4_7.53": EN_1992_4__7_53()
         , "EN_1992-4_7.57": EN_1992_4__7_57()
         , "EN_1992-4_7.58": EN_1992_4__7_58()
         , "EN_1992-4_7.59": EN_1992_4__7_59()
         , "EN_1992-4_7.60": EN_1992_4__7_60()
         , "EN_1992-4_7.62": EN_1992_4__7_62()
         , "EN_1992-4_7.63": EN_1992_4__7_63()
         , "EN_1992-4_7.63a": EN_1992_4__7_63a()
         , "EN_1992-4_7.64": EN_1992_4__7_64()
         , "EN_1992-4_7.65": EN_1992_4__7_65()
         , "EN_1992-4_7.66": EN_1992_4__7_66()
         , "EN_1992-4_7.67": EN_1992_4__7_67()
         , "EN_1992-4_7.68": EN_1992_4__7_68()
         , "EN_1992-4_7.69": EN_1992_4__7_69()
         , "EN_1992-4_7.70": EN_1992_4__7_70()
         , "EN_1992-4_7.71": EN_1992_4__7_71()
         , "EN_1992-4_7.72": EN_1992_4__7_72()
         , "EN_1992-4_7.73": EN_1992_4__7_73()
         , "EN_1992-4_7.74": EN_1992_4__7_74()
         , "EN_1992-4_7.75": EN_1992_4__7_75()
         , "EN_1992-4_7.76a": EN_1992_4__7_76a()
         , "EN_1992-4_7.76b": EN_1992_4__7_76b()
         , "EN_1992-4_7.77": EN_1992_4__7_77()
         , "EN_1992-4_7.78": EN_1992_4__7_78()
         , "EN_1992-4_7.80": EN_1992_4__7_80()
         , "EN_1992-4_7.81": EN_1992_4__7_81()
         , "EN_1992-4_7.82": EN_1992_4__7_82()
         , "EN_1992-4_7.83": EN_1992_4__7_83()
         , "EN_1992-4_7.84": EN_1992_4__7_84()
         , "EN_1992-4_7.86": EN_1992_4__7_86()
         , "EN_1992-4_7.87": EN_1992_4__7_87()
         , "EN_1992-4_7.88": EN_1992_4__7_88()
         , "EN_1992-4_7.89b": EN_1992_4__7_89b()
         , "EN_1992-4_7.90": EN_1992_4__7_90()
         , "EN_1992-4_7.91": EN_1992_4__7_91()
         , "EN_1992-4_7.92": EN_1992_4__7_92()
         , "EN_1992-4_8.1": EN_1992_4__8_1()
         , "EN_1992-4_8.2": EN_1992_4__8_2()
         , "EN_1992-4_8.3": EN_1992_4__8_3()
         , "EN_1992-4_A.1": EN_1992_4__A_1()
         , "EN_1992-4_A.2": EN_1992_4__A_2()
         , "EN_1992-4_A.3": EN_1992_4__A_3()
         , "EN_1992-4_A.4": EN_1992_4__A_4()
         , "EN_1992-4_A.5": EN_1992_4__A_5()
         , "EN_1992-4_C.1": EN_1992_4__C_1()
         , "EN_1992-4_C.2": EN_1992_4__C_2()
         , "EN_1992-4_C.3": EN_1992_4__C_3()
         , "EN_1992-4_C.4": EN_1992_4__C_4()
         , "EN_1992-4_C.5": EN_1992_4__C_5()
         , "EN_1992-4_C.6": EN_1992_4__C_6()
         , "EN_1992-4_C.7": EN_1992_4__C_7()
         , "EN_1992-4_C.8": EN_1992_4__C_8()
         , "EN_1992-4_C.9": EN_1992_4__C_9()
         , "EN_1992-4_C.10": EN_1992_4__C_10()
         , "EN_1992-4_C.11a": EN_1992_4__C_11a()
         , "EN_1992-4_C.11b": EN_1992_4__C_11b()
         , "EN_1992-4_D.1": EN_1992_4__D_1()
         , "EN_1992-4_D.2": EN_1992_4__D_2()
         , "EN_1992-4_D.3": EN_1992_4__D_3()
         , "EN_1992-4_D.4": EN_1992_4__D_4()
         , "EN_1992-4_D.5": EN_1992_4__D_5()
         , "EN_1992-4_D.6": EN_1992_4__D_6()
         , "EN_1992-4_D.7": EN_1992_4__D_7()
         , "EN_1992-4_D.8.9": EN_1992_4__D_8_9()
         , "EN_1992-4_D.10": EN_1992_4__D_10()
         , "EN_1992-4_D.11": EN_1992_4__D_11()
         , "EN_1992-4_G.1": EN_1992_4__G_1()
         , "EN_1992-4_G.2": EN_1992_4__G_2()
         , "EN_1992-4_G.3": EN_1992_4__G_3()
         }

# Windows
main_window = MainWindow()
main_window.show() # displays the main window
license_window = License_window()
tutorial_window = Tutorial_window()
faq_window = Faq_window()
notebook_window = Notebook_window()

# Starts the event loop
app.exec_()
