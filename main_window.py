#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 27 20:25:48 2021

@author: linus
"""

import sys
import json

#sys.path.insert(1, './calculators')
#from calculators import *

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
        #self.setMinimumSize(QSize(1340, 600))
        
        self.setStyleSheet("background-color: lightgray;")

        # Create the applications menu
        self.app_menu = self.menuBar()
        license_menu = self.app_menu.addMenu("&License")
        language_menu = self.app_menu.addMenu("&Language / Sprache")
        help_menu = self.app_menu.addMenu("&Help")
        
        # Adding file menu buttons
        register_action = QAction("&Register", self)
        register_action.triggered.connect(self.open_register)
        
        license_action = QAction("&License", self)
        license_action.triggered.connect(self.open_license)
        
        license_menu.addAction(register_action)
        license_menu.addSeparator()
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
        
        startup_action = QAction("&Startup window", self)
        startup_action.triggered.connect(self.open_startup)

        help_menu.addAction(faq_action)
        help_menu.addSeparator()
        help_menu.addAction(tutorial_action)
        help_menu.addSeparator()
        help_menu.addAction(startup_action)

        # Creating the left side menu QTreeWidget
        self.menu = QTreeWidget()
        self.menu.setHeaderLabels(("EC equations",))
        
        # Populating the left side menu widget
        with open("xml_trees/menu_tree.xml", "r") as file:
            xmlFileString = file.read()
        self.populate_tree_widget(self.menu, xmlFileString)
        
        # Setting size and connecting to itemClick signal of menu widget
        #self.menu.setMinimumWidth(185)
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
        
        # Three main layouts
        menu_layout.addWidget(self.menu)
        #menu_layout.addWidget(self.bookmark_btn)
        
        equation_layout.addWidget(self.equation)
        equation_layout.addLayout(button_layout1)
        equation_layout.addLayout(button_layout2)
        
        button_layout1.addWidget(self.open_calculator_btn)
        button_layout1.addWidget(self.open_tab_btn)
        button_layout2.addWidget(self.bookmark_btn)
        button_layout2.addWidget(self.delete_btn)

        # Creating tab widget
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
        
    def closeEvent(self, s):
        register_window.timer.stop()
        
    def open_faq(self):
        faq_window.show()
        
    def open_startup(self):
        startup_window.show()

    def open_register(self):
        register_window.show()

    def open_tutorial(self):
        tutorial_window.show()
        
    def open_license(self):
        license_window.show()
        
    # Displays the english equation
    def set_english(self):
        global language
        language = "English"
        
        faq_window.setWindowTitle("Frequently asked questions")
        tutorial_window.setWindowTitle("Tutorial")
        register_window.setWindowTitle("Register")
        
        register_window.label.setText("Input the registration code:")
        register_window.button.setText("Register")
        #register_window.status_label.setText("Status:") #TODO
        
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
        register_window.setWindowTitle("Registrierung")
        
        register_window.label.setText("Geben Sie den Registrierungscode ein:")
        register_window.button.setText("Registrieren")
        #register_window.status_label.setText("Status:") #TODO
        
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

    def update_current_tab(self, tab_index):
        global selected_tab
        selected_tab = opened_tabs[tab_index]
        
        if language == "English":
            self.bookmark_widgets[selected_tab].setHeaderLabels(("Bookmarked equations",))
        elif language == "German":
            self.bookmark_widgets[selected_tab].setHeaderLabels(("Lesezeichen Gleichungen",))
        
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
        #self.setMinimumSize(QSize(560, 160))
        
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
        
        # Writing to text boxes
        file_name = "data/saved_parameters"
        file_exists = exists(file_name)
        
        if file_exists:
            database_file = open('data/saved_parameters', 'r')
            database_json = database_file.read()
            database = json.loads(database_json)
            database_file.close()
            
            if self.calculator_name in database.keys():
                parameters = database[self.calculator_name]
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
            else:
                self.input0_1.setText("0")
                self.input0_2.setText("0")
                self.input0_3.setText("0")
                self.input1_1.setText("0")
                self.input1_2.setText("0")
                self.input1_3.setText("0")
                self.input2_1.setText("0")
                self.input2_2.setText("0")
                self.input2_3.setText("0")
                self.input3_1.setText("0")
                self.input3_2.setText("0")
                self.input3_3.setText("0")
        else:
            self.input0_1.setText("0")
            self.input0_2.setText("0")
            self.input0_3.setText("0")
            self.input1_1.setText("0")
            self.input1_2.setText("0")
            self.input1_3.setText("0")
            self.input2_1.setText("0")
            self.input2_2.setText("0")
            self.input2_3.setText("0")
            self.input3_1.setText("0")
            self.input3_2.setText("0")
            self.input3_3.setText("0")
                            
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
        
        # Defininf NA label
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
        
        #TODO: rename parameters and add new
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

        for count in range(self.parameter_count):
            try:
                xs[count] = float(inputs[count].text())
            except:
                error1 = True
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
                    
                    file_name = "data/saved_parameters"
                    file_exists = exists(file_name)
                    
                    if file_exists:
                        database_file = open("data/saved_parameters", "r")
                        database_json = database_file.read()
                        database_file.close()
                        
                        database = json.loads(database_json)
                        database[self.calculator_name] = str(xs[0]) + "|" + str(xs[1]) + "|" + \
                                                         str(xs[2]) + "|" + str(xs[3]) + "|" + \
                                                         str(xs[4]) + "|" + str(xs[5]) + "|" + \
                                                         str(xs[6]) + "|" + str(xs[7]) + "|" + \
                                                         str(xs[8]) + "|" + str(xs[9]) + "|" + \
                                                         str(xs[10]) + "|" + str(xs[11])
                        database_json = json.dumps(database)
                    else:
                        database = dict()
                        database[self.calculator_name] = str(xs[0]) + "|" + str(xs[1]) + "|" + \
                                                         str(xs[2]) + "|" + str(xs[3]) + "|" + \
                                                         str(xs[4]) + "|" + str(xs[5]) + "|" + \
                                                         str(xs[6]) + "|" + str(xs[7]) + "|" + \
                                                         str(xs[8]) + "|" + str(xs[9]) + "|" + \
                                                         str(xs[10]) + "|" + str(xs[11])
                        database_json = json.dumps(database)
                    
                    database_file = open("data/saved_parameters", "w")
                    database_file.write(database_json)
                    database_file.close()
                except:
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
        
# Creates the application handler. Has to be first line in code out of classes.
app = QApplication(sys.argv)

# Global variables
menu_window_text = "empty"
current_tab = 1
selected_tab = 1
opened_tabs = []
language = "English"
tree_widget_selection = "menu"
current_calc = ""
calculator_name = ""
registered_global = False

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

class Startup_window(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.setWindowTitle("Trial version")
        self.hide_window = False
        
        text = """The trial version of the program terminates after 15 minutes.\n
Die Testversion des Programms wird nach 15 Minuten beendet."""
        label = QLabel(text)
        
        check_box = QCheckBox("Hide this window upon startup.\nDieses Fenster beim Start ausblenden.")
        check_box.stateChanged.connect(self.check_function)
        #check_box.setCheckState(Qt.Checked)
        
        pagelayout = QVBoxLayout()
        pagelayout.addWidget(label)
        pagelayout.addWidget(check_box)
        
        container = QWidget()
        container.setLayout(pagelayout)

        # Set the central widget of the Window.
        self.setCentralWidget(container)
        
    def check_function(self, s):
        if s == Qt.Checked:
            self.hide_window = True
        else:
            self.hide_window = False
            
    def closeEvent(self, s):
        with open("data/hide_startup", "w") as file:
            file.write(str(self.hide_window))

startup_window = Startup_window()

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

class EN_1992_1_1__3_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 * 0.9
        result_name = "f_ct = "
        result_units = "kN/cm2"
        x1_name = "f_ct,sp [kN/cm2]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__3_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1**x2)*x3
        result_name = "f_ctm(t) = "
        result_units = "kN/cm2"
        x1_name = "β_cc(t)"
        x2_name = "α"
        x3_name = "f_ctm [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 / x2)**0.3 * x3
        result_name = "E_cm(t) = "
        result_units = "kN/cm2"
        x1_name = "f_cm(t)"
        x2_name = "f_cm"
        x3_name = "E_cm [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2) / x3
        result_name = "ε_cc(∞,t0) = "
        result_units = ""
        x1_name = "φ(∞,t0)"
        x2_name = "σ_c"
        x3_name = "E_c"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*exp(1.5*(x2 - 0.45))
        result_name = "φ_nl(∞,t0) = "
        result_units = ""
        x1_name = "φ(∞,t0)"
        x2_name = "k_σ"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "ε_cs = "
        result_units = ""
        x1_name = "ε_cd"
        x2_name = "ε_ca"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "ε_cd(t) = "
        result_units = ""
        x1_name = "β_ds(t,ts)"
        x2_name = "k_h"
        x3_name = "ε_cd,0"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1-x2)/((x1-x2) + 0.04*sqrt((2*x3/x4)**3))
        result_name = "β_ds(t,t_s) = "
        result_units = ""
        x1_name = "t [days]"
        x2_name = "t_s [days]"            
        x3_name = "A_c [mm2]"
        x4_name = "u [mm]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 * x2
        result_name = "ε_ca(t) = "
        result_units = ""
        x1_name = "β_as(t)"
        x2_name = "ε_ca(∞)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__3_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2.5*(x1 - 10)*10**(-6)
        result_name = "ε_ca(∞) = "
        result_units = ""
        x1_name = "f_ck"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)       

class EN_1992_1_1__3_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 - exp(-0.2*t**0.5)
        result_name = "β_as(t) = "
        result_units = ""
        x1_name = "t [days]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget) 

class EN_1992_1_1__3_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2 - x2**2)/(1 + (x1-2)*x2)
        result_name = "σ_c/f_cm = "
        result_units = ""
        x1_name = "k"
        x2_name = "η"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_15(QMainWindow):
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
        na_text += "α_cc = 1 (Austria)\n"
        na_text += "α_cc = 1 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_16(QMainWindow):
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
        na_text = "α_ct = 0.85 (Germany)\n"
        na_text += "α_ct = 1 (Austria)\n"
        na_text += "α_ct = 1 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1-(1-x2/x3)**x4)
        result_name = "σ_c = "
        result_units = "kN/cm2"
        x1_name = "f_cd [kN/cm2]"
        x2_name = "ε_c"            
        x3_name = "ε_c2"
        x4_name = "n"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.8 - (x1 - 50)/400
        result_name = "λ = "
        result_units = ""
        x1_name = "f_ck [MPa]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget) 
        
class EN_1992_1_1__3_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 - (x1 - 50)/200
        result_name = "η = "
        result_units = ""
        x1_name = "f_ck [MPa]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_23(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max((1.6 - x1/1000)*x2, x2)
        result_name = "f_ctm,cl = "
        result_units = "kN/cm2"
        x1_name = "h [mm]"
        x2_name = "f_ctm [kN/cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_24(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + 5*x2/x1)
        result_name = "f_ck,c = "
        result_units = "kN/cm2"
        x1_name = "f_ck [kN/cm2]"
        x2_name = "σ_2"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_25(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1.125 + 2.5*x2/x1)
        result_name = "f_ck,c = "
        result_units = "kN/cm2"
        x1_name = "f_ck"
        x2_name = "σ_2"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_26(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2/x3)**2
        result_name = "ε_c2,c = "
        result_units = ""
        x1_name = "ε_c2"
        x2_name = "f_ck,c"
        x3_name = "f_ck"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_27(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 0.2*(x2/x3)
        result_name = "ε_cu2,c = "
        result_units = ""
        x1_name = "ε_cu2"
        x2_name = "σ_2"
        x3_name = "f_ck"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_28(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        5.39*x1*exp(6.7*x2)*(x3/1000)**(0.75*(1-x2))*10**(-5)
        result_name = "Δσ_pr/σ_pi = "
        result_units = ""
        x1_name = "ρ_1000"
        x2_name = "μ"
        x3_name = "t"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_29(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.66*x1*exp(9.1*x2)*(x3/1000)**(0.75*(1-x2))*10**(-5)
        result_name = "Δσ_pr/σ_pi = "
        result_units = ""
        x1_name = "ρ_1000"
        x2_name = "μ"
        x3_name = "t"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__3_30(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.98*x1*exp(8*x2)*(x3/1000)**(0.75*(1-x2))*10**(-5)
        result_name = "Δσ_pr/σ_pi = "
        result_units = ""
        x1_name = "ρ_1000"
        x2_name = "μ"
        x3_name = "t"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__4_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "c_nom = "
        result_units = ""
        x1_name = "c_min"
        x2_name = "Δc_dev"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__4_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(x1, x2 + x3 - x4 - x5, 10)
        result_name = "c_min = "
        result_units = "mm"
        x1_name = "c_min,b"
        x2_name = "c_min,dur"            
        x3_name = "Δc_dur,γ"
        x4_name = "Δc_dur,st"
        x5_name = "Δc_dur,add"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(2/sqrt(x2))*sqrt(0.5*(1 + 1/x3))
        result_name = "θ_i = "
        result_units = ""
        x1_name = "θ_0"
        x2_name = "l [m]"
        x3_name = "m"
        na_text = "θ_0 = 1/200 or 0.008/(2m)^0.5 or 0.008/(m)^0.5 (Germany)\n"
        na_text += "θ_0 = 1/200 (Austria)\n"
        na_text += "θ_0 = 1/200 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/2
        result_name = "e_i = "
        result_units = ""
        x1_name = "θ_i"
        x2_name = "l_0"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__5_3a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "H_i = "
        result_units = "kN"
        x1_name = "θ_i"
        x2_name = "N [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__5_3b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1*x2
        result_name = "H_i = "
        result_units = "kN"
        x1_name = "θ_i"
        x2_name = "N [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "H_i = "
        result_units = "kN"
        x1_name = "θ_i"
        x2_name = "N_b [kN]"
        x3_name = "N_a [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 + x3)/2
        result_name = "H_i = "
        result_units = "kN"
        x1_name = "θ_i"
        x2_name = "N_b [kN]"
        x3_name = "N_a [kN]"
        na_text = "H_i = θ_i*(N_b + N_a) (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "H_i = "
        result_units = "kN"
        x1_name = "θ_i"
        x2_name = "N_a [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(x1 + x2, x3)
        result_name = "b_eff = "
        result_units = "cm or mm"
        x1_name = "Σb_eff,i"
        x2_name = "b_w"
        x3_name = "b"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_7a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(0.2*x1 + 0.1*x2, 0.2*x2, x1)
        result_name = "b_eff,i = "
        result_units = "cm or mm"
        x1_name = "b_i"
        x2_name = "l_0"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 + x3
        result_name = "l_eff = "
        result_units = "m or cm"
        x1_name = "l_n"
        x2_name = "a_1"
        x3_name = "a_2"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/8
        result_name = "ΔM_Ed = "
        result_units = "kNm"
        x1_name = "F_Ed,sup [kN]"
        x2_name = "t [m]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_10a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3/x4
        result_name = "δ = "
        result_units = ""
        x1_name = "k1"
        x2_name = "k2"            
        x3_name = "x_u"
        x4_name = "d"
        na_text = "k1 = 0.64, k2 = 0.8 (Germany)\n"
        na_text += "k1 = 0.44, k2 = 1.25*(0.6 + 0.0014/ε_cu2) (Austria)\n"
        na_text += "k1 = 0.44, k2 = 1.25*(0.6 + 0.0014/ε_cu2) (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_10b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3/x4
        result_name = "δ = "
        result_units = ""
        x1_name = "k3"
        x2_name = "k4"            
        x3_name = "x_u"
        x4_name = "d"
        na_text = "k3 = 0.72, k4 = 0.8 (Germany)\n"
        na_text += "k3 = 0.54, k4 = 1.25*(0.6 + 0.0014/ε_cu2) (Austria)\n"
        na_text += "k3 = 0.54, k4 = 1.25*(0.6 + 0.0014/ε_cu2) (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1/3)
        result_name = "k_λ = "
        result_units = ""
        x1_name = "λ"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "λ = "
        result_units = ""
        x1_name = "M_Sd"
        x2_name = "V_Sd"
        x3_name = "d"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        20*(1/(1 + 0.2*x1))*sqrt(1 + 2*x2*x3/(x4*x5))*(1.7 - x7/x8)/sqrt(x6/(x4*x5))
        result_name = "λ_lim = "
        result_units = ""
        x1_name = "φ_ef"
        x2_name = "A_s"            
        x3_name = "f_yd"
        x4_name = "A_c"
        x5_name = "f_cd"
        x6_name = "N_Ed"
        x7_name = "M_01"            
        x8_name = "M_02"
        na_text = "λ_lim = 25 (n>=0.41) or 16/(n)^0.5 (n<0.41), n = N_Ed/(A_c*f_cd) (Germany)"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name, x5_name, \
                                  x6_name, x7_name, x8_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "λ = "
        result_units = ""
        x1_name = "l_0"
        x2_name = "i"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*sqrt((1 + x2/(0.45 + x2))*(1 + x3/(0.45 + x3)))
        result_name = "l_0 = "
        result_units = ""
        x1_name = "l"
        x2_name = "k1"
        x3_name = "k2"
        na_text = "For k_1 and k_2 see DAfStb-Heft 600 (Germany)\n"
        na_text += "k1 = 0.2, k_2 = 0.2 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*max(sqrt(1 + 10*x2*x3/(x2 + x3)), (1 + x2/(1 + x2))*(1 + x3/(1 + x3)))
        result_name = "l_0 = "
        result_units = ""
        x1_name = "l"
        x2_name = "k1"
        x3_name = "k2"
        na_text = "For k_1 and k_2 see DAfStb-Heft 600 (Germany)\n"
        na_text += "k1 = 0.2, k_2 = 0.2 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__5_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        pi*sqrt(x1*x2/x3)
        result_name = "l_0 = "
        result_units = "cm"
        x1_name = "E [kN/cm2]"
        x2_name = "I [cm4]"
        x3_name = "Nb [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x2 + 1.6)*x3/x4**2
        result_name = "F_V,Ed = "
        result_units = "kN"
        x1_name = "k_1"
        x2_name = "n_s"
        x3_name = "ΣE_cd*I_c"
        x4_name = "L"
        na_text = "k_1 = 0.31, E_cd = E_cm/1.2 (Germany)\n"
        na_text += "k_1 = 0.31 (Austria)\n"
        na_text += "k_1 = 0.31 (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "φ_ef = "
        result_units = ""
        x1_name = "φ(∞,t0)"
        x2_name = "M_0Eqp"
        x3_name = "M_0Ed"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "E_cd = "
        result_units = "kN/cm2"
        x1_name = "E_cm [kN/cm2]"
        x2_name = "γ_CE"
        na_text = "γ_CE = 1.5 or 1.2 (Germany)\n"
        na_text += "γ_CE = 1.2 (Austria)\n"
        na_text += "γ_CE = 1.2 (Slovenia)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3 + x4*x5*x6
        result_name = "EI = "
        result_units = "kNcm2"
        x1_name = "K_c"
        x2_name = "E_cd"            
        x3_name = "I_c"
        x4_name = "K_s"
        x5_name = "E_s"
        x6_name = "I_s"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_22(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1/20)*x2/(x3*x4)*x5/(170*(1 + x6))
        result_name = "K_c = "
        result_units = ""
        x1_name = "f_ck [MPa]"
        x2_name = "N_Ed"
        x3_name = "A_c"
        x4_name = "f_cd"
        x5_name = "λ"
        x6_name = "φ_ef"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_26(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.3/(1 + 0.5*x1)
        result_name = "K_c = "
        result_units = ""
        x1_name = "φ_ef"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_27(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(1 + x2)
        result_name = "E_cd,eff = "
        result_units = "kN/cm2"
        x1_name = "E_cd [kN/cm2]"
        x2_name = "φ_ef"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_28(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + x2/(x3/x4 -1))
        result_name = "M_Ed = "
        result_units = "kNm"
        x1_name = "M_0Ed"
        x2_name = "β"
        x3_name = "N_B"
        x4_name = "N_Ed"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_29(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        pi**2/x1
        result_name = "β = "
        result_units = ""
        x1_name = "c_0"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_30(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(1 - x2/x3)
        result_name = "M_Ed = "
        result_units = "kNm"
        x1_name = "M_0Ed"
        x2_name = "N_Ed"
        x3_name = "N_B"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_31(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "M_Ed = "
        result_units = "kNm"
        x1_name = "M_Ed0"
        x2_name = "M_2"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_32(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.6*x1 + 0.4*x2, 0.4*x1)
        result_name = "M_0e = "
        result_units = "kNm"
        x1_name = "M_02"
        x2_name = "M_01"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_33(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3**2/x4
        result_name = "M_2 = "
        result_units = "kNm"
        x1_name = "N_Ed [kN]"
        x2_name = "r [m]"
        x3_name = "l_0 [m]"
        x4_name = "c"
        na_text = "e_2 = e_2*(λ/10 - 2.5) (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_34(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3/(0.45*x4*x5)
        result_name = "1/r = "
        result_units = "1/cm or 1/m"
        x1_name = "K_r"
        x2_name = "K_φ"
        x3_name = "f_yd"
        x4_name = "E_s"
        x5_name = "d [cm or m]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_35(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/2 + x2
        result_name = "d = "
        result_units = "cm or m"
        x1_name = "h"
        x2_name = "i_s"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_36(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 + x1*x2/(x3*x4) - x5/(x3*x4))*(1 + x1*x2/(x3*x4) - x6)
        result_name = "K_r = "
        result_units = ""
        x1_name = "A_s"
        x2_name = "f_yd"            
        x3_name = "A_c"
        x4_name = "f_cd"
        x5_name = "N_Ed"
        x6_name = "n_bal"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_37(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + (0.35 + x1/200 - x2/150)*x3
        result_name = "K_φ = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "λ"
        x3_name = "φ_ef"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_38b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x2/x3*x4*sqrt(12)/(x1/x3*x5*sqrt(12))
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x3*x5*sqrt(12)/(x2/x3*x4*sqrt(12))
                                  
        result_name = "right eq.: "
        result_units = ""
        x1_name = "M_Edy"
        x2_name = "M_Edz"
        x3_name = "N_Ed"
        x4_name = "i_y"
        x5_name = "i_z"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 non_equation = True, \
                                 result_name2 = "left eq.: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_39(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**x5 + (x3/x4)**x5
        result_name = "result: "
        result_units = ""
        x1_name = "M_Edz"
        x2_name = "M_Rdz"
        x3_name = "M_Edy"
        x4_name = "M_Rdy"
        x5_name = "a"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_40a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1/x2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        50/(x3/x2)**(1/3)
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "l_0t"
        x2_name = "b"
        x3_name = "h"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_40b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1/x2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        70/(x3/x2)**(1/3)
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "l_0t"
        x2_name = "b"
        x3_name = "h"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_41(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*min(x2*x3, x4*x5)
        result_name = "P_max = "
        result_units = "kN"
        x1_name = "A_p [cm2]"
        x2_name = "k_1"
        x3_name = "f_pk [kN/cm2]"
        x4_name = "k_2"
        x5_name = "f_p0,1k [kN/cm2]"
        na_text = "k_1 = 0.8, k_2 = 0.9 (Germany)\n"
        na_text += "k_1 = 0.8, k_2 = 0.9 (Austria)\n"
        na_text += "k_1 = 0.8, k_2 = 0.9 (Slovenia)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_42(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*x1
        result_name = "σ_c = "
        result_units = ""
        x1_name = "f_ck(t)"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_43(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*min(x2*x3, x4*x5)
        result_name = "P_m0(x) = "
        result_units = "kN"
        x1_name = "A_p [cm2]"
        x2_name = "k_7"
        x3_name = "f_pk [kN/cm2]"
        x4_name = "k_8"
        x5_name = "f_p0,1k [kN/cm2]"
        na_text = "k_7 = 0.75, k_8 = 0.85 (Germany)\n"
        na_text += "k_7 = 0.7, k_8 = 0.8 (Austria)\n"
        na_text += "k_7 = 0.75, k_8 = 0.85 (Slovenia)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_44(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "ΔP_el = "
        result_units = ""
        x1_name = "A_p"
        x2_name = "E_p"
        x3_name = "Σj*Δσ_c/E_cm"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_45(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - exp(-x2*(x3 + x4*x5)))
        result_name = "ΔP_μ(x) = "
        result_units = "kN"
        x1_name = "P_max"
        x2_name = "μ"
        x3_name = "θ"
        x4_name = "k"
        x5_name = "x"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__5_46(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                               x7*(x1*x2 + 0.8*x4 + x2/x3*x5*x6) / \
                        (1 + x2/x3*x7/x8*(1 + x8/x9*x10**2)*(1 + 0.8*x5))
        result_name = "ΔP_c+s+r = "
        result_units = "kN"
        x1_name = "ε_cs"
        x2_name = "E_p"
        x3_name = "E_cm"
        x4_name = "Δσ_pr"
        x5_name = "φ(t,t0)"
        x6_name = "σ_c,QP"
        x7_name = "A_p"
        x8_name = "A_c"
        x9_name = "I_c"
        x10_name = "z_cp"
        self.widget = Calculator(10, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, x10_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__5_47(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "P_k,sup/P_k,inf = "
        result_units = "kN"
        x1_name = "r_sup/r_inf"
        x2_name = "P_m,t(x)"
        na_text = "r_sup = r_inf = 1 is not allowed. Else take recommended values (Germany)\n"
        na_text += "Für Spannglieder im nachträglichen Verbund: r_sup=1.05 r_inf=0.95 (Austria)\n"
        na_text += "For pre-tensioning or unbonded tendons: r_sup = 1.05, r_inf = 0.95 (Slovenia)\n"
        na_text += "For post-tensioning with bonded tendons: r_sup = 1.1, r_inf = 0.90 (Slovenia)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "V_Rd = "
        result_units = "kN"
        x1_name = "V_Rd,s"
        x2_name = "V_ccd"
        x3_name = "V_td"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_2a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*min(1 + sqrt(200/x2), 2)*(100*min(x3/(x4*x2), \
                        0.02)*x5)**(1/3) + x6*min(x7/x8, 0.2*x9))*x4*x2
                
        result_name = "V_Rd,c = "
        result_units = "N"
        x1_name = "C_Rd,c"
        x2_name = "d [mm]"
        x3_name = "A_sl [mm2]"
        x4_name = "b_w [mm]"
        x5_name = "f_ck [MPa]"
        x6_name = "k_1"
        x7_name = "N_Ed [N]"
        x8_name = "A_c [mm2]"
        x9_name = "f_cd [MPa]"
        na_text = "k_1 = 0.12, C_Rdc = 0.15/γ_C (Germany)\n"
        na_text += "k_1 = 0.15, C_Rdc = 0.18/γ_C (Austria)\n"
        na_text += "k_1 = 0.15, C_Rdc = 0.18/γ_C (Slovenia)"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_2b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*min(x3/x4, 0.2*x7))*x5*x6
                
        result_name = "V_Rd,c = "
        result_units = "N"
        x1_name = "v_min"
        x2_name = "k_1"
        x3_name = "N_Ed"
        x4_name = "A_c"
        x5_name = "b_w"
        x6_name = "d"
        x7_name = "f_cd"
        na_text = "k_1 = 0.12, v_min = (0.0525/γ_C)*k^(3/2)*f_ck^(0.5) for d <= 600mm \nv_min = (0.0375/γ_C)*k^(3/2)*f_ck^(0.5) for d > 600mm (Germany)\n"
        na_text += "k_1 = 0.15, for v_min see equation 6_3 (Austria) and (Slovenia)\n"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.035*x1**1.5*x2**0.5
        result_name = "v_min = "
        result_units = ""
        x1_name = "k"
        x2_name = "f_ck"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*sqrt(x4**2 + x5/x6*x7/x8*x9)
                
        result_name = "V_Rd,c = "
        result_units = "kN"
        x1_name = "I"
        x2_name = "b_w"
        x3_name = "S"
        x4_name = "f_ctd"
        x5_name = "l_x"
        x6_name = "l_pt2"
        x7_name = "N_Ed"
        x8_name = "A_c"
        x9_name = "f_ctd"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*x2*x3*x4
        result_name = "V_Ed = "
        result_units = "kN"
        x1_name = "b_w"
        x2_name = "d"
        x3_name = "ν"
        x4_name = "f_cd [kN/cm2]"
        na_text = "ν = 0.675, for >= C55/67 multiply with (1.1 - f_ck/500) (Germany)\n"
        na_text += "Za ν glej enačbo 6_6 (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*(1 - x1/250)
        result_name = "ν = "
        result_units = ""
        x1_name = "f_ck [MPa]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_7a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         1/tan(x1)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1.2 + 1.4*x2/x3)/(1 - x4/x5)
                                  
        result_name = "right eq.: "
        result_units = ""
        x1_name = "θ [rad]"
        x2_name = "σ_cp"
        x3_name = "f_cd"
        x4_name = "V_Rd,cc"
        x5_name = "V_Ed"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 non_equation = True, \
                                 result_name2 = "left eq.: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_7b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.48*x2**(1/3)*(1 - 1.2*x3/x4)*x5*x6
        result_name = "V_Rd,cc = "
        result_units = ""
        x1_name = "c"
        x2_name = "f_ck"
        x3_name = "σ_cp"            
        x4_name = "f_cd"
        x5_name = "b_w"
        x6_name = "z"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3*x4/tan(x5)
        result_name = "V_Rd,s = "
        result_units = "kN"
        x1_name = "A_sw"
        x2_name = "s"            
        x3_name = "z"
        x4_name = "f_ywd [kN/cm2]"
        x5_name = "θ [rad]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*(tan(x6) + 1/tan(x6))
        result_name = "V_Rd,max = "
        result_units = "kN"
        x1_name = "α_cw"
        x2_name = "b_w"
        x3_name = "z"            
        x4_name = "f_cd [kN/cm2]"
        x5_name = "ν_1"
        x6_name = "θ [rad]"
        na_text = "α_cw = 1, ν_1 = 0.75 (<=C50/60), ν_1 = 0.75*(1 - f_ck/500) (>=C55/67)  (Germany)\n"
        na_text += "Gleichung 6.10 is nicht anzuwenden. (Austria)\n"
        na_text += "Za ν_1 glej enačbo 6_10a in 6_10b (Slovenia)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.9 - x1/200, 0.5)
        result_name = "ν_1 = "
        result_units = ""
        x1_name = "f_ck [MPa]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_11a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         1 + x1/x2
                                  
        result_name = "σ_cp < 0.25f_cd: "
        result_units = ""
        x1_name = "σ_cp"
        x2_name = "f_cd"
        na_text = "σ_cp = N_Ed/A_c (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_11c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2.5*(1 - x1/x2)
                                  
        result_name = "0.5f_cd < σ_cp: "
        result_units = ""
        x1_name = "σ_cp"
        x2_name = "f_cd"
        na_text = "σ_cp = N_Ed/A_c (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2/(x3*x4)
        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x5*x6*x7
                                  
        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "A_sw,max"
        x2_name = "f_ywd"
        x3_name = "b_w"
        x4_name = "s"
        x5_name = "α_cw"
        x6_name = "ν_1"
        x7_name = "f_cd"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3*x4*(1/tan(x5) + 1/tan(x6))*sin(x6)
        result_name = "V_Rd,s = "
        result_units = "kN"
        x1_name = "A_sw"
        x2_name = "s"
        x3_name = "z"            
        x4_name = "f_ywd [kN/cm2]"
        x5_name = "θ [rad]"
        x6_name = "α [rad]"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*(1/tan(x6) + 1/tan(x7))/(1 + 1/tan(x6)**2)
        result_name = "V_Rd,max = "
        result_units = "kN"
        x1_name = "α_cw"
        x2_name = "b_w"
        x3_name = "z"      
        x4_name = "ν_1"
        x5_name = "f_cd [kN/cm2]"
        x6_name = "θ [rad]"
        x7_name = "α [rad]"
        na_text = "α_cw = 1 (Germany)"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2/(x3*x4)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.5*x5*x6*x7/sin(x8)
                                  
        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "A_sw,max"
        x2_name = "f_ywd [kN/cm2]"
        x3_name = "b_w"
        x4_name = "s"
        x5_name = "α_cw"
        x6_name = "ν_1"
        x7_name = "f_cd"
        x8_name = "α [rad]"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - 0.5*x2
        result_name = "b_w,nom = "
        result_units = "cm"
        x1_name = "b_w [cm]"
        x2_name = "Σφ [cm]"
        na_text = "b_w,nom = b_w - Σφ (>=C55/67) (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - 1.2*x2
        result_name = "b_w,nom = "
        result_units = "cm"
        x1_name = "b_w [cm]"
        x2_name = "Σφ [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*(1/tan(x2) - 1/tan(x3))
        result_name = "ΔF_td = "
        result_units = "kN"
        x1_name = "V_Ed [kN]"
        x2_name = "θ [rad]"
        x3_name = "α [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*sin(x3)
        result_name = "V_Ed = "
        result_units = "kN"
        x1_name = "A_sw"
        x2_name = "f_ywd [kN/cm2]"
        x3_name = "α [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
                        
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "ΔF_d [kN]"
        x2_name = "h_f [cm]"
        x3_name = "Δx [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2/x3
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x4*x5*tan(x6)
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "A_sf [cm2]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "s_f [cm]"
        x4_name = "v_Ed [kN/cm2]"
        x5_name = "h_f [cm]"
        x6_name = "θ_f [rad]"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_22(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*sin(x3)*cos(x3)
                        
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "ν"
        x2_name = "f_cd [kN/cm2]"
        x3_name = "θ_f [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_24(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4)
                        
        result_name = "v_Edi = "
        result_units = "kN/cm2"
        x1_name = "β"
        x2_name = "V_Ed [kN]"
        x3_name = "z [cm]"
        x4_name = "b_i [cm]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_25(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2 + x3*x4 + x5*x6*(x3*sin(x7) + cos(x7))
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.5*x8*x9                            
                                  
        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "c"
        x2_name = "f_ctd [kN/cm2]"
        x3_name = "μ"
        x4_name = "σ_n [kN/cm2]"
        x5_name = "ρ"
        x6_name = "f_yd [kN/cm2]"
        x7_name = "α"
        x8_name = "ν"
        x9_name = "f_cd [kN/cm2]"
        na_text = "μ = 1.2*μ (Germany)\n"
        na_text += "ρ_min = 0.12*f_ctm/fyk >= 0.0005 (plattenartige Bauteile)\nρ_min = 0.20*f_ctm/fyk >=0.001 (balkenartige Bauteile) (Austria)"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__6_26(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(2*x2)
        result_name = "τ_t,i*t_ef,i = "
        result_units = "kN/cm"
        x1_name = "T_Ed [kNcm]"
        x2_name = "A_k [cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_27(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
                        
        result_name = "V_Ed,i = "
        result_units = "kN"
        x1_name = "τ_t,i [kN/cm2]"
        x2_name = "t_ef,i [cm]"
        x3_name = "z_i [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_28(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(2*x2*tan(x3))
                        
        result_name = "ΣA_sl*f_yd/u_k = "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "A_k"
        x3_name = "θ [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_29(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + x3/x4
                        
        result_name = "result: "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "T_Rd,max"
        x3_name = "V_Ed"
        x4_name = "V_Rd,max"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_30(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1*x2*x3*x4*x5*sin(x6)*cos(x6)
        result_name = "T_Rd,max = "
        result_units = ""
        x1_name = "ν"
        x2_name = "α_cw"            
        x3_name = "f_cd"
        x4_name = "A_k"
        x5_name = "t_ef,i"
        x6_name = "θ [rad]"
        na_text = "ν = 0.525, ν = ν*(1.1 - f_ck/500) (>=C55/67) (Germany)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_31(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + x3/x4
                        
        result_name = "result: "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "T_Rd,c"
        x3_name = "V_Ed"
        x4_name = "V_Rd,c"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_32(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2)/2
        result_name = "d_eff = "
        result_units = "cm"
        x1_name = "d_y [cm]"
        x2_name = "d_z [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_33(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + x2 + 0.5*x3
        result_name = "r_cont = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "l_h [cm]"
        x3_name = "c [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_34(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + 0.56*sqrt(x2*x3)
        result_name = "r_cont = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "l_1 [cm]"
        x3_name = "l_2 [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_35(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + 0.69*x2
        result_name = "r_cont = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "l_1 [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_36(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 2*x2 + 0.5*x3
        result_name = "r_cont,ext"
        result_units = "cm"
        x1_name = "l_h [cm]"
        x2_name = "d [cm]"
        x3_name = "c [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_37(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*(x1 + x2) + 0.5*x3
        result_name = "r_cont,int = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "h_H [cm]"
        x3_name = "c [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_38(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4)
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "β"
        x2_name = "V_Ed [kN]"
        x3_name = "u_i [cm]"
        x4_name = "d [cm]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_39(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + x1*x2/x3*x4/x5
        result_name = "β = "
        result_units = ""
        x1_name = "k"
        x2_name = "M_Ed"            
        x3_name = "V_Ed"
        x4_name = "u_1"
        x5_name = "W_1"
        na_text = "β >= 1.1 (Germany)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_41(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2/2 + x1*x2 + 4*x2*x3 + 16*x3**2 + 2*pi*x1*x3
        result_name = "W_1 = "
        result_units = ""
        x1_name = "c_1"
        x2_name = "c_2"
        x3_name = "d"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_42(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 0.6*pi*x1/(x2*(x3 + 4*x4))
        result_name = "β = "
        result_units = ""
        x1_name = "M_Ed [kNcm]"
        x2_name = "V_Ed [kN]"
        x3_name = "D [cm]"
        x4_name = "d [cm]"
        na_text = "β >= 1.1 (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_43(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 1.8*sqrt((x1/x2)**2 + (x3/x4)**2)
        result_name = "β = "
        result_units = ""
        x1_name = "e_y"
        x2_name = "b_z"
        x3_name = "e_z"
        x4_name = "b_y"
        na_text = "β >= 1.1 (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_43_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + sqrt((x1*x2/x3*x4/x5)**2 + (x6*x7/x3*x4/x8)**2)
        result_name = "β = "
        result_units = ""
        x1_name = "k_y"
        x2_name = "M_Ed,y"
        x3_name = "V_Ed"
        x4_name = "u_1"
        x5_name = "W_1,y"
        x6_name = "k_z"
        x7_name = "M_Ed,z"
        x8_name = "W_1,z"
        na_text = "Gleichung 6.43 gilt, wie angegeben, nur für rechteckige Innenstützen (Austria)"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, x8_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_44(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + x3*x1/x4*x5
        result_name = "β = "
        result_units = ""
        x1_name = "u_1"
        x2_name = "u_1*"
        x3_name = "k"
        x4_name = "W_1"
        x5_name = "e_par"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_45(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x2**2/4 + x1*x2 + 4*x1*x3 + 8*x3**2 + pi*x2*x3
        result_name = "W_1 = "
        result_units = ""
        x1_name = "c_1"
        x2_name = "c_2"
        x3_name = "d"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_46(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "β = "
        result_units = ""
        x1_name = "u_1"
        x2_name = "u_1*"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_47(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*(1 + sqrt(200/x2))*(100*min(sqrt(x3*x4), 0.02)*x5)**(1/3) + x6*(x7/x8 + x9/x10)/2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x11 + x6*(x7/x8 + x9/x10)/2
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_Rd,c"
        x2_name = "d [mm]"
        x3_name = "ρ_ly"
        x4_name = "ρ_lz"
        x5_name = "f_ck [MPa]"
        x6_name = "k_1"
        x7_name = "N_Ed,y [N]"
        x8_name = "A_cy [mm2]"
        x9_name = "N_Ed,z [N]"
        x10_name = "A_cz [mm2]"
        x11_name = "v_min [N/mm2]"
        na_text = "k_1 = 0.1, C_Rd,c = 0.18/γ_C or 0.18/γ_C*(0.1*u_0/d + 0.6) (Germany)\n"
        na_text += "k_1 = 0.15, C_Rdc = 0.18/γ_C, ρ_l = 0.4*f_cd/f_yd <= 0.02, v_min sehe 6.3 (Austria)\n"
        na_text += "k_1 = 0.15, C_Rdc = 0.18/γ_C, za v_min glej enačbo 6.3 (Slovenia)"
        self.widget = Calculator(11, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, x10_name, x11_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_48(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - x2
        result_name = "V_Ed,red = "
        result_units = "kN"
        x1_name = "V_Ed [kN]"
        x2_name = "ΔV_Ed [kN]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_49(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "V_Ed [kN]"
        x2_name = "u [cm]"
        x3_name = "d [cm]"
        na_text = "v_Ed = v_Ed*β (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_50(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*(100*x3*x4)**(1/3)*2*x5/x6
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x7*2*x5/x6
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_Rd,c"
        x2_name = "k"
        x3_name = "ρ_l"
        x4_name = "f_ck [MPa]"
        x5_name = "d [cm]"
        x6_name = "a [cm]"
        x7_name = "v_min [N/mm2]"
        na_text = "C_Rd,c = 0.15/γ_C (Germany)"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_51(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)*(1 + x4*x5*x2/(x1*x6))
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "V_Ed,red [kN]"
        x2_name = "u [cm]" 
        x3_name = "d [cm]"
        x4_name = "k"
        x5_name = "M_Ed"
        x6_name = "W"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_52(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1 + 1.5*x2/x3*x4*x5*sin(x8)/(x6*x7)
        result_name = "v_Rd,cs = "
        result_units = "kN/cm2"
        x1_name = "v_Rd,c [kN/cm2]"
        x2_name = "d [cm]"
        x3_name = "s_r [cm]"
        x4_name = "A_sw [cm2]"
        x5_name = "f_ywd,ef [kN/cm2]"
        x6_name = "u_1 [cm]"
        x7_name = "d [cm]"
        x8_name = "α [rad]"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name, x5_name, \
                                  x6_name, x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_53(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4)
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "β"
        x2_name = "V_Ed [kN]"
        x3_name = "u_0 [cm]"
        x4_name = "d [cm]"
        na_text = "v_Rd,max = 0.4*ν*f_cd, fűr ν sehe Gleichung 6.6 (Austria)\n"
        na_text += "v_Rd,max = 0.4*ν*f_cd, za ν glej enačbo 6.6 (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_54(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4)
        result_name = "u_out,ef = "
        result_units = "cm2"
        x1_name = "β"
        x2_name = "V_Ed [kN]"
        x3_name = "v_Rd,c [kN/cm2]"
        x4_name = "d [cm]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_56(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*x1*x2
        result_name = "σ_Rd,max = "
        result_units = "kN/cm2"
        x1_name = "ν'"
        x2_name = "f_cd [kN/cm2]"
        na_text = "ν' = 1.25 or 1.0 or 0.85 (Germany)\n"
        na_text += "Fűr ν' sehe Gleichung 6.57 (Austria)\n"
        na_text += "Za ν' glej enačbo 6.57 (Slovenia)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_57(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 - x1/250
        result_name = "ν = "
        result_units = ""
        x1_name = "f_ck"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_58(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*(x1 - x2)/x1*x3
        result_name = "T = "
        result_units = "kN"
        x1_name = "b [cm]"
        x2_name = "a [cm]"
        x3_name = "F [kN]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_59(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*(1 - 0.7*x1/x2)*x3
        result_name = "T = "
        result_units = "kN"
        x1_name = "a [cm]"
        x2_name = "h [cm]"
        x3_name = "F [kN]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_60(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "σ_Rd,max = "
        result_units = "kN/cm2"
        x1_name = "k_1"
        x2_name = "ν"
        x3_name = "f_Ecd [kN/cm2]"
        na_text = "k_1 = 1.1, ν' = ν'*(1.1 - f_ck/500) (>=C55/67) (Germany)\n"
        na_text += "k_1 = 1.25 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_61(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "σ_Rd,max = "
        result_units = "kN/cm2"
        x1_name = "k_2"
        x2_name = "ν"
        x3_name = "f_Ecd [kN/cm2]"
        na_text = "k_2 = 0.75, ν' = ν'*(1.1 - f_ck/500) (>=C55/67) (Germany)\n"
        na_text += "k_2 = 0.9 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_62(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "σ_Rd,max = "
        result_units = "kN/cm2"
        x1_name = "k_3"
        x2_name = "ν"
        x3_name = "f_Ecd [kN/cm2]"
        na_text = "k_3 = 0.75, ν' = ν'*(1.1 - f_ck/500) (>=C55/67) (Germany)\n"
        na_text += "k_3 = 0.9 (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_63(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x3*sqrt(x2/x1)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        3*x3*x1
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "A_c0 [cm2]"
        x2_name = "A_c1 [cm2]"
        x3_name = "f_cd [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side F_Rdu = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_64(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2)/(x1 + x2*sqrt(x5*x3/x4))
        result_name = "η = "
        result_units = ""
        x1_name = "A_s"
        x2_name = "A_p"
        x3_name = "Φ_s"
        x4_name = "Φ_p"
        x5_name = "ξ"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_65(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(tan(x1))
        result_name = "tan(θ_fat) = "
        result_units = ""
        x1_name = "θ [rad]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_67(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 + x3 + x4
        result_name = "result: "
        result_units = "kN or kNm"
        x1_name = "ΣG_j"
        x2_name = "P"
        x3_name = "ψ_1*Q_1"
        x4_name = "Σψ_i*Q_i"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_69(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 + x3 + x4 + x5
        result_name = "result: "
        result_units = "kN or kNm"
        x1_name = "ΣG_j"
        x2_name = "P"
        x3_name = "ψ_1*Q_1"
        x4_name = "Σψ_i*Q_i"
        x5_name = "Q_fat"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_71(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x3/x4
                                  
        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "γ_F,fat"
        x2_name = "Δσ_S,equ(N*)"
        x3_name = "Δσ_Rsk(N*)"
        x4_name = "γ_S,fat"
        na_text = "γ_S,fat = 1.15 (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_72(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x3/x2 + 0.43*sqrt(1 - x1/x3)
        result_name = "result: "
        result_units = ""
        x1_name = "σ_cd,min,equ"
        x2_name = "f_cd,fat"
        x3_name = "σ_cd,max,equ"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_76(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*(1 - x4/250)
        result_name = "f_cd,fat = "
        result_units = ""
        x1_name = "k_1"
        x2_name = "β_cc(t_0)"
        x3_name = "f_cd"
        x4_name = "f_ck"
        na_text = "k_1 = 1 (Germany)\n"
        na_text += "k_1 = 1 (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_77(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1/x3
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.5 + 0.45*x2/x3
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "σ_c,max"
        x2_name = "σ_c,min"
        x3_name = "f_cd,fat"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_78(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1/x3
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.5 + 0.45*x2/x3
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "V_Ed,max"
        x2_name = "V_Ed,min"
        x3_name = "V_Rd,c"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__6_79(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1/x3
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.5 - x2/x3
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "V_Ed,max"
        x2_name = "V_Ed,min"
        x3_name = "V_Rd,c"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "A_s,min*σ_s = "
        result_units = "kN"
        x1_name = "k_c"
        x2_name = "k"
        x3_name = "f_ct,eff [kN/cm2]"
        x4_name = "A_ct [cm2]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.4*(1 - x1/(x2*x3/x4*x5))
        result_name = "k_c = "
        result_units = ""
        x1_name = "σ_c"
        x2_name = "k_1"
        x3_name = "h"
        x4_name = "h*"
        x5_name = "f_ct,eff"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.9*x1/(x2*x3)
        result_name = "k_c = "
        result_units = ""
        x1_name = "F_cr [kN]"
        x2_name = "A_ct [cm2]"
        x3_name = "f_ct,eff [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "σ_c = "
        result_units = "kN/cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "b [cm]"
        x3_name = "h [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1*x2/x3)
        result_name = "ξ_1 = "
        result_units = ""
        x1_name = "ξ"
        x2_name = "Φ_s"
        x3_name = "Φ_p"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/2.9*x3*x4/(x5-x6)
        result_name = "Φ_s = "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "f_ct,eff" 
        x3_name = "k_c"
        x4_name = "h_cr"
        x5_name = "h"
        x6_name = "d"
        na_text = "Gleichung verändert sich. Sehe 7_7_1_AT und 7_7_2_AT (Austria)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/2.9*x3/(8*(x4-x5))
        result_name = "Φ_s = "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "f_ct,eff" 
        x3_name = "h_cr"
        x4_name = "h"
        x5_name = "d"
        na_text = "Gleichung verändert sich. Sehe 7_7_1_AT und 7_7_2_AT (Austria)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_7_1_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3/(4*(x4 - x5)*x6*2.9)
        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x7/2.9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "σ_s"
        x3_name = "A_s"
        x4_name = "h"
        x5_name = "d"
        x6_name = "b"
        x7_name = "f_ct,eff"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_7_2_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*x4*x5/(4*(x6 - x7)*2.9)
        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x5/2.9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "k_c"
        x3_name = "k"
        x4_name = "h_cr"
        x5_name = "f_ct,eff"
        x6_name = "h"
        x7_name = "d"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_7_3_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        6*x1*x2*x3/x4**2
        result_name = "Φ*_s = "
        result_units = ""
        x1_name = "w_k"
        x2_name = "f_ct,eff"
        x3_name = "E_s"
        x4_name = "σ_s"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "w_k = "
        result_units = "cm"
        x1_name = "s_r,max [cm]"
        x2_name = "ε_sm"
        x3_name = "ε_cm"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         (x1 - x2*x3/x4*(1 + x5/x6*x4))/x5
        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*x1/x5
                                  
        result_name = "ε_sm - ε_cm = "
        result_units = ""
        x1_name = "σ_s"
        x2_name = "k_t"
        x3_name = "f_ct,eff"
        x4_name = "ρ_p,eff"
        x5_name = "E_s"
        x6_name = "E_cm"
        na_text = "k_t = 0.4 (Germany)"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*x3)/x4
        result_name = "ρ_p,eff = "
        result_units = ""
        x1_name = "A_s"
        x2_name = "ξ_1"
        x3_name = "A'_p"
        x4_name = "A_c,eff"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__7_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x3*x5 + x1*x2*x4*x6/x7
        result_name = "s_r,max = "
        result_units = ""
        x1_name = "k_1"
        x2_name = "k_2"
        x3_name = "k_3"      
        x4_name = "k_4"
        x5_name = "c"
        x6_name = "Φ"
        x7_name = "ρ_p,eff"
        na_text = "Gleichung verändert sich. Sehe 7_11_DE (Germany)\n"
        na_text += "Gleichung verändert sich. Sehe 7_11_AT (Austria)\n"
        na_text += "k_3 = 3.4, k_4 = 0.425 (Slovenia)"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_11_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(3.6*x3)
        result_name = "s_r,max = "
        result_units = ""
        x1_name = "σ_s"
        x2_name = "Φ"
        x3_name = "f_ct,eff"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_11_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1/(x2*3.6)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x3*x1/(3.6*x4)
                        
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ"
        x2_name = "ρ_p,eff"
        x3_name = "σ_s"
        x4_name = "f_ct,eff"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "F_Rdu = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x3**2 + x2*x4**2)/(x1*x3 + x2*x4)
        result_name = "Φ_ef = "
        result_units = ""
        x1_name = "n_1"
        x2_name = "n_2"
        x3_name = "Φ_1"
        x4_name = "Φ_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2)/(2*x1)
        result_name = "k_2 = "
        result_units = ""
        x1_name = "ε_1"
        x2_name = "ε_2"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.3*(x1 - x2)
        result_name = "s_r,max = "
        result_units = "cm"
        x1_name = "h [cm]"
        x2_name = "x [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(cos(x1)/x2 + sin(x1)/x3)
        result_name = "s_r,max = "
        result_units = "cm"
        x1_name = "θ [rad]"
        x2_name = "s_r,max,y [cm]"
        x3_name = "s_r,max,z [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_16a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(11 + 1.5*x2*10**(-3)/x3 + 3.2*sqrt(x2)*(10**(-3)*sqrt(x2)/x3 - 1)**(3/2))
        result_name = "l/d = "
        result_units = ""
        x1_name = "K"
        x2_name = "f_ck [MPa]"
        x3_name = "ρ"
        na_text = "Za K glej tabelo 7.4 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_16b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(11 + 1.5*x2*10**(-3)/(x3 - x4) + 1/12*sqrt(x2)*sqrt(x4/(sqrt(x2)*10**(-3))))
        result_name = "l/d = "
        result_units = ""
        x1_name = "K"
        x2_name = "f_ck [MPa]"
        x3_name = "ρ"
        x4_name = "ρ'"
        na_text = "Za K glej tabelo 7.4 (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        500*x3/(x1*x2)
        result_name = "310/σ_s  = "
        result_units = "cm2/kN"
        x1_name = "f_yk"
        x2_name = "A_s,req"
        x3_name = "A_s,prov"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x3*x2 + (1 - x3)*x1
        result_name = "α  = "
        result_units = ""
        x1_name = "α_I"
        x2_name = "α_II"
        x3_name = "ζ"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 - x1*(x2/x3)**2
        result_name = "ζ = "
        result_units = ""
        x1_name = "β"
        x2_name = "σ_sr"
        x3_name = "σ_s"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(1 + x2)
        result_name = "E_c,eff = "
        result_units = "kN/cm2"
        x1_name = "E_cm [kN/cm2]"
        x2_name = "φ(∞,t0)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__7_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3/x4
        result_name = "1/r_cs = "
        result_units = "1/cm"
        x1_name = "ε_cs"
        x2_name = "E_s"
        x3_name = "E_c,eff"
        x4_name = "S [cm3]"
        x5_name = "I [cm4]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1/x2 + 1/(2*x3))/x4
        result_name = "Φ_m,min = "
        result_units = ""
        x1_name = "F_bt"
        x2_name = "a_b"
        x3_name = "Φ"
        x4_name = "f_cd"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2.25*x1*x2*x3
        result_name = "f_bd = "
        result_units = "kN/cm2"
        x1_name = "η_1"
        x2_name = "η_2"
        x3_name = "f_ctd [kN/cm2]"
        na_text = "f_ctd = f_ctk,0.05/γ_C (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/4*x2/x3
        result_name = "l_b,rdq = "
        result_units = ""
        x1_name = "Φ"
        x2_name = "σ_sd"
        x3_name = "f_bd"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*x6
        result_name = "l_bd = "
        result_units = "cm or mm"
        x1_name = "α_1"
        x2_name = "α_2"
        x3_name = "α_3"
        x4_name = "α_4"
        x5_name = "α_5"
        x6_name = "l_b,rdq"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "result: "
        result_units = ""
        x1_name = "α_2"
        x2_name = "α_3"
        x3_name = "α_5"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.3*x1, 10*x2, 100)
        result_name = "l_b,min = "
        result_units = "mm"
        x1_name = "l_b,rdq [mm]"
        x2_name = "φ"
        na_text = "l_b,rdq = α_1*α_4*l_b,rdq, 100mm wird entfernt (Germany)\n"
        na_text += "100mm wird mit 70mm ersetzt (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.6*x1, 10*x2, 100)
        result_name = "l_b,min = "
        result_units = "mm"
        x1_name = "l_b,rdq [mm]"
        x2_name = "φ"
        na_text = "100mm wird entfernt (Germany)\n"
        na_text += "100mm wird mit 70mm ersetzt (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.16*x1*(x2*(0.015 + 0.14*exp((-0.18)*(2*x5/x1 + 1)))/(x3 + x4))**0.5*(x3 + x4)/(0.015 + 0.14*exp((-0.18)*(2*x5/x1 + 1)))
        result_name = "F_btd = "
        result_units = ""
        x1_name = "Φ_t"
        x2_name = "f_yd"
        x3_name = "f_ctd"
        x4_name = "σ_cm"
        x5_name = "c"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        16*x1*x2*x3/x4
        result_name = "F_btd = "
        result_units = "kN"
        x1_name = "A_s [cm2]"
        x2_name = "f_cd [kN/cm2]"
        x3_name = "Φ_t"
        x4_name = "Φ_l"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*x6
        result_name = "l_0 = "
        result_units = "cm or mm"
        x1_name = "α_1"
        x2_name = "α_2"
        x3_name = "α_3"
        x4_name = "α_5"
        x5_name = "α_6"
        x6_name = "l_b,rdq"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.3*x1*x2, 15*x3, 200)
        result_name = "l_0,min = "
        result_units = "mm"
        x1_name = "α_6"
        x2_name = "l_b,rdq [mm]"
        x3_name = "Φ"
        na_text = "α_6 = α_6*α_1 (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*x1*x2
        result_name = "A_sh = "
        result_units = "cm2"
        x1_name = "A_s [cm2]"
        x2_name = "n_1"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*x1*x2
        result_name = "A_sv = "
        result_units = "cm2"
        x1_name = "A_s [cm2]"
        x2_name = "n_2"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*sqrt(x2)
        result_name = "Φ_n = "
        result_units = ""
        x1_name = "Φ"
        x2_name = "n_b"
        na_text = "Φ_n <= 28mm (>=C70/85) (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "f_bpt = "
        result_units = "kN/cm2"
        x1_name = "η_np1"
        x2_name = "η_1"
        x3_name = "f_ctd(t) [kN/cm2]"
        na_text = "η_np1 = 2.85 (Φ <= 8mm) (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4/x5
        result_name = "l_pt = "
        result_units = "cm or mm"
        x1_name = "α_1"
        x2_name = "α_2"
        x3_name = "Φ"
        x4_name = "σ_pm0"
        x5_name = "f_bpt"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.8*x1
        result_name = "l_pt1 = "
        result_units = ""
        x1_name = "l_pt"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.2*x1
        result_name = "l_pt1 = "
        result_units = "cm"
        x1_name = "l_pt [cm]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1**2 + x2**2)
        result_name = "l_disp = "
        result_units = "cm"
        x1_name = "l_pt [cm]"
        x2_name = "d [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "f_bpd = "
        result_units = "kN/cm2"
        x1_name = "η_p2"
        x2_name = "η_1"
        x3_name = "f_ctd [kN/cm2]"
        na_text = "η_p2 = 1.4 für profilierte Drähte und für Litzen mit 7 Drähten (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__8_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3*(x4 - x5)/x6
        result_name = "l_bpd = "
        result_units = ""
        x1_name = "l_pt2"
        x2_name = "α_2"
        x3_name = "Φ"
        x4_name = "σ_pd"
        x5_name = "σ_pm∞"
        x6_name = "f_bpd"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.26*x1/x2*x3*x4
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "f_ctm"
        x2_name = "f_yk"
        x3_name = "b_t [cm]"
        x4_name = "d [cm]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1/tan(x2) - 1/tan(x3))/2
        result_name = "a_l = "
        result_units = "cm"
        x1_name = "z [cm]"
        x2_name = "θ [rad]"
        x3_name = "α [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)*x2/x3 + x4
        result_name = "F_Ed = "
        result_units = "kN"
        x1_name = "V_Ed [kN]"
        x2_name = "a_l"
        x3_name = "z"
        x4_name = "N_Ed [kN]"
        na_text = "F_Ed >= 0.5*V_Ed (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3*sin(x4))
        result_name = "ρ_w = "
        result_units = ""
        x1_name = "A_sw"
        x2_name = "s"
        x3_name = "b_w"
        x4_name = "α [rad]"
        na_text = "ρ_w,min = (0.16 or 0.256)*f_ctm/f_yk (Germany)\n"
        na_text += "ρ_w,min = 0.08*sqrt(f_ck)/f_yk (Slovenia)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.08*sqrt(x1)/x2
        result_name = "ρ_w,min = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "f_yk"
        na_text = "Gleichung verändert sich. Sehe 9_5_DE (Germany)\n"
        na_text += "Gleichung verändert sich. Sehe 9_5_AT (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_5_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "ρ_w,min = "
        result_units = ""
        x1_name = "k"
        x2_name = "f_ctm"
        x3_name = "f_yk"
        na_text = "k = 0.16 (allgemein), k = 0.256 (gegliederte Querschnitte) (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_5_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.15*x1/x2
        result_name = "ρ_w,min = "
        result_units = ""
        x1_name = "f_ctm"
        x2_name = "f_yd"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1*(1 + 1/tan(x2))
        result_name = "s_l,max = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "α [rad]"
        na_text = "s_l,max <= 250mm (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*x1*(1 + 1/tan(x2))
        result_name = "s_b,max = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "α [rad]"
        na_text = "Gleichung verändert sich. Sehe 9_7_DE (Germany)\n"
        na_text += "Gleichung verändert sich. Sehe 9_7_AT (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_7_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*(1 + 1/tan(x2))
        result_name = "s_b,max = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "α [rad]"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_7_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.8*x1*(1 + 1/tan(x2))
        result_name = "s_b,max = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "α [rad]"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1
        result_name = "s_t,max = "
        result_units = "cm"
        x1_name = "d [cm]"
        na_text = "s_t,max <= 800mm (Austria)"
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1*(1 + 1/tan(x2))
        result_name = "s_max = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "α [rad]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1
        result_name = "s_max = "
        result_units = "cm"
        x1_name = "d [cm]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*(1.5*sin(x2) + cos(x2))/(x3*x4)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        0.08*sqrt(x5)/x6
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "A_sw,min"
        x2_name = "α [rad]"
        x3_name = "s_r"
        x4_name = "s_t"
        x5_name = "f_ck [MPa]"
        x6_name = "f_yk"
        na_text = "Gleichung verändert sich. Sehe 9_11_DE (Germany)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_11_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.08/1.5*sqrt(x1)/x2*x3*x4
        result_name = "A_sw,min = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "f_yk"
        x3_name = "s_r"
        x4_name = "s_t"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.1*x1/x2, 0.002*x3)
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "A_c [cm2]"
        na_text = "Gleichung verändert sich. Sehe 9_12_DE (Germany)\n"
        na_text += "Gleichung verändert sich. Sehe 9_12_AT (Austria)\n"
        na_text += "Enačba se spremeni. Glej 9_12_SI (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_12_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(0.15*abs(x1)/x2, 0.09*x3)
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "A_c [cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_12_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.13*abs(x1)/x2, 0.0026*x3)
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "A_c [cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_12_SI(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.15*abs(x1)/x2, 0.003*x3)
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "A_c [cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "F_s = "
        result_units = "kN"
        x1_name = "R [kN]"
        x2_name = "z_e [cm]"
        x3_name = "z_i [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*(1 - x1/x2)*x3
        result_name = "F_s = "
        result_units = "kN"
        x1_name = "c"
        x2_name = "h"
        x3_name = "N_Ed [kN]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "F_tie,per = "
        result_units = "kN"
        x1_name = "l_i [m]"
        x2_name = "q_1 [kN/m]"
        na_text = "q_1 = 10 kN/m, Q_2 = 70 kN (Germany)\n"
        na_text += "q_1 = 10 kN/m, Q_2 = 70 kN (Austria)\n"
        na_text += "q_1 = 10 kN/m, Q_2 = 70 kN (Slovenia)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__9_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 + x3)/2
        result_name = "F_tie = "
        result_units = "kN"
        x1_name = "q_3 [kN/m]"
        x2_name = "l_1 [m]"
        x3_name = "l_2 [m]"
        na_text = "q_3 = 20 kN/m, Q_4 = 70 kN (Germany)\n"
        na_text += "q_3 = 20 kN/m, Q_4 = 70 kN (Austria)\n"
        na_text += "q_3 = 20 kN/m, Q_4 = 70 kN (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + (x2 - x1)/log10(28 - x3 + 1)*log10(x4 - x3 + 1)
        result_name = "f_cm(t) = "
        result_units = "MPa or kN/cm"
        x1_name = "f_cmp"
        x2_name = "f_cm"
        x3_name = "t_p [days]"
        x4_name = "t [days]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.14**(x1 - 20)/(x1 - 20)*x2
        result_name = "t_eq = "
        result_units = "hours"
        x1_name = "T_max"
        x2_name = "Σ(Ti - 20)Δti"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*x2*x3*(x4 - x5)
        result_name = "ΔP_0 = "
        result_units = "kN"
        x1_name = "A_P [cm2]"
        x2_name = "E_p [kN/cm2]"
        x3_name = "α_c [1/°C]"
        x4_name = "T_max [°C]"
        x5_name = "T_0 [°C]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/3
        result_name = "v_Ed = "
        result_units = "kN/m"
        x1_name = "q_Ed [kN/m2]"
        x2_name = "b_e [m]"
        na_text = "b_e/3 >= 0.50m (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*x1/x2*x3/x4
        result_name = "A_s = "
        result_units = "cm2"
        x1_name = "t"
        x2_name = "h"
        x3_name = "F_Ed [kN]"
        x4_name = "f_yd [kN/cm2]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__10_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3) + x4 + x5 + sqrt(x6**2 + (x7/2500)**2)
        result_name = "a = "
        result_units = "cm"
        x1_name = "F_Ed [kN]"
        x2_name = "b_1 [cm]"
        x3_name = "f_Rd  [kN/cm2]"
        x4_name = "a_2 [cm]"
        x5_name = "a_3 [cm]"
        x6_name = "Δa_2 [cm]"
        x7_name = "l_n"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.4 + 0.6*x1/2200
        result_name = "η_1 = "
        result_units = ""
        x1_name = "ρ"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__11_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/2200)**2
        result_name = "η_E = "
        result_units = ""
        x1_name = "ρ"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_3_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2)/x3
        result_name = "f_lcd = "
        result_units = "kN/cm2"
        x1_name = "α_lcc"
        x2_name = "f_lck [kN/cm2]"
        x3_name = "γ_C"
        na_text = "α_lcc = 0.75 or 0.8 (Germany)\n"
        na_text += "α_lcc = 0.85 (Austria)\n"
        na_text += "α_lcc = 0.85 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_3_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2)/x3
        result_name = "f_lctd = "
        result_units = "kN/cm2"
        x1_name = "α_lct"
        x2_name = "f_lctk [kN/cm2]"
        x3_name = "γ_C"
        na_text = "α_lct = 0.85 (Germany)\n"
        na_text += "α_lct = 0.85 (Austria)\n"
        na_text += "α_lct = 0.85 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_3_24(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + x2*x3/x1)
        result_name = "f_lck,c = "
        result_units = "kN/cm2"
        x1_name = "f_lck [kN/cm2]"
        x2_name = "k"
        x3_name = "σ_2"
        na_text = "k = 1.1 for lightweight aggregate concrete with sand as the fine aggregate\n"
        na_text += "k = 1.0 for lightweight aggregate (both fine and coarse aggregate) concrete\n"
        na_text += "(Germany), (Austria) and (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_3_26(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2/x3)**2
        result_name = "ε_lc2,c = "
        result_units = ""
        x1_name = "ε_lc2"
        x2_name = "f_lckc"
        x3_name = "f_lck"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_3_27(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 0.2*x2/x3
        result_name = "ε_lcu2 = "
        result_units = ""
        x1_name = "ε_lcu2"
        x2_name = "σ_2"
        x3_name = "f_lck"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         (x1*x2*x3*(100*x4*x5)**(1/3) + x6*x7)*x8*x9
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        (x2*x10 + x6*x7)*x8*x9
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_lRd,c"
        x2_name = "η_1"
        x3_name = "k"
        x4_name = "ρ_l"
        x5_name = "f_lck [MPa]"
        x6_name = "k_1"
        x7_name = "σ_cp [N/mm2]"
        x8_name = "b_w [mm]"
        x9_name = "d [mm]"
        x10_name = "v_min [N/mm2]"
        na_text = "k_1 = 0.12, C_lRd,c = 0.15/γ_C (Germany)\n"
        na_text += "k_1 = 0.15, C_lRd,c = 0.15/γ_C, v_1,min = 0,028*k3^(3/2)*f_ck^0.5 (Austria)\n"
        na_text += "k_1 = 0.15, C_lRd,c = 0.15/γ_C, v_1,min = 0,028*k3^(3/2)*f_ck^0.5 (Slovenia)"
        self.widget = Calculator(10, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, x10_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*x2*x3*x4
        result_name = "V_Ed = "
        result_units = "kN"
        x1_name = "b_w [cm]"
        x2_name = "d [cm]"
        x3_name = "ν_1"
        x4_name = "f_lcd"
        na_text = "ν_1 = 0.675*η_1 (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_6N(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*(1 - x1/250)
        result_name = "ν_1 = "
        result_units = ""
        x1_name = "f_lck"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_47(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*(100*x4*x5)**(1/3) + x6*x7
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x2*x8 + x6*x7
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_lRd,c"
        x2_name = "η_1"
        x3_name = "k"
        x4_name = "ρ_l"
        x5_name = "f_lck [MPa]"
        x6_name = "k_2"
        x7_name = "σ_cp [N/mm2]"
        x8_name = "v_l,min [N/mm2]"
        na_text = "k_2 = 0.08, C_lRd,c = 0.15/γ_C (Germany)\n"
        na_text += "k_2 = 0.08 (Austria)\n"
        na_text += "k_2 = 0.08 (Slovenia)"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, \
                                 non_equation = True, \
                                 result_name2 = "v_lRD,c = ", \
                                 calc_function2 = calc_function2, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_50(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*(100*x4*x5)**(1/3)*2*x6/x7
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x2*x8*2*x6/x7
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_lRd,c"
        x2_name = "η_1"
        x3_name = "k"
        x4_name = "ρ_l"
        x5_name = "f_lck [MPa]"
        x6_name = "d [cm]"
        x7_name = "a [cm]"
        x8_name = "v_lmin [N/mm2]"
        na_text = "C_lRd,c = 0.15/γ_C (Germany)"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, \
                                 non_equation = True, \
                                 result_name2 = "v_lRD,c = ", \
                                 calc_function2 = calc_function2, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_52(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1 + 1.5*x2/x3*1/(x4*x2)*x5*x6*sin(x7)
        result_name = "v_Rd,cs = "
        result_units = "kN/cm2"
        x1_name = "v_Rd,c [kN/cm2]"
        x2_name = "d [cm]"
        x3_name = "s_r [cm]"
        x4_name = "u_1 [cm]"
        x5_name = "A_sw [cm2]"
        x6_name = "f_ywd,eff [kN/cm2]"
        x7_name = "α [rad]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_53(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "V_Ed [kN]"
        x2_name = "u_0 [cm]"
        x3_name = "d [cm]"
        na_text = "v_Rd,max = 0.4*ν*f_lcd (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__11_6_63(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*(x3/x1)**(x4/4400)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        3*x2*x1*x4/2200
                        
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "A_c0 [cm2]"
        x2_name = "f_lcd [kN/cm2]"
        x3_name = "A_c1 [cm2]"
        x4_name = "ρ"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "F_Rdu = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2)/x3
        result_name = "f_ctd,pl = "
        result_units = "kN/cm2"
        x1_name = "α_ct,pl"
        x2_name = "f_ctk,0.05 [kN/cm2]"
        x3_name = "γ_C"
        na_text = "Gleichung verändert sich. Sehe 12_1_AT (Austria)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_1_AT(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2)/x3
        result_name = "f_cd,pl = "
        result_units = "kN/cm2"
        x1_name = "α_cc,pl"
        x2_name = "f_ck [kN/cm2]"
        x3_name = "γ_C"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*(1 - 2*x5/x4)
        result_name = "N_Rd = "
        result_units = "kN"
        x1_name = "η"
        x2_name = "f_cd,pl [kN/cm2]"
        x3_name = "b [cm]"
        x4_name = "h_w [cm]"
        x5_name = "e [cm]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "σ_cp = "
        result_units = "kN/cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "A_cc [cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "τ_cp = "
        result_units = "kN/cm2"
        x1_name = "k"
        x2_name = "V_Ed [kN]"
        x3_name = "A_cc [cm2]"
        na_text = "Gleichung verändert sich. Sehe 12_4_DE (Germany)\n"
        na_text += "k = 1.5 (Austria)\n"
        na_text += "k = 1.5 (Slovenia)"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_4_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4)
        result_name = "τ_cp = "
        result_units = "kN/cm2"
        x1_name = "V_Ed [kN]"
        x2_name = "S [cm3]"
        x3_name = "b_w [cm]"
        x4_name = "I [cm4]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1**2 + x1*x2)
        result_name = "f_cvd = "
        result_units = "kN/cm2"
        x1_name = "f_ctd,pl [kN/cm2]"
        x2_name = "σ_cp"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1**2 + x1*x2 - (x2 - x3)**2/4)
        result_name = "f_cvd = "
        result_units = "kN/cm2"
        x1_name = "f_ctd,pl [kN/cm2]"
        x2_name = "σ_cp [kN/cm2]"
        x3_name = "σ_c,lim [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - 2*sqrt(x2*(x2 + x1))
        result_name = "σ_c,lim = "
        result_units = "kN/cm2"
        x1_name = "f_cd,pl [kN/cm2]"
        x2_name = "f_ctd,pl [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "λ = "
        result_units = ""
        x1_name = "l_0"
        x2_name = "i"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "l_0 = "
        result_units = "cm"
        x1_name = "β"
        x2_name = "l_w [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "N_Rd = "
        result_units = "kN"
        x1_name = "b [cm]"
        x2_name = "h_w [cm]"
        x3_name = "f_cd,pl [kN/cm2]"
        x4_name = "Φ"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                  x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         1.14*(1 - 2*x1/x2) - 0.02*x3/x2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        1 - 2*x1/x2
                        
        result_name = "Right side: "
        result_units = ""
        x1_name = "e_tot [cm]"
        x2_name = "h_w [cm]"
        x3_name = "l_0 [cm]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "e_tot = "
        result_units = "cm"
        x1_name = "e_0 [cm]"
        x2_name = "e_i [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__12_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         0.85*x1/x2
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        sqrt(3*x3/x4)
                        
        result_name = "Right side: "
        result_units = ""
        x1_name = "h_F [cm]"
        x2_name = "a [cm]"
        x3_name = "σ_gd [kN/cm2]"
        x4_name = "f_ctd,pl [kN/cm2]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "F_Rdu = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__B_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "φ(t,t0) = "
        result_units = ""
        x1_name = "φ_0"
        x2_name = "β_c(t,t0)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_1__B_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "φ_0 = "
        result_units = ""
        x1_name = "φ_RH"
        x2_name = "β(f_cm)"
        x3_name = "β(t_0)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_3a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + (1 - x1/100)/(0.1*x2**(1/3))
        result_name = "φ_RH = "
        result_units = ""
        x1_name = "RH"
        x2_name = "h_0"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_3b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 + (1 - x1/100)/(0.1*x2**(1/3))*x3)*x4
        result_name = "φ_RH = "
        result_units = ""
        x1_name = "RH"
        x2_name = "h_0"
        x3_name = "α_1"
        x4_name = "α_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        16.8/sqrt(x1)
        result_name = "β(f_cm) = "
        result_units = ""
        x1_name = "f_cm [MPa]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(0.1 + x1**0.2)
        result_name = "β(t_0) = "
        result_units = ""
        x1_name = "t_0"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1/x2
        result_name = "h_0 = "
        result_units = "cm"
        x1_name = "A_c [cm2]"
        x2_name = "u [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        ((x1 - x2)/(x3 + x1 - x2))**0.3
        result_name = "β_c(t,t0) = "
        result_units = ""
        x1_name = "t [days]"
        x2_name = "t_0 [days]"
        x3_name = "β_H"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_8a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.5*(1 + (0.012*x1)**18)*x2 + 250
        result_name = "β_H = "
        result_units = ""
        x1_name = "RH"
        x2_name = "h_0"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_8b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         1.5*(1 + (0.012*x1)**18)*x2 + 250*x3
                         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1500*x3
        result_name = "Right side: = "
        result_units = ""
        x1_name = "RH"
        x2_name = "h_0"
        x3_name = "α_3"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_8c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (35/x1)**x2
        result_name = "α_1 = "
        result_units = ""
        x1_name = "f_cm"
        x2_name = "exponent"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(9/(2 + x1**1.2) + 1)**x2
        result_name = "t_0 = "
        result_units = ""
        x1_name = "t_0,T [days]"
        x2_name = "α"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.85*((220 + 110*x1)*exp((-x2)*x3/x4))*10**(-6)*x5
        result_name = "ε_cd,0 = "
        result_units = ""
        x1_name = "α_ds1"
        x2_name = "α_ds2"
        x3_name = "f_cm"
        x4_name = "f_cm0"
        x5_name = "β_RH"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__B_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.55*(1 - (x1/x2)**3)
        result_name = "β_RH = "
        result_units = ""
        x1_name = "RH"
        x2_name = "RH_0"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__C_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.098*(80 - 1.2*x1)
        result_name = "τ_m = "
        result_units = ""
        x1_name = "Φ"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__C_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.098*(130 - 1.9*x1)
        result_name = "τ_r = "
        result_units = ""
        x1_name = "Φ"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__C_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "M = "
        result_units = ""
        x1_name = "C_v"
        x2_name = "a"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__D_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.66*x1*2.7183**(9.09*x2)*(x3/1000)**(0.75*(1 - x2))*(x4 + x5)*10**(-5)
        result_name = "ΣΔσ_pr,i = "
        result_units = ""
        x1_name = "ρ_1000"
        x2_name = "μ"
        x3_name = "t_e"
        x4_name = "σ_p,i"
        x5_name = "ΣΔσ_pr,i"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__D_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.66*x1*2.7183**(9.09*x2)*((x3 + x4)/1000)**(0.75*(1 - x2))*(x5 + x6)*10**(-5) - x6
        result_name = "ΣΔσ_pr,i = "
        result_units = ""
        x1_name = "ρ_1000"
        x2_name = "μ"
        x3_name = "t_e"
        x4_name = "Δt_i"
        x5_name = "σ_p,i"
        x6_name = "ΣΔσ_pr,i"
        
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "f_tdx = "
        result_units = "kN/cm2"
        x1_name = "ρ_x"
        x2_name = "f_yd [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1) - x2
        result_name = "f'_tdx = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1) - x2
        result_name = "f'_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edy [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*abs(x1)
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "τ_Edy [kN/cm2]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2/x2 - x3
        result_name = "f'_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        x3_name = "σ_Edy [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + (x2/x1)**2)
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "σ_Edx [kN/cm2]"
        x2_name = "τ_Edxy [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)/tan(x2) - x3
        result_name = "f_tdx = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "θ [rad]"
        x3_name = "σ_Edx [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)*tan(x2) - x3
        result_name = "f_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "θ [rad]"
        x3_name = "σ_Edy [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__F_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)*(1/tan(x2) + tan(x2))
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "θ [rad]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__G_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3**3)
        result_name = "K_R = "
        result_units = ""
        x1_name = "(EJ)_S"
        x2_name = "E"
        x3_name = "I"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.1*x1
        result_name = "F_V,Ed = "
        result_units = "kN"
        x1_name = "F_V,BB [kN]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3**2
        result_name = "F_V,BB = "
        result_units = "kN"
        x1_name = "ξ"
        x2_name = "ΣEI [kNcm2]"
        x3_name = "L [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.4*x1*x2
        result_name = "EI = "
        result_units = "kNcm2"
        x1_name = "E_ca [kN/cm2]"
        x2_name = "I_c [cm4]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        7.8*x1/(x1 + 1.6)*1/(1 + 0.7*x2)
        result_name = "ξ = "
        result_units = ""
        x1_name = "n_S"
        x2_name = "k"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)*(x3/x4)
        result_name = "k = "
        result_units = ""
        x1_name = "θ"
        x2_name = "M"
        x3_name = "EI"
        x4_name = "L"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.1*x1/(1 + x1/x2)
        result_name = "0.1*F_V,B = "
        result_units = "kN"
        x1_name = "F_V,BB [kN]"
        x2_name = "F_V,BS [kN]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(1 - x2/x3)
        result_name = "F_H,Ed = "
        result_units = "kN"
        x1_name = "F_H,0Ed [kN]"
        x2_name = "F_V,Ed [kN]"
        x3_name = "F_V,B [kN]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__H_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(1 - x2/x1)
        result_name = "F_H,Ed = "
        result_units = "kN"
        x1_name = "F_H,0Ed [kN]"
        x2_name = "F_H,1Ed [kN]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_1__I_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2/x3 + x1*x4*x5*x2/(x3*x5**2)
                         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3 - x1*x4*x5*x2/(x3*x5**2)
        result_name = "P_n(-) = "
        result_units = "kN"
        x1_name = "P [kN]"
        x2_name = "(EI)_n [kNcm2]"
        x3_name = "Σ(EI) [kNcm2]"
        x4_name = "e [cm]"
        x5_name = "y_n [cm]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name,\
                                 non_equation = True, \
                                 result_name2 = "P_n(+) = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "X_d,fi = "
        result_units = ""
        x1_name = "k_θ"
        x2_name = "X_k"
        x3_name = "γ_M,fi"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_2a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "X_d,fi = "
        result_units = ""
        x1_name = "X_k,θ"
        x2_name = "γ_M,fi"
        na_text = "γ_M,fi = 1 (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_2b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "X_d,fi = "
        result_units = ""
        x1_name = "X_k,θ"
        x2_name = "γ_M,fi"
        na_text = "γ_M,fi = 1 (Germany)"        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_1_2__2_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "E_d,fi = "
        result_units = ""
        x1_name = "η_fi"
        x2_name = "E_d"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*x3)/(x4*x1 + x5*x3)
        result_name = "η_fi = "
        result_units = ""
        x1_name = "G_k"
        x2_name = "ψ_fi"
        x3_name = "Q_k,1"
        x4_name = "γ_G"
        x5_name = "γ_Q,1"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_5a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*x3)/(x4*x1 + x5*x3*x6)
        result_name = "η_fi = "
        result_units = ""
        x1_name = "G_k"
        x2_name = "ψ_fi"
        x3_name = "Q_k,1"
        x4_name = "γ_G"
        x5_name = "γ_Q,1"
        x6_name = "ψ_0,1"
        
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__2_5b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*x3)/(x4*x1*x5 + x3*x6)
        result_name = "η_fi = "
        result_units = ""
        x1_name = "G_k"
        x2_name = "ψ_fi"
        x3_name = "Q_k,1"
        x4_name = "ξ"
        x5_name = "γ_G"
        x6_name = "γ_Q,1"
        
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__3_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "f_ck,t(θ) = "
        result_units = ""
        x1_name = "k_c,t(θ)"
        x2_name = "f_ck,t"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__3_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        ((x1 - x2/x3)/(x4 - x2/x3)*(x5 - x2)/x5 + x2/x5)
        result_name = "β = "
        result_units = ""
        x1_name = "ε_ud"
        x2_name = "f_p0,1k"
        x3_name = "E_p"
        x4_name = "ε_uk"
        x5_name = "f_pk"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__4_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 + x3 + x4
        result_name = "ε = "
        result_units = ""
        x1_name = "ε_th"
        x2_name = "ε_σ"
        x3_name = "ε_creep"
        x4_name = "ε_tr"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "result = "
        result_units = ""
        x1_name = "E_d,fi"
        x2_name = "R_d,fi"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3/x4*x5/x6
        result_name = "σ_s,fi = "
        result_units = "kN/cm2"
        x1_name = "E_d,fi [kN/cm2]"
        x2_name = "E_d [kN/cm2]"
        x3_name = "f_yk [kN/cm2]"
        x4_name = "γ_s"
        x5_name = "A_s,req [cm2]"
        x6_name = "A_s,prov [cm2]"
        
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.1*(500 - x1)
        result_name = "Δa = "
        result_units = "mm"
        x1_name = "θ_cr"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 0.8*(400 - x2)
        result_name = "b_mod = "
        result_units = "mm"
        x1_name = "b_min [mm]"
        x2_name = "θ_cr"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "a_m = "
        result_units = ""
        x1_name = "ΣA_si*a_i"
        x2_name = "ΣA_si"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "μ_fi = "
        result_units = ""
        x1_name = "N_Ed,fi"
        x2_name = "N_Rd"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        120*((83*(1 - x1*(1 + x3*x4/(x5*x6))/(0.85/x2 + x3*x4/(x5*x6))) + \
                        1.6*(x7 - 30) + 9.6*(5 - x8) + 0.09*2*x5/(x9 + x10) + x11)/120)**1.8
        result_name = "R = "
        result_units = ""
        x1_name = "μ_fi"
        x2_name = "α_cc"
        x3_name = "A_s"
        x4_name = "f_yd"
        x5_name = "A_c"
        x6_name = "f_cd"
        x7_name = "a"
        x8_name = "l_o,fi"
        x9_name = "b"
        x10_name = "h"
        x11_name = "R_n"
        
        self.widget = Calculator(10, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, x8_name, x9_name, x10_name, x11_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_8a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(0.7*(x2*x3 + x4*x5))
        result_name = "n = "
        result_units = ""
        x1_name = "N_0Ed,fi [kN]"
        x2_name = "A_c [cm2]"
        x3_name = "f_cd [kN/cm2]"
        x4_name = "A_s [cm2]"
        x5_name = "f_yd [kN/cm2]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_8b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "e = "
        result_units = "cm"
        x1_name = "M_0Ed,fi [kNcm]"
        x2_name = "N_0Ed,fi [kN]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_8c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "λ_fi = "
        result_units = ""
        x1_name = "l_0,fi"
        x2_name = "i"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 0.5*x2
        result_name = "d_eff = "
        result_units = "cm"
        x1_name = "d_1 [cm]"
        x2_name = "d_2 [cm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1.85 - x2/x3*sqrt(x4/x5))
        result_name = "a_eff = "
        result_units = ""
        x1_name = "a"
        x2_name = "d_eff"
        x3_name = "b_min"
        x4_name = "b_w"
        x5_name = "b"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__5_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - 2.5*x2/x3)
        result_name = "A_s,req(x) = "
        result_units = ""
        x1_name = "A_s,req(0)"
        x2_name = "x"
        x3_name = "l_eff"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__6_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "M_d,fi = "
        result_units = "kNm"
        x1_name = "M_500 [kNm]"
        x2_name = "k_m"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "k_ν(θ) = "
        result_units = ""
        x1_name = "Σk(θ_i)"
        x2_name = "n_ν"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "a = "
        result_units = ""
        x1_name = "Σa_ν*k_ν(θ)"
        x2_name = "Σk_ν(θ)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1*x2)
        result_name = "a = "
        result_units = ""
        x1_name = "a_1"
        x2_name = "a_2"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "k(φ)*f_sd,fi = "
        result_units = ""
        x1_name = "Σk_s(θ_i)*f_sd,i*A_i"
        x2_name = "ΣA_i"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "a = "
        result_units = ""
        x1_name = "Σa_i*k_s(θ_i)*f_sd,i*A_i"
        x2_name = "k_s(θ_i)*f_sd,i*A_i"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "M_u1 = "
        result_units = "kNm"
        x1_name = "A_s1 [cm2]"
        x2_name = "f_sd,fi(θ_m) [kN/cm2]"
        x3_name = "z [m]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*x4*x5)
        result_name = "ω_k = "
        result_units = ""
        x1_name = "A_s1"
        x2_name = "f_sd,fi(θ_m)"
        x3_name = "b_fi"
        x4_name = "d_fi"
        x5_name = "f_cd,fi(20)"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "M_u2 = "
        result_units = "kNm"
        x1_name = "A_s2 [cm2]"
        x2_name = "f_scd,fi(θ_m) [kN/cm2]"
        x3_name = "z' [m]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "A_s = "
        result_units = "cm2"
        x1_name = "A_s1 [cm2]"
        x2_name = "A_s2 [cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "M_u = "
        result_units = "kNm"
        x1_name = "M_u1 [kNm]"
        x2_name = "M_u2 [kNm]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - 0.2/x1)*x2/x1
        result_name = "k_c,m = "
        result_units = ""
        x1_name = "n"
        x2_name = "Σk_c(θ_i)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - x2/x3)
        result_name = "a_z = "
        result_units = ""
        x1_name = "w"
        x2_name = "k_c,m"
        x3_name = "k_c(θ_M)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - (x2/x3)**1.3)
        result_name = "a_z = "
        result_units = ""
        x1_name = "w"
        x2_name = "k_c,m"
        x3_name = "k_c(θ_M)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__B_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2*x2*x3
        result_name = "EI_z = "
        result_units = ""
        x1_name = "k_c(θ_M)"
        x2_name = "E_c"
        x3_name = "I_z"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__E_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2**2/8
        result_name = "M_Ed,fi = "
        result_units = "kNm"
        x1_name = "w_Ed,fi [kN/m]"
        x2_name = "l_eff [m]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__E_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3*x4*x5/x6
        result_name = "M_Rd,fi = "
        result_units = "kNm"
        x1_name = "γ_s"
        x2_name = "γ_s,fi"
        x3_name = "k_s(θ)"
        x4_name = "M_Ed [kNm]"
        x5_name = "A_s,prov"
        x6_name = "A_s,req"
        
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__E_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3*x4/x5*(x6 - x7)/x6
        result_name = "M_Rd,fi = "
        result_units = "kNm"
        x1_name = "γ_s"
        x2_name = "γ_s,fi"
        x3_name = "M_Ed [kNm]"
        x4_name = "A_s,prov"
        x5_name = "A_s,req"
        x6_name = "d"
        x7_name = "a"
        
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__E_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3/x4*x5
        result_name = "l_bd,fi = "
        result_units = ""
        x1_name = "γ_s"
        x2_name = "γ_s,fi"
        x3_name = "γ_c,fi"
        x4_name = "γ_c"
        x5_name = "l_bd"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "v_R,fi,d,90 = "
        result_units = ""
        x1_name = "N_R,fi,d,90"
        x2_name = "A_c"
        x3_name = "f_cd"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_3_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3*x4)
        result_name = "μ_tot,fi,d,90 = "
        result_units = ""
        x1_name = "M_tot,fi,d,90"
        x2_name = "A_c"
        x3_name = "h"
        x4_name = "f_cd"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_4_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "v_R,fi,d,90 = "
        result_units = ""
        x1_name = "k_fi"
        x2_name = "k_a"
        x3_name = "k_C"
        x4_name = "k_ρ"
        x5_name = "X_R90"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_5_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "μ_R,fi,d,90 = "
        result_units = ""
        x1_name = "k_fi"
        x2_name = "k_a"
        x3_name = "k_C"
        x4_name = "k_ρ"
        x5_name = "X_tot,90"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_6_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(x1/x2,1)*max(6 - x2/75, 0.3) + max(6 - x2/150, 0.7)
        result_name = "k_fi = "
        result_units = ""
        x1_name = "e_1"
        x2_name = "h"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_7_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(0.6 + 0.2*x1/x2, 0.8)
        result_name = "k_fi = "
        result_units = ""
        x1_name = "e_1"
        x2_name = "h"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_8_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (max(0.65*(5 - x2/150) - max(0.65*(1 - (x3/x2))*(3 - x2/150), 0), 1) \
                        - 1)/0.05*x1/x2 - 2*max(0.65*(5 - x2/150) - max(0.65*(1 - (x3/x2))*(3 - x2/150), 0), 1) + 3
        result_name = "k_a = "
        result_units = ""
        x1_name = "a"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_9_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - 0.3 + max(0.3*(x2 - 450)/350 + max((1 - x3/x2)* \
                        max(-0.1*(x2/150) + 0.4, 0), 0), max((1 - x3/x2)* \
                        max(-0.1*(x2/150) + 0.4, 0), 0)))/0.05*(x1/x2) + \
                        2*(0.3 + max(0.3*(x2 - 450)/350 + max((1 - x3/x2)* \
                        max(-0.1*(x2/150) + 0.4, 0), 0), max((1 - x3/x2)* \
                        max(-0.1*(x2/150) + 0.4, 0), 0))) - 1
        result_name = "k_a = "
        result_units = ""
        x1_name = "a"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_10_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (max(1.1 - 0.1*(x3/x2), 1) - 1)/20*x1 - 1.5*max(1.1 - 0.1*(x3/x2), 1) + 2.5
        result_name = "k_C = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_11_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - min(0.75 + 0.2*(x3/x2), 0.95))/10*x1 + 3*min(0.75 + 0.2*(x3/x2), 0.95) - 2
        result_name = "k_C = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_12_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(1 + (x1 - 2)*(x3/x2), x1/2)
        result_name = "k_ρ = "
        result_units = ""
        x1_name = "ρ"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_13_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(0.6 - 0.1*(x1 +1)*(x3/x2), x1/2)
        result_name = "k_ρ = "
        result_units = ""
        x1_name = "ρ"
        x2_name = "h"
        x3_name = "e_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_14_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1
        result_name = "v_R,fi,d,90(e1/h=3.5) = "
        result_units = ""
        x1_name = "v_R,fi,d,90(e1/h=1.5)"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_1_2__AA_15_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1
        result_name = "μ_tot,fi,d,90(e1/h=3.5) = "
        result_units = ""
        x1_name = "μ_tot,fi,d,90(e1/h=1.5)"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
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

class EN_1992_2__5_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/8
        result_name = "ΔM_Ed = "
        result_units = "kNm"
        x1_name = "F_Ed,sup [kN]"
        x2_name = "t [m]"
        na_text = "The recommended value is the breadth of the bearing. (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_10a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3/x4
        result_name = "δ = "
        result_units = ""
        x1_name = "k_1"
        x2_name = "K_2"
        x3_name = "x_u"
        x4_name = "d"
        na_text = "k_1 = 0.64, k_2 = 0.8 (Germany)\n"
        na_text += "k_1 = 0.44, k_2 = 1.25*(0.6 + 0.0014/ε_cu2) (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name, \
                                 na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_10b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3/x4
        result_name = "δ = "
        result_units = ""
        x1_name = "k_3"
        x2_name = "K_4"
        x3_name = "x_u"
        x4_name = "d"
        na_text = "k_3 = 0.54, k_4 = 1.25*(0.6 + 0.0014/ε_cu2) (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name, x4_name, \
                                 na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_11N_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1/3)
        result_name = "k_λ = "
        result_units = ""
        x1_name = "λ"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_12N_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "λ = "
        result_units = ""
        x1_name = "M_Ed [kNm]"
        x2_name = "V_Ed [kN]"
        x3_name = "d [m]"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_13b_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        16/sqrt(x1/(x2*x3))
        result_name = "λ_lim = "
        result_units = ""
        x1_name = "N_Ed [kN]"
        x2_name = "A_c [cm2]"
        x3_name = "f_cd [kN/cm]"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_41_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        exp((-x1)*x2*(x3 - 1))
        result_name = "k_μ = "
        result_units = ""
        x1_name = "μ"
        x2_name = "γ"
        x3_name = "κ"
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "θ_l = "
        result_units = ""
        x1_name = "θ_0"
        x2_name = "α_h"
        na_text = "θ_0 = 1/200 (Austria)"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_102a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*(x3*x4 + x5*x6)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x7*x8/x9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "γ_Rd"
        x2_name = "E"
        x3_name = "γ_G"
        x4_name = "G"
        x5_name = "γ_Q"
        x6_name = "Q"
        x7_name = "R"
        x8_name = "q_ud"
        x9_name = "γ_O"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_102b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x2*(x3*x4 + x5*x6)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x7*x8/(x9*x1)
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "γ_Rd"
        x2_name = "E"
        x3_name = "γ_G"
        x4_name = "G"
        x5_name = "γ_Q"
        x6_name = "Q"
        x7_name = "R"
        x8_name = "q_ud"
        x9_name = "γ_O"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__5_102c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x10*x1*x2*(x3*x4 + x5*x6)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x7*x8/x9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "γ_Rd"
        x2_name = "E"
        x3_name = "γ_G"
        x4_name = "G"
        x5_name = "γ_Q"
        x6_name = "Q"
        x7_name = "R"
        x8_name = "q_ud"
        x9_name = "γ_O"
        x10_name = "γ_Sd"
        self.widget = Calculator(10, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, x10_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_2a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*min(1 + sqrt(200/x2), 2)*(100*min(x3/(x4*x2), \
                        0.02)*x5)**(1/3) + x6*min(x7/x8, 0.2*x9))*x4*x2
                
        result_name = "V_Rd,c = "
        result_units = "N"
        x1_name = "C_Rd,c"
        x2_name = "d [mm]"
        x3_name = "A_sl [mm2]"
        x4_name = "b_w [mm]"
        x5_name = "f_ck [MPa]"
        x6_name = "k_1"
        x7_name = "N_Ed [N]"
        x8_name = "A_c [mm2]"
        x9_name = "f_cd [MPa]"
        na_text = "k_1 = 0.15, C_Rdc = 0.18/γ_C (Austria)"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_2b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2*x3)*x4*x5
                
        result_name = "V_Rd,c = "
        result_units = "N"
        x1_name = "v_min"
        x2_name = "k_1"
        x3_name = "σ_cp [MPa]"
        x4_name = "b_w [mm]"
        x5_name = "d [mm]"
        na_text = "v_min = (0.0525/γ_c)*k^(3/2)*f_ck^(1/2) (Germany)\n"
        na_text += "k_1 = 0.15, for v_min see 6_3 (Austria)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.035*x1**(3/2)*x2**0.5
        result_name = "ν_min = "
        result_units = ""
        x1_name = "k"
        x2_name = "f_ck"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_7b_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.48*x2**(1/3)*(1 - 1.2*x3/x4)*x5*x6
        result_name = "V_Rd,cc = "
        result_units = ""
        x1_name = "c"
        x2_name = "f_ck"
        x3_name = "σ_cp"
        x4_name = "f_cd"
        x5_name = "b_w"
        x6_name = "z"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3*x4/tan(x5)
        result_name = "V_Rd,s = "
        result_units = "kN"
        x1_name = "A_sw [cm2]"
        x2_name = "s [cm]"
        x3_name = "z [cm]"
        x4_name = "f_ywd [kN/cm2]"
        x5_name = "θ [rad]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5/(1/tan(x6) + tan(x6))
        result_name = "V_Rd,max = "
        result_units = "kN"
        x1_name = "α_cw"
        x2_name = "b_w [cm]"
        x3_name = "z [cm]"
        x4_name = "ν_1"
        x5_name = "f_cd [kN/cm2]"
        x6_name = "θ [rad]"
        na_text = "α_cw = 1, ν_1 = 0.75 (Germany)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_10b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.9 - x1/200
        result_name = "ν_1 = "
        result_units = ""
        x1_name = "f_ck"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_11a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + x1/x2
        result_name = "result: "
        result_units = ""
        x1_name = "σ_cp"
        x2_name = "f_cd"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_11c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2.5*(1 - x1/x2)
        result_name = "result: "
        result_units = ""
        x1_name = "σ_cp"
        x2_name = "f_cd"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2/(x3*x4)
        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x5*x6*x7
                                  
        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "A_sw,max"
        x2_name = "f_ywd"
        x3_name = "b_w"
        x4_name = "s"
        x5_name = "α_cw"
        x6_name = "ν_1"
        x7_name = "f_cd"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*(1/tan(x2) - 1/tan(x3))
        result_name = "ΔF_td = "
        result_units = "kN"
        x1_name = "V_Ed [kN]"
        x2_name = "θ [rad]"
        x3_name = "α [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "v_Ed = "
        result_units = "kN/cm2"
        x1_name = "ΔF [kN]"
        x2_name = "h_f [cm]"
        x3_name = "Δx [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_22_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2) + (x3/x4)
        result_name = "result: "
        result_units = ""
        x1_name = "v_Ed,Platte"
        x2_name = "v_Rd,max,Platte"
        x3_name = "v_Ed,Scheibe"
        x4_name = "v_Rd,max,Scheibe"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_27_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3/x4
        result_name = "V_Ed,T+V = "
        result_units = ""
        x1_name = "v_Ed,T"
        x2_name = "v_Ed"
        x3_name = "t_ef,i"
        x4_name = "b_w"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_28(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(2*x2*tan(x3))
        result_name = "ΣA_sl*f_yd/u_k = "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "A_k"
        x3_name = "θ [rad]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_29(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + x3/x4     
        result_name = "result: "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "T_Rd,max"
        x3_name = "V_Ed"
        x4_name = "V_Rd,max"
        na_text = "Für Vollquerschnitte werder die Quotients quadriert (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_30(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1*x2*x3*x4*x5*sin(x6)*cos(x6)
        result_name = "T_Rd,max = "
        result_units = ""
        x1_name = "ν"
        x2_name = "α_cw"            
        x3_name = "f_cd"
        x4_name = "A_k"
        x5_name = "t_ef,i"
        x6_name = "θ [rad]"
        na_text = "ν = 0,525 für Torsion allgemein, ν = 0,75 für Kastenquerschnitte (Germany)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_31_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/4.5
        result_name = "T_Ed = "
        result_units = ""
        x1_name = "V_Ed"
        x2_name = "b_w"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_31_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + 4.5*x2/(x1*x3))
        result_name = "V_Rd,c = "
        result_units = ""
        x1_name = "T_Ed"
        x2_name = "V_Ed"
        x3_name = "b_w"        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_33_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + x2 + 0.5*x3
        result_name = "r_cont = "
        result_units = "cm"
        x1_name = "d [cm]"
        x2_name = "l_H [cm]"
        x3_name = "c [cm]"        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_34_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + 0.56*sqrt(x2/x3)
        result_name = "r_cont = "
        result_units = ""
        x1_name = "d"
        x2_name = "l_1"
        x3_name = "l_2"        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_35_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + 0.69*x2
        result_name = "r_cont = "
        result_units = ""
        x1_name = "d"
        x2_name = "l_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_39_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + sqrt((x1*x2/x3*x4/x5)**2 + (x6*x7/x3*x4/x8)**2)
        result_name = "β = "
        result_units = ""
        x1_name = "k_y"
        x2_name = "M_Ed,y"
        x3_name = "V_Ed"
        x4_name = "u_1"
        x5_name = "W_1,y"
        x6_name = "k_z"
        x7_name = "M_Ed,z"
        x8_name = "W_1,z"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, x8_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__106_50_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*(100*x3*x4)**(1/3)*2*x5/x6
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x7*2*x5/x6
                                  
        result_name = "Right side: "
        result_units = "N/mm2"
        x1_name = "C_Rd,c"
        x2_name = "k"
        x3_name = "ρ_l"
        x4_name = "f_ck [MPa]"
        x5_name = "d [cm]"
        x6_name = "a [cm]"
        x7_name = "v_min [N/mm2]"
        na_text = "Fűr Bodenplatten und Stützenfundamente C_Rd,c = 0.15/γ_C (Germany)"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_51_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + x1*x2/x3*x4/x5
        result_name = "β = "
        result_units = ""
        x1_name = "k"
        x2_name = "M_Ed"
        x3_name = "V_Ed,red"
        x4_name = "u"
        x5_name = "W"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_52_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x3*x4
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "β"
        x2_name = "V_Ed,red"
        x3_name = "A_sw,1+2 [cm2]"
        x4_name = "f_ywd,ef [kN/cm2]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_52_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.3*x3*x4*sin(x5)
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "β"
        x2_name = "V_Ed,red"
        x3_name = "A_sw,1+2 [cm2]"
        x4_name = "f_ywd [kN/cm2]"
        x5_name = "α [rad]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_53_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.4*x1
        result_name = "v_Rd,max = "
        result_units = ""
        x1_name = "v_Rd,c,u1"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_54_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "m_Ed,z/m_Ed,y = "
        result_units = ""
        x1_name = "η_z/η_y"
        x2_name = "V_Ed"
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_59_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/4*(1 - 0.7*x1/x2)*x3
        result_name = "T = "
        result_units = ""
        x1_name = "a"
        x2_name = "H"
        x3_name = "F"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_76(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*(1 - x4/250)
        result_name = "f_cd,fat = "
        result_units = ""
        x1_name = "k_1"
        x2_name = "β_cc(t_0)"
        x3_name = "f_cd"
        x4_name = "f_ck"
        na_text = "k_1 = 0.85 (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)
        
class EN_1992_2__6_101a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "M_rep [KNcm]"
        x2_name = "z_s [cm]"
        x3_name = "f_yk [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_101b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2 + x3*x4        
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x5/x6
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "A_s,min [cm2]"
        x2_name = "f_yk [kN/cm2]"
        x3_name = "A_p [cm2]"
        x4_name = "Δσ_p [kN/cm2]"
        x5_name = "M_rep [kNcm]"
        x6_name = "z [cm]"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2 + x3*x4*x5
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x6*x7*x8*x9
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "A_s [cm2]"
        x2_name = "f_yk [kN/cm2]"
        x3_name = "k_p"
        x4_name = "A_p [cm2]"
        x5_name = "f_p0.1k [kN/cm2]"
        x6_name = "t_inf [cm]"
        x7_name = "b_0 [cm]"
        x8_name = "α_cc"
        x9_name = "f_ck [kN/cm2]"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, x9_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_103(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3*x4)*(1/tan(x5) + tan(x5))
        result_name = "h_red = "
        result_units = "cm"
        x1_name = "V_Ed [kN]"
        x2_name = "b_w [cm]"
        x3_name = "ν"
        x4_name = "f_cd [kN/cm2]"
        x5_name = "θ [rad]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_104(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)*tan(x4)
        result_name = "A_sw/s = "
        result_units = "cm2/cm"
        x1_name = "V_Ed"
        x2_name = "h_red"
        x3_name = "f_ywd"
        x4_name = "θ [rad]"
        na_text = "h_red >= 0.5*h (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_106(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        10**(14*(1 - x2/x3)/sqrt(1 - x1/x2))
        result_name = "N_i = "
        result_units = ""
        x1_name = "σ_cd,min,i"
        x2_name = "σ_cd,max,i"
        x3_name = "f_cd,fat"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_107a_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1.2 + 1.4*x1/x2)/(1 - x3/x4)
        result_name = "cot θ = "
        result_units = ""
        x1_name = "σ_cp"
        x2_name = "f_cd"
        x3_name = "V_Rd,cc"
        x4_name = "V_Ed"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_110(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.85*x1*(1 + 3.8*x2)/(1 + x2)**2
        result_name = "σ_cd,max = "
        result_units = "kN/cm2"
        x1_name = "f_cd [kN/cm2]"
        x2_name = "α"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_111(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(0.85 - x2/x3*(0.85 - x4))
        result_name = "σ_cd,max = "
        result_units = "kN/cm2"
        x1_name = "f_cd [kN/cm2]"
        x2_name = "σ_s"
        x3_name = "f_yd"
        x4_name = "ν"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__6_112(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*(1 - 0.032*abs(x3 - x4))
        result_name = "σ_cd,max = "
        result_units = "kN/cm2"
        x1_name = "ν"
        x2_name = "f_cd [kN/cm2]"
        x3_name = "θ"
        x4_name = "θ_el"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "A_s,min*σ_s = "
        result_units = ""
        x1_name = "k_c"
        x2_name = "k"
        x3_name = "f_ct,eff"
        x4_name = "A_ct"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.4*(1 - x1/(x2*x3/x4*x5))
        result_name = "k_c = "
        result_units = ""
        x1_name = "σ_c"
        x2_name = "k_1"
        x3_name = "h"
        x4_name = "h*"
        x5_name = "f_ct,eff"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.9*x1/(x2*x3)
        result_name = "k_c = "
        result_units = ""
        x1_name = "F_cr"
        x2_name = "A_ct"
        x3_name = "f_ct,eff"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(x2*x3)
        result_name = "σ_c = "
        result_units = "kN/cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "b [cm]"
        x3_name = "h [cm]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_5_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2/x3       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x4*x1*x5/x6
                                  
        result_name = "Right side: "
        result_units = "cm2"
        x1_name = "f_ct,eff [kN/cm2]"
        x2_name = "A_c,eff [cm2]"
        x3_name = "σ_s [kN/cm2]"
        x4_name = "k"
        x5_name = "A_ct [cm2]"
        x6_name = "f_yk [kN/cm2]"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name ,non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_5_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/2.9
        result_name = "Φ = "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "f_ct,eff"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_5_3_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 0.4*x2*(1/x3 - 1/x4)
        result_name = "σ_s = "
        result_units = "kN/cm2"
        x1_name = "σ_s2 [kN/cm2]"
        x2_name = "f_ct,eff [kN/cm2]"
        x3_name = "ρ_p,eff"
        x4_name = "ρ_tot"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_5_4_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2)/x3
        result_name = "ρ_tot = "
        result_units = ""
        x1_name = "A_s"
        x2_name = "A_p"
        x3_name = "A_c,eff"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_6_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*x4/(4*(x5 - x6))*x7/2.9       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x7/2.9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "k_c"
        x3_name = "k"
        x4_name = "h_cr"
        x5_name = "h"
        x6_name = "d"
        x7_name = "f_ct,eff"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_7_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*x4/(8*(x5 - x6))*x7/2.9       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x7/2.9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "k_c"
        x3_name = "k"
        x4_name = "h_cr"
        x5_name = "h"
        x6_name = "d"
        x7_name = "f_ct,eff"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_7_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3/(4*(x4 - x5)*x6*2.9)       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x7/2.9
                                  
        result_name = "Right side: "
        result_units = ""
        x1_name = "Φ*_s"
        x2_name = "σ_s"
        x3_name = "A_s"
        x4_name = "h"
        x5_name = "d"
        x6_name = "b"
        x7_name = "f_ct,eff"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_8_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "w_k = "
        result_units = ""
        x1_name = "s_r,max"
        x2_name = "ε_sm"
        x3_name = "ε_cm"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_105_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        180*sin(2*pi*x1*x2)
        result_name = "F = "
        result_units = "N"
        x1_name = "f_0"
        x2_name = "t (sec)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__7_105_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.9*x1
        result_name = "v = "
        result_units = ""
        x1_name = "f_0"
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__8_19_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + 0.5*x3*(1/tan(x4) - 1/tan(x5))
        result_name = "F_Ed(x) = "
        result_units = ""
        x1_name = "M_Ed(x)"
        x2_name = "z"
        x3_name = "V_Ed(x)"
        x4_name = "θ [rad]"
        x5_name = "α [rad]"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__8_21_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3*(x4 - x5)/x6
        result_name = "l_bpd = "
        result_units = ""
        x1_name = "l_r"
        x2_name = "α_2"
        x3_name = "Φ"
        x4_name = "σ_pd"
        x5_name = "σ_pt(x=l_r)"
        x6_name = "f_bpd"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_3_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         abs(x1)*x2/x3 + x4       
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x5/2

        result_name = "Right side: "
        result_units = "kN"
        x1_name = "V_Ed [kN]"
        x2_name = "a_l"
        x3_name = "z"
        x4_name = "N_Ed"
        x5_name = "V_Ed"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_5a_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.16*x1/x2
        result_name = "ρ_w,min = "
        result_units = ""
        x1_name = "f_ctm"
        x2_name = "f_yk"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_5b_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.256*x1/x2
        result_name = "ρ_w,min = "
        result_units = ""
        x1_name = "f_ctm"
        x2_name = "f_yk"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_7_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*(1 + 1/tan(x2))
        result_name = "s_b,max = "
        result_units = "cm"
        x1_name = "h [cm]"
        x2_name = "α [rad]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_11_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.08/1.5*sqrt(x1)/x2*x3*x4
        result_name = "A_sw,min = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "f_yk"
        x3_name = "s_r"
        x4_name = "s_t"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__9_12_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.15*abs(x1)/x2
        result_name = "A_s,min = "
        result_units = "cm2"
        x1_name = "N_Ed [kN]"
        x2_name = "f_yd [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_113(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "result: "
        result_units = ""
        x1_name = "f_cm(t)"
        x2_name = "f_ck"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, \
                                 result_units, x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_114(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 - 20)*(2.2*x2/x1 - 0.2)*10**(-6)
        result_name = "ε_ca(t) = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "f_cm(t)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_115(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 - 20)*(2.8 - 1.1*exp((-x2)/96))*10**(-6)
        result_name = "ε_ca(t) = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "t"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_116(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(72*exp((-0.046)*x2) + 75 - x3)*(x4 - x5)*10**(-6)/(x4 - x5 + x6*x7**2)
        result_name = "ε_cd(t) = "
        result_units = ""
        x1_name = "K(f_ck)"
        x2_name = "f_ck"
        x3_name = "RH"
        x4_name = "t"
        x5_name = "t_s"
        x6_name = "β_cd"
        x7_name = "h_0"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_117(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*(x3 + x4)
        result_name = "ε_cc(t,t_0) = "
        result_units = ""
        x1_name = "σ(t0)"
        x2_name = "E_c"
        x3_name = "φ_b(t,t_0)"
        x4_name = "φ_d(t,t_0)"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_118(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*sqrt(x2 - x3)/(sqrt(x2 - x3) + x4)
        result_name = "φ_b(t,t_0) = "
        result_units = ""
        x1_name = "φ_b0"
        x2_name = "t"
        x3_name = "t_0"
        x4_name = "β_bc"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_119(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        3.6/x1**0.37
        result_name = "φ_b0 = "
        result_units = ""
        x1_name = "f_cm(t0)"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, \
                                 result_units, x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_120(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         0.37*exp(2.8*x1/x2)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.4*exp(3.1*x1/x2)
                                  
        result_name = "Silica: "
        result_units = ""
        x1_name = "f_cm(t_0)"
        x2_name = "f_ck"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, \
                                 non_equation = True, \
                                 result_name2 = "Non silica: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_121(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "φ_d(t,t_0) = "
        result_units = ""
        x1_name = "φ_d0"
        x2_name = "ε_cd(t)"
        x3_name = "ε_cd(t0)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_122(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - 20)*(2.2*x3/x2 - 0.2)*10**(-6)
        result_name = "ε_ca(t) = "
        result_units = ""
        x1_name = "β_ca1"
        x2_name = "f_ck"
        x3_name = "f_cm(t)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_123(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x2*(x1 - 20)*(x3 - x4*exp((-x6)/x5))*10**(-6)
        result_name = "ε_ca(t) = "
        result_units = ""
        x1_name = "f_ck"
        x2_name = "β_ca1"
        x3_name = "β_ca2"
        x4_name = "β_ca3"
        x5_name = "β_ca4"
        x6_name = "t"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_124(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*(72*exp((-0.046)*x3) + 75 - x4)*(x5 - x6)*10**(-6)/(x5 - x6 + x7*x8**2)
        result_name = "ε_cd(t) = "
        result_units = ""
        x1_name = "β_cd1"
        x2_name = "K(f_ck)"
        x3_name = "f_ck"
        x4_name = "RH"
        x5_name = "t"
        x6_name = "t_s"
        x7_name = "β_cd2"
        x8_name = "h_0"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_125(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*sqrt(x3 - x4)/(sqrt(x3 - x4) + x5)
        result_name = "φ_b(t,t0,f_ck,f_cm(t0)) = "
        result_units = ""
        x1_name = "β_cd1"
        x2_name = "φ_b0"
        x3_name = "t"
        x4_name = "t_0"
        x5_name = "β_bc"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_126(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*exp(2.8*x2/x3)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*exp(3.1*x2/x3)
                                  
        result_name = "Silica: "
        result_units = ""
        x1_name = "β_bc2"
        x2_name = "f_cm(t_0)"
        x3_name = "f_ck"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Non silica: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_127(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "φ_d(t) = "
        result_units = ""
        x1_name = "φ_d0"
        x2_name = "ε_cd(t)"
        x3_name = "ε_cd(t0)"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__B_128(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 0.1*log10(x1/x2)
        result_name = "γ_h = "
        result_units = ""
        x1_name = "t"
        x2_name = "t_ref"
        na_text = "Für Kriechen gilt γlt=1.0 (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1) - x2
        result_name = "f'_tdx = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1) - x2
        result_name = "f'_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edy [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*abs(x1)
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2/x2 - x3
        result_name = "f'_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        x3_name = "σ_Edy [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 + (x2/x1)**2)
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)/tan(x3) - x2
        result_name = "f'_tdx = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edx [kN/cm2]"
        x3_name = "θ [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)*tan(x3) - x2
        result_name = "f'_tdy = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "σ_Edy [kN/cm2]"
        x3_name = "θ [rad]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__F_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        abs(x1)*(1/tan(x2) + tan(x2))
        result_name = "σ_cd = "
        result_units = "kN/cm2"
        x1_name = "τ_Edxy [kN/cm2]"
        x2_name = "θ [rad]"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__J_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1/(x2*x3)         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.6*x3

        result_name = "Right side: "
        result_units = "kN/cm2"
        x1_name = "P_max [kN]"
        x2_name = "c [cm]"
        x3_name = "c' [cm]"
        x4_name = "f_ck(t)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__J_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.15*x1/x2*x3
        result_name = " = "
        result_units = "cm2"
        x1_name = "P_max [kN]"
        x2_name = "f_yd [kN/cm2]"
        x3_name = "γ_p,unfav"

        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2 + x3*x1/x4 + x5 + x6
        result_name = "ε_c(t) = "
        result_units = ""
        x1_name = "σ_0"
        x2_name = "E_c(t0)"            
        x3_name = "φ(t,t0)"
        x4_name = "E_c(28)"
        x5_name = "Σ(1/Ec(ti)+φ(t,ti)/Ec(28))"
        x6_name = "ε_cs(t,t_s)"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "dε_∞cc(t)/dt = "
        result_units = ""
        x1_name = "dσ/dt"
        x2_name = "φ(∞,t)"
        x3_name = "E_c"

        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_103(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "ε_cc(t) = "
        result_units = ""
        x1_name = "ε_∞cc(t)"
        x2_name = "β_c(t,t_e)"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_104(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "dε_cc(t)/dt = "
        result_units = ""
        x1_name = "ε_∞cc(t)"
        x2_name = "∂β_c(t,t_e)/∂t"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_105(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 - x2)*x3
        result_name = "ε_ccMax(t) - ε_cc(t) = "
        result_units = ""
        x1_name = "ε_ccMax(t)"
        x2_name = "ε_∞cc(t)"
        x3_name = "β_c(t,t_e)"

        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_106(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 - x2)*x3
        result_name = "d(ε_ccMax(t) - ε_cc(t))/dt = "
        result_units = ""
        x1_name = "ε_ccMax(t)"
        x2_name = "ε_cc(t)"
        x3_name = "∂β_c(t,t_e)/∂t"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_111(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2*x3
        result_name = "S_2(t) = "
        result_units = ""
        x1_name = "S_el,1"
        x2_name = "ξ(t,t0,t1)"
        x3_name = "ΔS_el,1"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_114(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "S_j+1 = "
        result_units = ""
        x1_name = "S_el,1"
        x2_name = "Σξ(t,t0,ti)*ΔS_el,i"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__KK_119(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + (x2 - x1)*x3/x4*(x5 - x6)/(1 + x7*x8)
        result_name = "S_∞ = "
        result_units = ""
        x1_name = "S_0"
        x2_name = "S_1"
        x3_name = "E_c(t1)"
        x4_name = "E_c(t0)"
        x5_name = "φ(∞,t0)"
        x6_name = "φ(t1,t0)"
        x7_name = "χ"
        x8_name = "φ(∞,t1)"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3**2 + x4*sqrt(x2)/x3 + x5*x6/x3 - 1
        result_name = "Φ = "
        result_units = ""
        x1_name = "α"
        x2_name = "J_2"            
        x3_name = "f_cm"
        x4_name = "λ"
        x5_name = "β"
        x6_name = "I_1"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        ((x1 - x2)**2 + (x2 - x3)**2 + (x3 - x1)**2)/6
        result_name = "J_2 = "
        result_units = "kN2/cm4"
        x1_name = "σ_1 [kN/cm2]"
        x2_name = "σ_2 [kN/cm2]"
        x3_name = "σ_3 [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_103(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 - x4)*(x2 - x4)*(x3 - x4)
        result_name = "J_3 = "
        result_units = "kN/cm2"
        x1_name = "σ_1 [kN/cm2]"
        x2_name = "σ_2 [kN/cm2]"
        x3_name = "σ_3 [kN/cm2]"
        x4_name = "σ_m [kN/cm2]"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_104(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 + x3
        result_name = "I_1 = "
        result_units = "kN/cm2"
        x1_name = "σ_1 [kN/cm2]"
        x2_name = "σ_2 [kN/cm2]"
        x3_name = "σ_3 [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_105(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1 + x2 + x3)/3
        result_name = "σ_m = "
        result_units = "kN/cm2"
        x1_name = "σ_1 [kN/cm2]"
        x2_name = "σ_2 [kN/cm2]"
        x3_name = "σ_3 [kN/cm2]"
        
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_106(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(9*x1**1.4)
        result_name = "α = "
        result_units = ""
        x1_name = "k"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_107(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*cos(1/3*acos(x2*cos(3*x3)))
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*cos(pi/3 - 1/3*acos((-x2)*cos(3*x3)))
        result_name = "cos(3θ) < 0: "
        result_units = ""
        x1_name = "C_1"
        x2_name = "C_2"
        x3_name = "θ [rad]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "cos(3θ) >= 0: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_108(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(3.7*x1**1.1)
        result_name = "β = "
        result_units = ""
        x1_name = "k"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_109(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        3*sqrt(3)/2*x1/x2**(3/2)
        result_name = "cos(3*θ) = "
        result_units = ""
        x1_name = "J_3"
        x2_name = "J_2"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_110(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(0.7*x1**0.9)
        result_name = "c_1 = "
        result_units = ""
        x1_name = "k"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_111(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 - 6.8*(x1 - 0.07)**2
        result_name = "c_2 = "
        result_units = ""
        x1_name = "k"
        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_112(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "k = "
        result_units = ""
        x1_name = "f_ctm"
        x2_name = "f_cm"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_113(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edxs = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z_x"
        x3_name = "y_xs"
        x4_name = "m_Edx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_114(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edxi = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z_x"
        x3_name = "y_xi"
        x4_name = "m_Edx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_115(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edys = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z_y"
        x3_name = "y_ys"
        x4_name = "m_Edy"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_116(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edyi = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z_y"
        x3_name = "y_yi"
        x4_name = "m_Edy"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_117(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edyxs = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z_yx"
        x3_name = "y_yxs"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_118(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edyx = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z_yx"
        x3_name = "y_yxi"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_119(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edyxs = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z_yx"
        x3_name = "y_yxs"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_120(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edyxi = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z_yx"
        x3_name = "y_yxi"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_121(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1**2 + x2**2)
        result_name = "v_Ed0 = "
        result_units = ""
        x1_name = "v_Edx"
        x2_name = "v_Edy"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_122(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "tan(φ_0) = "
        result_units = ""
        x1_name = "v_Edy"
        x2_name = "v_Edx"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_123(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*cos(x3)**2 + x2*sin(x3)**2
        result_name = "ρ = "
        result_units = ""
        x1_name = "ρ_x"
        x2_name = "ρ_y"
        x3_name = "φ_0 [rad]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_124(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2/(x2*tan(x3))
        result_name = "n_Edyc = "
        result_units = ""
        x1_name = "v_Edy"
        x2_name = "v_Ed0"
        x3_name = "θ [rad]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_125(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*tan(x4))
        result_name = "n_Edyxc = "
        result_units = ""
        x1_name = "v_Edx"
        x2_name = "v_Edy"
        x3_name = "v_Ed0"
        x4_name = "θ [rad]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_126(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1**2/(x2*tan(x3))
        result_name = "n_Edxc = "
        result_units = ""
        x1_name = "v_Edx"
        x2_name = "v_Ed0"
        x3_name = "θ [rad]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_127(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3*tan(x4))
        result_name = "n_Edyxc = "
        result_units = ""
        x1_name = "v_Edx"
        x2_name = "v_Edy"
        x3_name = "v_Ed0"
        x4_name = "θ [rad]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_137(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edxs = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_138(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edxi = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_139(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edys = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edy"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_140(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edyi = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edy"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_141(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2
        result_name = "n_Edyxs = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_142(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2
        result_name = "n_Edyxi = "
        result_units = ""
        x1_name = "n_Edyx"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edyx"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_143(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2 + 0.5*x5**2/(x6*tan(x7))
        result_name = "n_Edxs = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edx"
        x5_name = "ν_Edx"
        x6_name = "ν_Ed0"
        x7_name = "θ [rad]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_144(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2 + 0.5*x5**2/(x6*tan(x7))
        result_name = "n_Edxi = "
        result_units = ""
        x1_name = "n_Edx"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edx"
        x5_name = "ν_Edx"
        x6_name = "ν_Ed0"
        x7_name = "θ [rad]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_145(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2 + 0.5*x5**2/(x6*tan(x7))
        result_name = "n_Edys = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edy"
        x5_name = "ν_Edy"
        x6_name = "ν_Ed0"
        x7_name = "θ [rad]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_146(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2 + 0.5*x5**2/(x6*tan(x7))
        result_name = "n_Edyi = "
        result_units = ""
        x1_name = "n_Edy"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edy"
        x5_name = "ν_Edy"
        x6_name = "ν_Ed0"
        x7_name = "θ [rad]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_147(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 - x4/x2 + 0.5*x5*x6/(x7*tan(x8))
        result_name = "n_Edxys = "
        result_units = ""
        x1_name = "n_Edxy"
        x2_name = "z"
        x3_name = "y_s"
        x4_name = "m_Edxy"
        x5_name = "ν_Edx"
        x6_name = "ν_Edy"
        x7_name = "ν_Ed0"
        x8_name = "θ [rad]"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_148(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/x2 + x4/x2 + 0.5*x5*x6/(x7*tan(x8))
        result_name = "n_Edxyi = "
        result_units = ""
        x1_name = "n_Edxy"
        x2_name = "z"
        x3_name = "y_i"
        x4_name = "m_Edxy"
        x5_name = "ν_Edx"
        x6_name = "ν_Edy"
        x7_name = "ν_Ed0"
        x8_name = "θ [rad]"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_149(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*(x2 - x3/2 - x4) + x5*(x6/2 - x4))/(x2 - x4 -x7)
        result_name = "n*_Eds = "
        result_units = ""
        x1_name = "n_Eds"
        x2_name = "h"
        x3_name = "t_s"
        x4_name = "b'_i"
        x5_name = "n_Edi"
        x6_name = "t_i"
        x7_name = "b'_s"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__LL_150(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2 - x3
        result_name = "n*_Edi = "
        result_units = ""
        x1_name = "n_Eds"
        x2_name = "n_Edi"
        x3_name = "n*_Eds"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/((2*x2 - x4 - x3)*x4)
        result_name = "τ_Ed1 = "
        result_units = ""
        x1_name = "v_Ed"
        x2_name = "b_w"
        x3_name = "z_2"
        x4_name = "z_1"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/((2*x2 - x4 - x3)*x4)
        result_name = "τ_Ed1 = "
        result_units = ""
        x1_name = "v_Ed"
        x2_name = "b_w"
        x3_name = "z_1"
        x4_name = "z_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_103(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/((x2 - (x4 + x3)/2)*x3)
        result_name = "σ_Edy1 = "
        result_units = ""
        x1_name = "m_Edx"
        x2_name = "b_w"
        x3_name = "z_1"
        x4_name = "z_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_104(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/((x2 - (x4 + x3)/2)*x4)
        result_name = "σ_Edy2 = "
        result_units = ""
        x1_name = "m_Edx"
        x2_name = "b_w"
        x3_name = "z_1"
        x4_name = "z_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_105(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x4)/((2*x2 - x4 - x3)*x3)
        result_name = "σ_Edx1 = "
        result_units = ""
        x1_name = "p_d"
        x2_name = "b_w"
        x3_name = "z_1"
        x4_name = "z_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__MM_106(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)/((2*x2 - x4 - x3)*x4)
        result_name = "σ_Edx2 = "
        result_units = ""
        x1_name = "p_d"
        x2_name = "b_w"
        x3_name = "z_1"
        x4_name = "z_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_1_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "Δσ_s,equ = "
        result_units = "kN/cm2"
        x1_name = "Δσ_s [kN/cm2]"
        x2_name = "λ_s"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_2_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "λ_s = "
        result_units = ""
        x1_name = "φ_fat"
        x2_name = "λ_s,1"
        x3_name = "λ_s,2"
        x4_name = "λ_s,3"
        x5_name = "λ_s,4"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_3_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x3/2)**(1/x2)
        result_name = "λ_s,2 = "
        result_units = ""
        x1_name = "Q"
        x2_name = "k_2"
        x3_name = "N_obs"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_4_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/100)**(1/x1)
        result_name = "λ_s,3 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "N_Years"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_5_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/x3)**(1/x1)
        result_name = "λ_s,4 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "ΣN_obs,i"
        x3_name = "N_obs,1"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_6_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "Δσ_s,equ = "
        result_units = "kN/cm2"
        x1_name = "λ_s"
        x2_name = "Δσ_s,71 [kN/cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_7_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "λ_s = "
        result_units = ""
        x1_name = "λ_s,1"
        x2_name = "λ_s,2"
        x3_name = "λ_s,3"
        x4_name = "λ_s,4"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_8_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/(25*10**6))**(1/x1)
        result_name = "λ_s,2 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "Vol"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_9_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/100)**(1/x1)
        result_name = "λ_s,3 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "N_Years"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_10_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2 + (1 - x2)*x3**x1 + (1 - x2)*x4**x1)**(1/x1)
        result_name = "λ_s,4 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "n"
        x3_name = "s_1"
        x4_name = "s_2"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_11_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + (x2 - x1)*(log10(x3) - 0.3)
        result_name = "λ_s,1(L) = "
        result_units = ""
        x1_name = "λ_s,1(2)"
        x2_name = "λ_s,1(20)"
        x3_name = "L"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_12_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x3*x2/(x4*x5*x6/x7*(1 - x6/250)) + 0.43*sqrt(1 - x1/x2)
                
        result_name = "result: "
        result_units = ""
        x1_name = "min|σ_cd,equ|"
        x2_name = "max|σ_cd,equ|"
        x3_name = "γ_Ed,fat"
        x4_name = "β_cc(t0)"
        x5_name = "α_cc"
        x6_name = "f_ck [N/mm2]"
        x7_name = "γ_c,fat"
        self.widget = Calculator(9, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_14_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "λ_c = "
        result_units = ""
        x1_name = "λ_c,0"
        x2_name = "λ_c,1"
        x3_name = "λ_c,2"
        x4_name = "λ_c,3"
        x5_name = "λ_c,4"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_15_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.94 + 0.2*abs(x1)/x2
        result_name = "λ_c,0 = "
        result_units = ""
        x1_name = "σ_c,perm"
        x2_name = "f_cd,fat"
        
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_16_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 1/8*log10(x1/(25*10**6))
        result_name = "λ_c,2 = "
        result_units = ""
        x1_name = "Vol"        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_17_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 1/8*log10(x1/100)
        result_name = "λ_c,3 = "
        result_units = ""
        x1_name = "N_Years"        
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_18_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         1 + 1/8*log10(x1/x2)
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x3,x4)/x5

        result_name = "Right side: "
        result_units = ""
        x1_name = "N_c"
        x2_name = "N_T"
        x3_name = "Δσ_c,1"
        x4_name = "Δσ_c,2"
        x5_name = "Δσ_c,1+2"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 non_equation = True, \
                                 result_name2 = "λ_c,4 = ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_19_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + (x2 - x1)*log10(x3/x4)/log10(x5/x4)
        result_name = "λ_c,1(L) = "
        result_units = ""
        x1_name = "λ_c,1(L_L)"
        x2_name = "λ_c,1(L_U)"
        x3_name = "L"
        x4_name = "L_L"
        x5_name = "L_U"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_20_DE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        14*(1 - x3*x4*x5/x6*(1 - x5/250))/sqrt(1 - x1/x2)
        result_name = "log(N) = "
        result_units = ""
        x1_name = "min|σ_cd|"
        x2_name = "max|σ_cd|"
        x3_name = "β_cc(t0)*"
        x4_name = "α_cc"
        x5_name = "f_ck [N/mm2]"
        x6_name = "γ_c,fat"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "Δσ_s,equ = "
        result_units = "kN/cm2"
        x1_name = "Δσ_s,Ec [kN/cm2]"
        x2_name = "λ_s"
        na_text = "Gleichung verändert sich. Sehe NN_1_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_102(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "λ_s = "
        result_units = ""
        x1_name = "φ_fat"
        x2_name = "λ_s,1"
        x3_name = "λ_s,2"
        x4_name = "λ_s,3"
        x5_name = "λ_s,4"
        na_text = "Gleichung verändert sich. Sehe NN_2_DE (Germany)"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_103(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x3/2)**(1/x2)
        result_name = "λ_s,2 = "
        result_units = ""
        x1_name = "Q"
        x2_name = "k_2"
        x3_name = "N_obs"
        na_text = "Gleichung verändert sich. Sehe NN_3_DE (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_104(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/100)**(1/x1)
        result_name = "λ_s,3 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "N_Years"
        na_text = "Gleichung verändert sich. Sehe NN_4_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_105(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/x3)**(1/x1)
        result_name = "λ_s,4 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "ΣN_obs,i"
        x3_name = "N_obs,1"
        na_text = "Gleichung verändert sich. Sehe NN_5_DE (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_106(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "Δσ_s,equ = "
        result_units = "kN/cm2"
        x1_name = "λ_s"
        x2_name = "Φ"
        x3_name = "Δσ_s,71 [kN/cm2]"
        na_text = "Gleichung verändert sich. Sehe NN_6_DE (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_107(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "λ_s = "
        result_units = ""
        x1_name = "λ_s,1"
        x2_name = "λ_s,2"
        x3_name = "λ_s,3"
        x4_name = "λ_s,4"
        na_text = "Gleichung verändert sich. Sehe NN_7_DE (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_108(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + (x2 - x1)*(log10(x3) - 0.3)
        result_name = "λ_s1(L) = "
        result_units = ""
        x1_name = "λ_s1(2m)"
        x2_name = "λ_s1(20m)"
        x3_name = "L [m]"
        na_text = "Gleichung verändert sich. Sehe NN_11_DE (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_109(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/(25*10**6))**(1/x1)
        result_name = "λ_s,2 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "Vol"
        na_text = "Gleichung verändert sich. Sehe NN_8_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_110(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2/100)**(1/x1)
        result_name = "λ_s,3 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "N_Years"
        na_text = "Gleichung verändert sich. Sehe NN_9_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_111(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x2 + (1 - x2)*x3**x1 + (1 - x2)*x4**x1)**(1/x1)
        result_name = "λ_s,4 = "
        result_units = ""
        x1_name = "k_2"
        x2_name = "n"
        x3_name = "s_1"
        x4_name = "s_2"
        na_text = "Gleichung verändert sich. Sehe NN_10_DE (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_112(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        14*(1 - x1*x3/x4)/sqrt(1 - x2/x3)
        result_name = "Result: "
        result_units = ""
        x1_name = "γ_sd"
        x2_name = "σ_cd,min,equ"
        x3_name = "σ_cd,max,equ"
        x4_name = "f_cd,fat"
        na_text = "Gleichung verändert sich. Sehe NN_12_DE (Germany)\n"
        na_text += "γ_sd = 1 (Austria)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_113(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1 + x2*(x3 - x1)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - x2*(x1 - x4)
                                  
        result_name = "σ_cd,min,equ = "
        result_units = ""
        x1_name = "σ_c,perm"
        x2_name = "λ_c"
        x3_name = "σ_c,max,71"
        x4_name = "σ_c,min,71"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "σ_cd,max,equ = ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_114(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "λ_c = "
        result_units = ""
        x1_name = "λ_c,0"
        x2_name = "λ_c,1"
        x3_name = "λ_c,2,3"
        x4_name = "λ_c,4"
        na_text = "Gleichung verändert sich. Sehe NN_14_DE (Germany)"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_115(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.94 + 0.2*x1/x2
        result_name = "λ_c,0 = "
        result_units = ""
        x1_name = "σ_c,perm"
        x2_name = "f_cd,fat"
        na_text = "Gleichung verändert sich. Sehe NN_15_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_116(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 1/8*log10(x1/(25*10**6)) + 1/8*log10(x2/100)
        result_name = "λ_c,2,3 = "
        result_units = ""
        x1_name = "Vol"
        x2_name = "N_Years"
        na_text = "Gleichung verändert sich. Sehe NN_16_DE (Germany)"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_117(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1 + 1/8*log10(x1)
        result_name = "λ_c,4 = "
        result_units = ""
        x1_name = "n"
        na_text = "Gleichung verändert sich. Sehe NN_17_DE (Germany)"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__NN_118(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1,x2)/x3
        result_name = "a = "
        result_units = ""
        x1_name = "σ_c1"
        x2_name = "σ_c2"
        x3_name = "σ_c1+2"
        na_text = "Gleichung verändert sich. Sehe NN_18_DE (Germany)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, na_text = na_text)
        self.setCentralWidget(self.widget)

class EN_1992_2__QQ_101(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - 0.8*x1/x2)*x3
        result_name = "f_ctb = "
        result_units = "kN/cm2"
        x1_name = "σ_3 [kN/cm2]"
        x2_name = "f_ck [kN/cm2]"
        x3_name = "f_ctk;0.05 [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
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

class EN_1992_3__K_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3/x4
        result_name = "ε_Tr = "
        result_units = ""
        x1_name = "κ"
        x2_name = "σ_c"
        x3_name = "ε_Th"
        x4_name = "f_cm"
        
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_3__L_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - x1)*x2 + (1 - x3)*(1/x4)*(x5 - x6)
        result_name = "ε_az = "
        result_units = ""
        x1_name = "R_ax"
        x2_name = "ε_iav"
        x3_name = "R_m"
        x4_name = "r"
        x5_name = "z"
        x6_name = "ẑ"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_3__L_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(x2 - x3)
        result_name = "σ_z = "
        result_units = "kN/cm2"
        x1_name = "E_c,eff [kN/cm2]"
        x2_name = "ε_iz"
        x3_name = "ε_az"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_3__M_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1*x2*x3*x4*(1 + 1/(x1*x5))/x6
        result_name = "ε_sm - ε_cm = "
        result_units = ""
        x1_name = "α_e"
        x2_name = "k_c"
        x3_name = "k"
        x4_name = "f_ct,eff"
        x5_name = "ρ"
        x6_name = "E_s"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_3__M_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3/x4
        result_name = "σ_s = "
        result_units = ""
        x1_name = "k_c"
        x2_name = "k"
        x3_name = "f_ct,eff"
        x4_name = "ρ"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_3__M_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "ε_sm - ε_cm = "
        result_units = ""
        x1_name = "R_ax"
        x2_name = "ε_free"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__4_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "R_d = "
        result_units = ""
        x1_name = "R_k"
        x2_name = "γ_M"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__4_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + x2
        result_name = "σ_adm = "
        result_units = "kN/cm2"
        x1_name = "σ_L"
        x2_name = "σ_R"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "M_Ed = "
        result_units = "kNm"
        x1_name = "V_Ed [kN]"
        x2_name = "l_a [m]"
        x3_name = "α_M"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.5 + x2
        result_name = "l_a = "
        result_units = ""
        x1_name = "d_nom"
        x2_name = "e_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "N_Ed,i = "
        result_units = ""
        x1_name = "k"
        x2_name = "A'_i"
        x3_name = "N_Ed"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/x1
        result_name = "k = "
        result_units = ""
        x1_name = "ΣA'_i"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        13*x1**0.05*x2**0.5
        result_name = "l_i = "
        result_units = ""
        x1_name = "l_y"
        x2_name = "s"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__6_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2 + 1)*x3
        result_name = "N_Ed,re = "
        result_units = ""
        x1_name = "e_s"
        x2_name = "z"
        x3_name = "V_Ed"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*x4*x5*x6*x7
        result_name = "N_Rk,c = "
        result_units = "kN"
        x1_name = "N0_Rk,c [kN]"
        x2_name = "A_c,N"
        x3_name = "A0_c,N"
        x4_name = "ψ_s,N"
        x5_name = "ψ_re,N"
        x6_name = "ψ_ec,N"
        x7_name = "ψ_M,N"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*sqrt(x2)*x3**1.5
        result_name = "N0_Rk,c = "
        result_units = ""
        x1_name = "k_1"
        x2_name = "f_ck"
        x3_name = "h_ef"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "A0_c,N = "
        result_units = "cm2"
        x1_name = "s_cr,N [cm]"
        x2_name = "c_cr,N [cm]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7 + 0.3*x1/x2
        result_name = "ψ_s,N = "
        result_units = ""
        x1_name = "c"
        x2_name = "c_cr,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5 + x1/200
        result_name = "ψ_re,N = "
        result_units = ""
        x1_name = "h_ef"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(1 + 2*x1/x2)
        result_name = "ψ_ec,N = "
        result_units = ""
        x1_name = "e_N"
        x2_name = "s_cr,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2 - x1/(1.5*x2)
        result_name = "ψ_M,N = "
        result_units = ""
        x1_name = "z"
        x2_name = "h_ef"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3
        result_name = "h'_ef = "
        result_units = ""
        x1_name = "c_max"
        x2_name = "c_cr,N"
        x3_name = "h_ef"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1/x2*x3, x4/x5*x3)
        result_name = "h'_ef = "
        result_units = "cm"
        x1_name = "c_max [cm]"
        x2_name = "c_cr,N [cm]"
        x3_name = "h_ef [cm]"
        x4_name = "s_max [cm]"
        x5_name = "s_cr,N [cm]"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "s'_cr,N = "
        result_units = "cm"
        x1_name = "s_cr,N [cm]"
        x2_name = "h'_ef [cm]"
        x3_name = "h_ef [cm]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "N_Rk,p = "
        result_units = "kN"
        x1_name = "k_2"
        x2_name = "A_h [cm2]"
        x3_name = "f_ck [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_12(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        pi/4*(x1**2 - x2**2)
        result_name = "A_h = "
        result_units = "cm2"
        x1_name = "d_h [cm]"
        x2_name = "d_a [cm]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_13(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*x4*x5*x6*x7
        result_name = "N_Rk,p = "
        result_units = "kN"
        x1_name = "N0_Rk,p [kN]"
        x2_name = "A_p,N"
        x3_name = "A0_p,N"
        x4_name = "ψ_g,Np"
        x5_name = "ψ_s,Np"
        x6_name = "ψ_re,N"
        x7_name = "ψ_ec,Np"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_14(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*pi
        result_name = "N0_Rk,p = "
        result_units = "kN"
        x1_name = "ψ_sus"
        x2_name = "τ_Rk [kN/cm2]"
        x3_name = "d [cm]"
        x4_name = "h_ef [cm]"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_14ab(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 + 1 - x2
        result_name = "ψ_sus = "
        result_units = ""
        x1_name = "ψ0_sus"
        x2_name = "α_sus"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_15(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        7.3*x1*(x2*x3)**0.5
        result_name = "s_cr,Np = "
        result_units = ""
        x1_name = "d"
        x2_name = "ψ_sus"
        x3_name = "τ_Rk"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_16(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/2
        result_name = "c_cr,Np = "
        result_units = "cm"
        x1_name = "s_cr,Np [cm]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_17(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1 - (x2/x3)**0.5*(x1 - 1)
        result_name = "ψ_g,Np = "
        result_units = ""
        x1_name = "ψ0_g,Np"
        x2_name = "s"
        x3_name = "s_cr,Np"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_18(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1) - (sqrt(x1) - 1)*(x2/x3)**1.5
        result_name = "ψ_g,Np = "
        result_units = ""
        x1_name = "n"
        x2_name = "τ_Rk"
        x3_name = "τ_Rk,c"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_19(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/(pi*x2)*sqrt(x3*x4)
        result_name = "τ_Rk,c = "
        result_units = ""
        x1_name = "k_3"
        x2_name = "d"
        x3_name = "h_ef"
        x4_name = "f_ck"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_20(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7 + 0.3*x1/x2
        result_name = "ψ_s,Np = "
        result_units = ""
        x1_name = "c"
        x2_name = "c_cr,Np"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_21(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(1 + 2*x1/x2)
        result_name = "ψ_ec,Np = "
        result_units = ""
        x1_name = "e_N"
        x2_name = "s_cr,Np"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_22(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/(x3/x4)
        result_name = "ΣA_s,re = "
        result_units = "cm2"
        x1_name = "k_4"
        x2_name = "ΣN_Ed [kN]"
        x3_name = "f_yk,re [kN/cm2]"
        x4_name = "γ_Ms,re"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_23(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*x4*x5*x6*x7
        result_name = "N_Rk,sp = "
        result_units = "kN"
        x1_name = "N0_Rk,sp [kN]"
        x2_name = "A_c,N"
        x3_name = "A0_c,N"
        x4_name = "ψ_s,N"
        x5_name = "ψ_re,N"
        x6_name = "ψ_ec,N"
        x7_name = "ψ_h,sp"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_24(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         (x1/x2)**(2/3)         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(1,((x3 + 1.5*x4)/x2)**(2/3))

        result_name = "Right side: "
        result_units = ""
        x1_name = "h"
        x2_name = "h_min"
        x3_name = "h_ef"
        x4_name = "c_1"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_25(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*x4*x5*x6
        result_name = "N_Rk,cb = "
        result_units = "kN"
        x1_name = "N0_Rk,cb [kN]"
        x2_name = "A_c,Nb"
        x3_name = "A0_c,Nb"
        x4_name = "ψ_s,Nb"
        x5_name = "ψ_g,Nb"
        x6_name = "ψ_ec,Nb"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_26(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*sqrt(x3*x4)
        result_name = "N0_Rk,cb = "
        result_units = ""
        x1_name = "k_5"
        x2_name = "c_1"
        x3_name = "A_h"
        x4_name = "f_ck"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_27(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*4)**2
        result_name = "A0_c,Nb = "
        result_units = "cm2"
        x1_name = "c_1 [cm]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_28(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7 + 0.3*x1/(2*x2)
        result_name = "ψ_s,Nb = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_29(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(x1) + (sqrt(x1) - 1)*x2/(4*x3)
        result_name = "ψ_g,Nb = "
        result_units = ""
        x1_name = "n"
        x2_name = "s_2"
        x3_name = "c_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_30(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(1 + 2*x1/(4*x2))
        result_name = "ψ_ec,Nb = "
        result_units = ""
        x1_name = "e_N"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_31(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "N_Rk,re = "
        result_units = "kN"
        x1_name = "ΣA_s,re,i [cm2]"
        x2_name = "f_yk,re [kN/cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_33(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*pi/(x4*x5)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x6*x7/x8
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "l_1 [cm]"
        x2_name = "Φ [cm]"
        x3_name = "f_bd [kN/cm2]"
        x4_name = "α_1"
        x5_name = "α_2"
        x6_name = "A_s,re"
        x7_name = "f_yk,re"
        x8_name = "γ_Ms,re"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_34(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "V0_Rk,s = "
        result_units = "kN"
        x1_name = "k_6"
        x2_name = "A_s [cm2]"
        x3_name = "f_uk [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_35(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,s = "
        result_units = "kN"
        x1_name = "k_7"
        x2_name = "V0_Rk,s [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_36(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1 - 0.01*x1)*x2*x3
        result_name = "V0_Rk,s = "
        result_units = "kN"
        x1_name = "t_grout"
        x2_name = "k_7"
        x3_name = "V0_Rk,s [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_37(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "V0_Rk,s,M = "
        result_units = "kN"
        x1_name = "α_M"
        x2_name = "M_Rk,s [kNm]"
        x3_name = "l_a [m]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_38(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - x2/(x3/x4))
        result_name = "M_Rk,s = "
        result_units = "kNm"
        x1_name = "M0_Rk,s [kNm]"
        x2_name = "N_Ed"
        x3_name = "N_Rk,s"
        x4_name = "γ_Ms"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_39a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_b"
        x2_name = "N_Rk,c [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_39b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1*x2
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_b"
        x2_name = "N_Rk,c [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_39c(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*min(x2, x3)
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_b"
        x2_name = "N_Rk,c [kN]"
        x3_name = "N_Rk,p [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)
        
class EN_1992_4__7_39d(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1*min(x2, x3)
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_b"
        x2_name = "N_Rk,c [kN]"
        x3_name = "N_Rk,p [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_40(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3*x4*x5*x6*x7*x8
        result_name = "V_Rk,c = "
        result_units = "kN"
        x1_name = "V0_Rk,c [kN]"
        x2_name = "A_c,V"
        x3_name = "A0_c,V"
        x4_name = "ψ_s,V"
        x5_name = "ψ_h,V"
        x6_name = "ψ_ec,V"
        x7_name = "ψ_α,V"
        x8_name = "ψ_re,V"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_41(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2**x3*x4**x5*sqrt(x6)*x7**1.5
        result_name = "V_Rk,c = "
        result_units = ""
        x1_name = "k_9"
        x2_name = "d_nom"
        x3_name = "α"
        x4_name = "l_f"
        x5_name = "β"
        x6_name = "f_ck"
        x7_name = "c_1"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_42(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.1*(x1/x2)**0.5
        result_name = "α = "
        result_units = ""
        x1_name = "l_f"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_43(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.1*(x1/x2)**0.2
        result_name = "β = "
        result_units = ""
        x1_name = "d_nom"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_44(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        4.5*x1**2
        result_name = "A0_c,v = "
        result_units = "cm2"
        x1_name = "c_1 [cm]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_45(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7 + 0.3*x1/(1.5*x2)
        result_name = "ψ_s,V = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_46(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (1.5*x1/x2)**0.5
        result_name = "ψ_h,V = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "h"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_47(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/(1 + 2*x1/(3*x2))
        result_name = "ψ_ec,V = "
        result_units = ""
        x1_name = "e_V"
        x2_name = "c_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_48(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        sqrt(1/(cos(x1)**2 + (0.5*sin(x1))**2))
        result_name = "ψ_α,V = "
        result_units = ""
        x1_name = "α_V [rad]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_49(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1/1.5, x2/1.5)
        result_name = "c'_1 = "
        result_units = ""
        x1_name = "c_2,max"
        x2_name = "h"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_50(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1/1.5, x2/1.5, x3/3)
        result_name = "c'_1 = "
        result_units = ""
        x1_name = "c_2,max"
        x2_name = "h"
        x3_name = "s_2,max"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_51(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "N_Rk,re = "
        result_units = "kN"
        x1_name = "k_10"
        x2_name = "ΣA_s,re,i [cm2]"
        x3_name = "f_yk,re [kN/cm2]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_53(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                         x1*x2*x3*pi/(x4*x5)
                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                        x6*x7/x8
                                  
        result_name = "Right side: "
        result_units = "kN"
        x1_name = "l_1 [cm]"
        x2_name = "Φ [cm]"
        x3_name = "f_bd [kN/cm2]"
        x4_name = "α_1"
        x5_name = "α_2"
        x6_name = "A_s,re"
        x7_name = "f_yk,re"
        x8_name = "γ_Ms,re"
        self.widget = Calculator(8, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, \
                                 x7_name, x8_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_57(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**x5 + (x3/x4)**x5
        result_name = "result: "
        result_units = ""
        x1_name = "N_Ed"
        x2_name = "N_Rd,i"
        x3_name = "V_Ed"
        x4_name = "V_Rd,i"
        x5_name = "k_11"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_58(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "N_Rk,s,l = "
        result_units = "kN"
        x1_name = "N0_Rk,s,l [kN]"
        x2_name = "ψ_l,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_59(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*(1 + x1/x2)
        result_name = "ψ_l,N = "
        result_units = ""
        x1_name = "s_cb0"
        x2_name = "s_l,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_60(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "N_Rk,c = "
        result_units = "kN"
        x1_name = "N0_Rk,c [kN]"
        x2_name = "ψ_ch,s,N"
        x3_name = "ψ_ch,e,N"
        x4_name = "ψ_ch,c,N"
        x5_name = "ψ_re,N"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_62(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*(2.8 - 1.3*x1/180)*x1
        result_name = "s_cr,N = "
        result_units = ""
        x1_name = "h_ef"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_63(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**0.5
        result_name = "ψ_ch,e,N = "
        result_units = ""
        x1_name = "c_1"
        x2_name = "c_cr,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_63a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.5
        result_name = "c_cr,N = "
        result_units = "cm"
        x1_name = "s_cr,N [cm]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_64(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**0.5
        result_name = "ψ_ch,c,N = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "c_cr,N"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_65(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2*x3
        result_name = "h'_ef = "
        result_units = "cm"
        x1_name = "c_max/s_max"
        x2_name = "c_cr,N/s_cr,N"
        x3_name = "h_ef [cm]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_66(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*x1/(x2/x3)
        result_name = "ΣA_s,re = "
        result_units = "cm2"
        x1_name = "N_ed [kN]"
        x2_name = "f_yk,re [kN/cm2]"
        x3_name = "γ_Ms,re"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_67(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*x6
        result_name = "N_Rk,sp = "
        result_units = "kN"
        x1_name = "N0_Rk [kN]"
        x2_name = "ψ_ch,s,N"
        x3_name = "ψ_ch,c,N"
        x4_name = "ψ_ch,e,N"
        x5_name = "ψ_re,N"
        x6_name = "ψ_h,sp"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_68(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         (x1/x2)**(2/3)         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(1,((x3 + x4)/x2)**(2/3))

        result_name = "Right side: "
        result_units = ""
        x1_name = "h"
        x2_name = "h_min"
        x3_name = "h_ef"
        x4_name = "c_cr,N"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_69(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4
        result_name = "N_Rk,cb = "
        result_units = "kN"
        x1_name = "N0_Rk,cb [kN]"
        x2_name = "ψ_ch,s,Nb"
        x3_name = "ψ_ch,c,Nb"
        x4_name = "ψ_ch,h,Nb"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_70(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**0.5
        result_name = "ψ_ch,c,Nb = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "c_cr,Nb"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_71(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         (x1 + x2)/(4*x3)
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (2*x3 + x2)/(4*x3)

        result_name = "Right side: "
        result_units = ""
        x1_name = "h_ef"
        x2_name = "f"
        x3_name = "c_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, \
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2, \
                                 left_right_naming = True)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_72(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,s,l = "
        result_units = "kN"
        x1_name = "V0_Rk,s,l [kN]"
        x2_name = "ψ_l,V"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_73(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.5*(1 + x1/x2)
        result_name = "ψ_l,V = "
        result_units = ""
        x1_name = "s_cb0"
        x2_name = "s_l,V"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_74(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "V_Rk,s,M = "
        result_units = "kN"
        x1_name = "M_Rk,s [kNm]"
        x2_name = "l_a [m]"
        x3_name = "α_M"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_75(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*(1 - x2/(x3/x4))
        result_name = "M_Rk,s = "
        result_units = "kNm"
        x1_name = "M0_Rk,s [kNm]"
        x2_name = "N_Ed"
        x3_name = "N_Rk,s"
        x4_name = "γ_Ms"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_76a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_8"
        x2_name = "N_Rk,c [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_76b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.75*x1*x2
        result_name = "V_Rk,cp = "
        result_units = "kN"
        x1_name = "k_8"
        x2_name = "N_Rk,c [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_77(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5*x6
        result_name = "V_Rk,c = "
        result_units = "kN"
        x1_name = "V0_Rk,c [kN]"
        x2_name = "ψ_ch,s,V"
        x3_name = "ψ_ch,c,V"
        x4_name = "ψ_ch,h,V"
        x5_name = "ψ_ch,90°,V"
        x6_name = "ψ_re,V"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_78(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*sqrt(x2)*x3**(4/3)
        result_name = "V0_Rk,c = "
        result_units = ""
        x1_name = "k_12"
        x2_name = "f_ck"
        x3_name = "c_1"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_80(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        4*x1 + 2*x2
        result_name = "s_cr,V = "
        result_units = "cm"
        x1_name = "c_1 [cm]"
        x2_name = "b_ch [cm]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_81(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**0.5
        result_name = "ψ_ch,c,V = "
        result_units = ""
        x1_name = "c_2"
        x2_name = "c_cr,V"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_82(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.5
        result_name = "c_cr,V = "
        result_units = "cm"
        x1_name = "s_cr,V [cm]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_83(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**0.5
        result_name = "ψ_ch,h,V = "
        result_units = ""
        x1_name = "h"
        x2_name = "h_cr,V"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_84(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        2*x1 + 2*x2
        result_name = "h_cr,V = "
        result_units = "cm"
        x1_name = "c_1 [cm]"
        x2_name = "h_ch [cm]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_86(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max((x1 - x2)/2, (x3 - 2*x4)/2)
        result_name = "c'_1 = "
        result_units = ""
        x1_name = "c_2,max"
        x2_name = "b_ch"
        x3_name = "h"
        x4_name = "h_ch"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_87(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**2 + (x3/x4)**2
        result_name = "result: "
        result_units = ""
        x1_name = "N_Ed"
        x2_name = "N_Rd,s"
        x3_name = "V_Ed"
        x4_name = "V_Rd,s"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_88(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1/x2, x3/x4)**x7 + (x5/x6)**x7
        result_name = "result: "
        result_units = ""
        x1_name = "N_Ed"
        x2_name = "N_Rd,s,l"
        x3_name = "M_Ed"
        x4_name = "M_Rd,s,flex"
        x5_name = "V_Ed"
        x6_name = "V_Rd,s,l"
        x7_name = "k_13"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_89b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        max(x1/x2, x1/x3)**x6 + (x4/x5)**x6
        result_name = "result: "
        result_units = ""
        x1_name = "N_Ed"
        x2_name = "N_Rd,s,a"
        x3_name = "N_Rd,s,c"
        x4_name = "V_Ed"
        x5_name = "V_Rd,s,a"
        x6_name = "k_14"
        self.widget = Calculator(6, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, \
                                 x6_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_90(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**1.5 + (x3/x4)**1.5
        result_name = "result: "
        result_units = ""
        x1_name = "Na_Ed"
        x2_name = "N_Rd"
        x3_name = "Va_Ed"
        x4_name = "V_Rd"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_91(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2) + (x3/x4)
        result_name = "result: "
        result_units = ""
        x1_name = "Na_Ed"
        x2_name = "N_Rd"
        x3_name = "Va_Ed"
        x4_name = "V_Rd"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__7_92(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2) + (x3/x4)
        result_name = "result: "
        result_units = ""
        x1_name = "Na_Ed"
        x2_name = "N_Rd,i"
        x3_name = "Va_Ed"
        x4_name = "V_Rd,i"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__8_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1)**x3 + (x2)**x3
        result_name = "result: "
        result_units = ""
        x1_name = "β_N,fat"
        x2_name = "β_V,fat"
        x3_name = "α"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__8_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "β_N,fat = "
        result_units = ""
        x1_name = "γ_F,fat"
        x2_name = "ΔN_Ek"
        x3_name = "ψ_F,N"
        x4_name = "ΔN_Rk"
        x5_name = "γ_M,fat"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__8_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3*x4*x5
        result_name = "β_V,fat = "
        result_units = ""
        x1_name = "γ_F,fat"
        x2_name = "ΔV_Ek"
        x3_name = "ψ_F,V"
        x4_name = "ΔV_Rk"
        x5_name = "γ_M,fat"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__A_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.8
        result_name = "V_Ed = "
        result_units = "kN"
        x1_name = "V_Rd,c [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__A_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(x1, x2)*0.8
        result_name = "V_Ed = "
        result_units = "kN"
        x1_name = "V_Rd,s [kN]"
        x2_name = "V_Rd,max [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__A_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        200*sqrt(x1)
        result_name = "a = "
        result_units = ""
        x1_name = "N_Ek"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__A_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*0.4
        result_name = "V_Ed,a = "
        result_units = "kN"
        x1_name = "V_Rd,c [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__A_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        min(x1, x2)*0.4
        result_name = "V_Ed,a = "
        result_units = "kN"
        x1_name = "V_Rd,s [kN]"
        x2_name = "V_Rd,max [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7*x1/x2
        result_name = "R_k,s,eq = "
        result_units = "kN"
        x1_name = "R_k,conc,eq [kN]"
        x2_name = "γ_inst"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1/x2
                         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.7*x3/(x4*x5)
        result_name = "Right side: "
        result_units = ""
        x1_name = "R_k,s,eq"
        x2_name = "Eh_d"
        x3_name = "R_k,conc,eq"
        x4_name = "Eg_d"
        x5_name = "γ_inst"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name,\
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function2 = lambda x1, x2, x3, x4, x5, x6, \
                                x7, x8, x9, x10, x11, x12: \
                         x1*x2*((1 + x3/x4)*x5 - 0.5)
                         
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "Right side: "
        result_units = ""
        x1_name = "α"
        x2_name = "S"
        x3_name = "z"
        x4_name = "H"
        x5_name = "A_a"
        
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name,\
                                 non_equation = True, \
                                 result_name2 = "Left side: ", \
                                 calc_function2 = calc_function2)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        3/(1 + (1 - x1/x2)**2)
        result_name = "A_a = "
        result_units = ""
        x1_name = "T_a"
        x2_name = "T_1"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1*x2*x3)/x4
        result_name = "F_va "
        result_units = ""
        x1_name = "S_va"
        x2_name = "W_a"
        x3_name = "γ_a"
        x4_name = "q_a"
        self.widget = Calculator(4, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "S_va = "
        result_units = ""
        x1_name = "α_V"
        x2_name = "A_a"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "R_d,eq = "
        result_units = ""
        x1_name = "R_k,eq"
        x2_name = "γ_M,eq"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_8(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):                                  
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2*x3
        result_name = "R_k,eq = "
        result_units = "kN"
        x1_name = "α_gap"
        x2_name = "α_eq"
        x3_name = "R0_k,eq [kN]"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        (x1/x2)**x5 + (x3/x4)**x5
        result_name = "result: "
        result_units = ""
        x1_name = "N_Ed"
        x2_name = "N_Rd,i,eq"
        x3_name = "V_Ed"
        x4_name = "V_Rd,i,eq"
        x5_name = "k_15"
        self.widget = Calculator(5, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "θ_p = "
        result_units = ""
        x1_name = "δ_N,eq"
        x2_name = "s_max"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_11a(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "N_Rd,eq,red = "
        result_units = "kN"
        x1_name = "N_Rd,eq [kN]"
        x2_name = "δ_N,req(DLS)"
        x3_name = "δ_N,eq(DLS)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__C_11b(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/x3
        result_name = "V_Rd,eq,red = "
        result_units = "kN"
        x1_name = "V_Rd,eq [kN]"
        x2_name = "δ_V,req(DLS)"
        x3_name = "δ_V,eq(DLS)"
        self.widget = Calculator(3, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "N_Rk,s,fi = "
        result_units = "kN"
        x1_name = "σ_Rk,s,fi [kN/cm2]"
        x2_name = "A_s [cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2/200
        result_name = "N0_Rk,c,fi(90) = "
        result_units = ""
        x1_name = "h_ef"
        x2_name = "N0_Rk,c"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.8*x1*x2/200
        result_name = "N0_Rk,c,fi(120) = "
        result_units = ""
        x1_name = "h_ef"
        x2_name = "N0_Rk,c"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_4(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*x1
        result_name = "N0_Rk,p,fi(90) = "
        result_units = "kN"
        x1_name = "N_Rk,p [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_5(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.2*x1
        result_name = "N0_Rk,p,fi(120) = "
        result_units = "kN"
        x1_name = "N_Rk,p [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_6(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,s,fi = "
        result_units = "kN"
        x1_name = "σ_Rk,s,fi [kN/cm2]"
        x2_name = "A_s [cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_7(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1.2*x1*x2
        result_name = "M_Rk,s,fi = "
        result_units = "kNcm"
        x1_name = "W_el [cm3]"
        x2_name = "σ_Rk,s,fi [kN/cm2]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_8_9(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1*x2
        result_name = "V_Rk,cp,fi(90/120) = "
        result_units = "kNc"
        x1_name = "k_8"
        x2_name = "N_Rk,c,fi(90/120) [kN]"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_10(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.25*x1
        result_name = "V0_Rk,c,fi(90) = "
        result_units = "kN"
        x1_name = "V0_Rk,c [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__D_11(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        0.2*x1
        result_name = "V0_Rk,c,fi(120) = "
        result_units = "kN"
        x1_name = "V0_Rk,c [kN]"
        self.widget = Calculator(1, window_title, calc_function, result_name, result_units, \
                                 x1_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__G_1(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "F0_Rd = "
        result_units = "kN"
        x1_name = "F0_Rk [kN]"
        x2_name = "γ_M"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__G_2(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        1/x1*x2/x3*x4*x5*x6*x7
        result_name = "F_Rd = "
        result_units = "kN"
        x1_name = "n"
        x2_name = "A_c"
        x3_name = "A0_c"
        x4_name = "ψ_s"
        x5_name = "ψ_re"
        x6_name = "ψ_c"
        x7_name = "F0_Rd [kN]"
        self.widget = Calculator(7, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name, x3_name, x4_name, x5_name, x6_name, x7_name)
        self.setCentralWidget(self.widget)

class EN_1992_4__G_3(QMainWindow):
    def __init__(self):
        super().__init__()
        self.windowTitleChanged.connect(self.display_calculator)
        
    def display_calculator(self, window_title):
        calc_function = lambda x1, x2, x3, x4, x5, x6, \
                               x7, x8, x9, x10, x11, x12: \
                        x1/x2
        result_name = "F_Rd = "
        result_units = "kN"
        x1_name = "F_Rk [kN]"
        x2_name = "γ_M"
        self.widget = Calculator(2, window_title, calc_function, result_name, result_units, \
                                 x1_name, x2_name)
        self.setCentralWidget(self.widget)

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

main_window = MainWindow()
main_window.show()
license_window = License_window()
tutorial_window = Tutorial_window()
faq_window = Faq_window()
register_window = Register_window()
notebook_window = Notebook_window()

app.exec_()
