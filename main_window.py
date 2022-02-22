#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 2022

@author: Luka Kurnjek
@license: MIT License
"""

import sys
sys.path.insert(1, './python-libs')

from calculators import *
from small_windows import *
from registration import *

import json
import time
import hashlib
import subprocess
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
        self.menu.setMinimumWidth(190)
        self.menu.setMaximumWidth(190)
        self.menu.itemClicked.connect(self.menu_clicked_item)
        
        # Creating the equation widget that displays the .png equations
        self.equation = ScrollPicture(self)
        self.equation.setPicture("language/start_text_English.png")

        # Creating 10 bookmark widgets. That is the upper limit for creating tabs.
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
        
        self.bookmark_widgets = {1: self.bookmark1,
                                 2: self.bookmark2,
                                 3: self.bookmark3,
                                 4: self.bookmark4,
                                 5: self.bookmark5,
                                 6: self.bookmark6,
                                 7: self.bookmark7,
                                 8: self.bookmark8,
                                 9: self.bookmark9,
                                 10: self.bookmark10}
        
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
        
        # By default the bookmark tab 1 gets always displayed
        self.display_bookmark_window_at_startup(1)
        
        # Checks if populated bookmark XML files exist and if yes displays them
        for bookmark_key in range(2,11):
            file_name = "xml_trees/bookmark_tree" + str(bookmark_key) + ".xml"
            file_exists = exists(file_name)
            
            if file_exists:
                fp = open(file_name, "r")
                file_content = fp.read()
                fp.close()
            
            if file_exists and file_content != "" and \
            file_content != '<EN_1992>\n</EN_1992>':
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
        register_window.timer.stop()

    # Defining tab functions        
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
        
    # Sets the text of the menu, main window and calculator buttons to english
    def set_english(self):
        global language
        language = "English"
        
        faq_window.setWindowTitle("Frequently asked questions")
        tutorial_window.setWindowTitle("Tutorial")
        register_window.setWindowTitle("Register")
        
        register_window.label.setText("Input the registration code:")
        register_window.button.setText("Register")
        
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
        
    # Sets the text of the menu, main window and calculator buttons to german
    def set_german(self):
        global language
        language = "German"
        
        faq_window.setWindowTitle("Häufig gestellte Fragen")
        tutorial_window.setWindowTitle("Tutorium")
        register_window.setWindowTitle("Registrierung")
        
        register_window.label.setText("Geben Sie den Registrierungscode ein:")
        register_window.button.setText("Registrieren")
        
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
        
        self.bookmark_widgets[bookmark_key].setHeaderLabels(("Bookmarked equations",))
        self.bookmark_widgets[bookmark_key].setMinimumWidth(190)
        self.bookmark_widgets[bookmark_key].setMaximumWidth(190)
        self.tabs.addTab(self.bookmark_widgets[bookmark_key], "Tab " + str(bookmark_key))
        self.bookmark_widgets[bookmark_key].itemClicked.connect(self.bookmark_clicked_item)
        
        current_tab = bookmark_key
        opened_tabs.append(bookmark_key)
        
        # Populates XML files from the SQL database
        self.update_xml_files()
        
        file_name = "xml_trees/bookmark_tree" + str(bookmark_key) + ".xml"
        with open(file_name, "r") as file:
            xmlFileString = file.read()
        self.populate_tree_widget(self.bookmark_widgets[bookmark_key], xmlFileString)
        
    # Prints the chapter tree structure from XML string to the given tree widget
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
        
        # If an equation is selected bookmark it
        if "Eq" in tree_path_elements[-1]:
            # Cals the haskell code for writing the bookmarked equation to the SQL database
            response = subprocess.Popen(["../haskell-libs/Main", "ParseDatabase", "writeSQL", \
                                         str(selected_tab), tree_path_elements[1], \
                                         tree_path_elements[2], tree_path_elements[3]], \
                                         cwd="./data", stdout=subprocess.PIPE)
            time.sleep(0.2) # needed because of the next subprocess call that reads from the
            # SQL database. If the delay is not present the response is busy.
            output = response.communicate()[0]
            
            # If the item does not allready exist write to the xml bookmark file
            if "Item allready exists." not in output.decode("utf-8"):
                
                # Populates the XML bookmark file with the data from the SQL database
                self.update_xml_files()
        
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

    # Reads out the SQL database and calls the function parse_bookmark_xml
    # for each entry in the database.
    def update_xml_files(self):
        response = subprocess.Popen(["../haskell-libs/Main", "ParseDatabase", "readSQL"], cwd="./data", stdout=subprocess.PIPE)
        output = response.communicate()[0]

        parameters = output.decode("utf-8")
        parameters = parameters.strip("[\"]\n")
        parameters = parameters.split(",")
        
        # Creating clean XML files to be populated from the SQL database
        for count in range(1,11):
            file_name = "xml_trees/bookmark_tree" + str(count) + ".xml"
            file_exists = exists(file_name)
            
            if file_exists:
                fp = open(file_name, 'w')
                fp.write("<EN_1992>\n</EN_1992>")
                fp.close()

        # Looping through the SQL database output parameters and calling parse_bookmark_xml function
        for parameter in parameters:
            if "|" in parameter:
                input_data = parameter.split("|")
                self.parse_bookmark_xml(input_data[1], input_data[2], input_data[3], input_data[0])
    
    # Opens the calculator window
    def show_calculator(self):
        global calculator_name
        
        # Shows the calculator window only if an equation is selected
        if "Eq" in menu_window_text:
            if tree_widget_selection == "menu":
                selected_menu_item = self.menu.currentItem()
                tree_path_elements = self.get_parent_path(selected_menu_item)
                tree_path_elements = tree_path_elements.split("/")
                
                modified_window_text = menu_window_text.replace('Eq','')
                modified_window_text = tree_path_elements[1] + modified_window_text
                
                calculator_name = modified_window_text + "_menu"
                self.process_calculator_window_data(modified_window_text, calculator_name)
                
            elif tree_widget_selection == "bookmark":
                selected_menu_item = self.bookmark_widgets[selected_tab].currentItem()
                tree_path_elements = self.get_parent_path(selected_menu_item)
                tree_path_elements = tree_path_elements.split("/")
                
                modified_window_text = menu_window_text.replace('Eq','')
                modified_window_text = tree_path_elements[1] + modified_window_text
                
                calculator_name = modified_window_text + "_tab" + str(selected_tab)
                self.process_calculator_window_data(modified_window_text, calculator_name)
    
    # Process some txt data on the calculator window
    def process_calculator_window_data(self, modified_window_text, calculator_name):
        global current_calc
        
        current_calc = Calcs[modified_window_text]
        Calcs[modified_window_text].show()
        Calcs[modified_window_text].setWindowTitle(calculator_name)
        
        # Changes the text in the calculator window depending on the language
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
        if current_element_to_delete != "EN_1992":
            (item[0].parent() or root).removeChild(item[0])
                
        # The if statements delete content from XML file depending on what was
        # selected. If for e.g. a chapter was selected all subelements from that
        # chapter get deleted from the SQL database and from the XML files.
        if "Eq" in current_element_to_delete:
            response = subprocess.Popen(["../haskell-libs/Main", "ParseDatabase", "deleteSQL", \
                                          str(selected_tab), tree_path_elements[1], \
                                          tree_path_elements[2], tree_path_elements[3]], \
                                          cwd="./data", stdout=subprocess.PIPE)
            time.sleep(0.2)
            self.update_xml_files()
            
        elif "Chapter" in current_element_to_delete:
            response = subprocess.Popen(["../haskell-libs/Main", "ParseDatabase", "deleteSQL", \
                                          str(selected_tab), tree_path_elements[1], \
                                          tree_path_elements[2]], \
                                          cwd="./data", stdout=subprocess.PIPE)
            time.sleep(0.2)
            self.update_xml_files()
        
        elif "EN" in current_element_to_delete and "-" in current_element_to_delete:
            response = subprocess.Popen(["../haskell-libs/Main", "ParseDatabase", "deleteSQL", \
                                          str(selected_tab), tree_path_elements[1]], \
                                          cwd="./data", stdout=subprocess.PIPE)
            time.sleep(0.2)
            self.update_xml_files()
                        
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
                if current_tab <= 10:
                    current_tab = opened_tabs_sorted[index] + 1
                    opened_tabs.append(current_tab)
                break

        # Handels the case if tabs are ordered
        if 1 not in opened_tabs and tabs_are_orderd:
            current_tab = 1
            opened_tabs.append(current_tab)
        # Makes a boundary for max 10 opened tabs
        elif tabs_are_orderd and current_tab <= 9:
            current_tab = max(opened_tabs) + 1
            if current_tab > 10:
                current_tab = max(opened_tabs)
                return
            else:
                opened_tabs.append(current_tab)
        # Does nothing if 10 tabs are allready opened
        elif tabs_are_orderd:
            return
        
        # Creates a new QTreeWidget in the stacked widget
        self.bookmark_widgets[current_tab] = QTreeWidget()        
        if language == "English":
            self.bookmark_widgets[current_tab].setHeaderLabels(("Bookmarked equations",))
        elif language == "German":
            self.bookmark_widgets[current_tab].setHeaderLabels(("Lesezeichen Gleichungen",))
        
        self.bookmark_widgets[current_tab].setMinimumWidth(190)
        self.bookmark_widgets[current_tab].setMaximumWidth(190)
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

# The startup window that notifies the user of the trial version terms
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

# Global variables
menu_window_text = "empty"
current_tab = 1
selected_tab = 1
opened_tabs = []
language = "English"
tree_widget_selection = "menu"
current_calc = ""
calculator_name = ""

# Creates the application handler. Has to be first line in code out of classes.
app = QApplication(sys.argv)

Calcs = dict()
Calcs = {  "EN_1992-1-1_3.1": EN_1992_1_1__3_1()
         , "EN_1992-1-1_3.2": EN_1992_1_1__3_2()
         , "EN_1992-2_3.15": EN_1992_2__3_15()
         , "EN_1992-2_3.16": EN_1992_2__3_16()
         , "EN_1992-3_7.122": EN_1992_3__7_122()
         , "EN_1992-3_K.1": EN_1992_3__K_1()
         }

# Windows
startup_window = Startup_window()
main_window = MainWindow()
main_window.show() # displays the main window
license_window = License_window()
tutorial_window = Tutorial_window()
faq_window = Faq_window()
register_window = Register_window()
notebook_window = Notebook_window()

# Function to display the startup window
def show_startup():
    with open("data/hide_startup", "r") as file:
        hide_startup_txt = file.read()
    
    with open("data/registered_global", "r") as file:
        registered_global_txt = file.read()
    
    if registered_global_txt == "False" and "False" in hide_startup_txt:
        startup_window.show()

show_startup()

# Starts the event loop
app.exec_()
