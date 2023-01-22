# eurocode-calculator
Simple calculator for predefined EC2 (eurocode 2) equations written in PyQt5.

This branch contains source code for microsoft windows installer file that is 
published on the page www.eurocode-calculator.com. The installer file can be 
found in the executables folder.

The Microsoft installer files were created with [Inno Setup](https://jrsoftware.org/isdl.php) software.

The main exacutable file was created with the [PyInstaller](https://pyinstaller.org/en/stable/) packaging tool.
`pyinstaller --onefile main_window.py --noconsole`

Before creating the main executable file Python 3.11 nad PyQt5 had to be installed.
For installation of PyQt5 look at the *Installation* section bellow.  

Program description
-------------------

Simple program that acts as a calculator for the given equations.
Equations on the left side can be grouped on the right side for a
given tab. Multiple tabs can be created.

![alt text](https://github.com/LukaKurnjek/eurocode-calculator/blob/main/data/eurocode-calculator.png) 

Languages can be switched in the Language menu. A short tutorial is 
provided in the Help menu of the calculator.

Installation
------------

If you use windows, python needs to be installed from www.python.org/downloads/.

The python code uses the PyQt5 library. On most linux distributions
it comes with the standard python installation. 

If it is not installed or if you are using windows, you can install by using PIP 
(python package manager) which can be installed for Linux as:

For Fedora: `sudo yum install python3-pip`<br>
For Ubuntu: `sudo apt install python3-pip`

For windows it comes with the standard Python installation for versions +3.4.
You can then install PyQt5 with the terminal command: `pip install PyQt5`

Running the program
-------------------

The program can be started from the main folder with the command:
`python main_window.py`
