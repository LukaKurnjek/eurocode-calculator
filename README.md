# eurocode-calculator
Simple calculator for predefined EC2 (eurocode 2) equations written in PyQt5 and Haskell.
This branch contains only a handful equations as oposed to the python
branches that contains all (more then 500) EC equations. 

For the pure Python version look at the branch python-single-file. The
sources published on the page www.eurocode-calculator.com are compiled
using code from this branch.

There is also a branch python-only that shares the same code as the 
python-single-file branch, where the code is distributed in multiple
files, but the branch is currently under maintenance. 

The versions on the other branches do not require registration and do
not contain a rest-api folder. 

Program description
-------------------

Simple program that acts as a calculator for the given equations.
Equations on the left side can be grouped on the right side for a
given tab. Multiple tabs can be created.

![alt text](https://github.com/LukaKurnjek/eurocode-calculator/blob/main/data/eurocode-calculator.png) 

Languages can be switched in the Language menu. A short tutorial is 
provided in the Help menu of the calculator.

The program terminates every 15min. Registration can be performed via
the menu *Licens -> Register*. Register keys are in *rest-api/keys.txt*.

Registration works only if you also setup a local REST API. You can 
follow the procedure in the **REST API setup** chapter. 

Setting up a local REST API is not neccessary to simply test the program.

Installation
------------

The python part in writen in Python 3. If you use windows, python
needs to be installed from www.python.org/downloads/.

The python code uses the PyQt5 library. On most linux distributions
it comes with the standard python installation. 

If it is not installed, you can install by using pip (python package 
manager) which can be installed as:

For Fedora: `sudo yum install python3-pip`<br>
For Ubuntu: `sudo apt install python3-pip`

And then install PyQt5 with: `pip install PyQt5`

The haskell part is written in GHC version 8.10.7. You will need to 
install haskell and cabal.

Follow insturctions for ghcup: https://www.haskell.org/ghcup/

Following haskell libraries need to be installed with cabal:<br>
`cabal new-install --lib http-conduit pureMD5 aeson array split sqlite-simple`<br>

If you get the error: `/usr/bin/ld.gold: error: cannot find -lgmp`
install the GMP devel library.

For Fedora: `sudo yum install gmp-devel`<br>
For Ubuntu: `sudo apt-get install libgmp3-dev`

If you get the error message `cabal: Failed to build zlib-0.6.3.0`
then try to install zlib-devel package.

Now you can compile the haskell code. Go to the haskel-libs folder and
compile with: `ghc Main.hs`

If you get the error message bellow you need to install ncurses library:<br>
`/usr/bin/ld.gold: error: cannot find -ltinfo`

For Fedora: `sudo yum install ncurses-compat-libs`<br>
For Ubuntu: `sudo apt-get install libncurses5-dev`

Then you also have to create a symbolic link as:<br>
`sudo ln -s /usr/lib64/libtinfo.so.5 /usr/lib64/libtinfo.so`

Running the program
-------------------

The program can be started from the main folder with the command:
`python main_window.py`

REST API setup
--------------

The files to setup the local REST API are in the folder rest-api.
First you need to install pip (python package manager).

Installing pip on Fedora:
`sudo yum install python3-pip`

Then you need to install Flask by using pip:
`pip install -U Flask`

After that cd into the folder rest-api and run the following three 
commands, that will set up a local REST API:
```
export FLASK_APP=rest-api.py
export FLASK_ENV=development
flask run -h 127.0.0.1 -p 3000
```