# eurocode-calculator
Simple calculator for predefined equations written in PyQt and Haskell.


Program description
-------------------

Simple program that acts as a calculator for the given equations.
Equations on the left side can be grouped on the right side for a
given tab. Multiple tabs can be created.

The program terminates every 15min. Registration can be performed
via the Licens menu -> Register. Languages can be switched in the
Language menu. A short tutorial is provided in the Help menu.


Installation
------------

The python part in writen in Python 3. If you use windows, python
needs to be installed from www.python.org/downloads/.

The python code uses the PyQt5 library. You can install it with:
pip install PyQt5

The haskell part is written in GHC version 8.10.7. 
You will need to install haskell and cabal.
Follow insturctions for ghcup: https://www.haskell.org/ghcup/

NOTE1: Do not install the haskell-platform via your package manager.
Issues can arise in the next step when installing packages with cabal.

NOTE2: If you get the following error:
"_eghcup --cache install ghc recommended" failed!
you either need to upgrade WSL 1 to WSL 2 if running Linux on Windows
system. Or temporary enlarge your /tmp folder with the command:
mount -o remount,size=7G /tmp/

NOTE3: When the installation finishes restart your terminal.

NOTE4: To remove ghc or any other related program you can run:
ghcup rm ghc-<version>

Following haskell libraries need to be installed with cabal:
cabal new-install --lib http-conduit pureMD5 aeson array split sqlite-simple
NOTE: If installation of http-conduit fails try to install zlib-devel.

Now you can compile the haskell code. Go to the haskel-libs folder and
compile with: ghc Main.hs


Running the program
-------------------

The program can be started from the main folder with the command:
python main_window.py
