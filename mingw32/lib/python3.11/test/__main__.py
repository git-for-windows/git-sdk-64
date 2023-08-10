import os
import sys

from test.libregrtest import main

if sys.platform == "win32":
    # Enable DLL loading from PATH.
    os.environ["PYTHONLEGACYWINDOWSDLLLOADING"] = "1"

main()
