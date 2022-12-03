try:
    from .asciidoc import cli, set_caller
except ImportError:
    raise SystemExit('ERROR: You must execute as a module using the -m flag')

set_caller(__name__)
cli()
