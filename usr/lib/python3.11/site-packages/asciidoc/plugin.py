import os
import re
import shutil
import sys
from typing import NoReturn
import zipfile

from .message import Message
from .utils import userdir


class Plugin:
    """
    --filter and --theme option commands.
    """
    CMDS = ('install', 'remove', 'list', 'build')

    def __init__(self, type: str, message: Message, config):
        self.type = type
        self.message = message
        self.config = config

    def die(self, msg: str) -> NoReturn:
        self.message.stderr(msg)
        sys.exit(1)

    def get_dir(self) -> str:
        """
        Return plugins path (.asciidoc/filters or .asciidoc/themes) in user's
        home directory or None if user home not defined.
        """
        result = userdir()
        if result:
            result = os.path.join(result, '.asciidoc', self.type + 's')
        return result

    def install(self, args) -> None:
        """
        Install plugin Zip file.
        args[0] is plugin zip file path.
        args[1] is optional destination plugins directory.
        """
        if len(args) not in (1, 2):
            self.die(
                'invalid number of arguments: --{} install {}'.format(
                    self.type,
                    ' '.join(args)
                )
            )
        zip_file = args[0]
        if not os.path.isfile(zip_file):
            self.die('file not found: %s' % zip_file)
        reo = re.match(r'^\w+', os.path.split(zip_file)[1])
        if not reo:
            self.die('file name does not start with legal {} name: {}'.format(
                self.type,
                zip_file
            ))
        plugin_name = reo.group()
        if len(args) == 2:
            plugins_dir = args[1]
            if not os.path.isdir(plugins_dir):
                self.die('directory not found: %s' % plugins_dir)
        else:
            plugins_dir = Plugin.get_dir()
            if not plugins_dir:
                self.die('user home directory is not defined')
        plugin_dir = os.path.join(plugins_dir, plugin_name)
        if os.path.exists(plugin_dir):
            self.die('%s is already installed: %s' % (self.type, plugin_dir))
        try:
            os.makedirs(plugin_dir)
        except Exception as e:
            self.die('failed to create %s directory: %s' % (self.type, str(e)))
        try:
            self.extract_zip(zip_file, plugin_dir)
        except Exception as e:
            if os.path.isdir(plugin_dir):
                shutil.rmtree(plugin_dir)
            self.die('failed to extract %s: %s' % (self.type, str(e)))

    def remove(self, args) -> None:
        """
        Delete plugin directory.
        args[0] is plugin name.
        args[1] is optional plugin directory (defaults to ~/.asciidoc/<plugin_name>).
        """
        if len(args) not in (1, 2):
            self.die('invalid number of arguments: --{} remove {}'.format(
                self.type,
                ' '.join(args)
            ))
        plugin_name = args[0]
        if not re.match(r'^\w+$', plugin_name):
            self.die('illegal %s name: %s' % (self.type, plugin_name))
        if len(args) == 2:
            d = args[1]
            if not os.path.isdir(d):
                self.die('directory not found: %s' % d)
        else:
            d = Plugin.get_dir()
            if not d:
                self.die('user directory is not defined')
        plugin_dir = os.path.join(d, plugin_name)
        if not os.path.isdir(plugin_dir):
            self.die('cannot find %s: %s' % (self.type, plugin_dir))
        try:
            self.message.verbose('removing: %s' % plugin_dir)
            shutil.rmtree(plugin_dir)
        except Exception as e:
            self.die('failed to delete %s: %s' % (self.type, str(e)))

    def list(self, _) -> None:
        """
        List all plugin directories (global and local).
        """
        dirs = [os.path.join(d, self.type + 's') for d in self.config.get_load_dirs()]
        for d in dirs:
            if os.path.isdir(d):
                plugin_dirs = [os.path.join(d, o) for o in os.listdir(d)]
                for f in sorted(filter(os.path.isdir, plugin_dirs)):
                    if f.endswith('__pycache__'):
                        continue
                    self.message.stdout(f)

    def build(self, args) -> None:
        """
        Create plugin Zip file.
        args[0] is Zip file name.
        args[1] is plugin directory.
        """
        if len(args) != 2:
            self.die('invalid number of arguments: --{} build {}'.format(
                self.type,
                ' '.join(args)
            ))
        zip_file = args[0]
        plugin_source = args[1]
        if not (os.path.isdir(plugin_source) or os.path.isfile(plugin_source)):
            self.die('plugin source not found: %s' % plugin_source)
        try:
            self.create_zip(zip_file, plugin_source, skip_hidden=True)
        except Exception as e:
            self.die('failed to create %s: %s' % (zip_file, str(e)))

    def extract_zip(self, zip_file: str, destdir: str) -> None:
        """
        Unzip Zip file to destination directory.
        Throws exception if error occurs.
        """
        zipo = zipfile.ZipFile(zip_file, 'r')
        try:
            for zi in zipo.infolist():
                outfile = zi.filename
                if not outfile.endswith('/'):
                    d, outfile = os.path.split(outfile)
                    directory = os.path.normpath(os.path.join(destdir, d))
                    if not os.path.isdir(directory):
                        os.makedirs(directory)
                    outfile = os.path.join(directory, outfile)
                    perms = (zi.external_attr >> 16) & 0o777
                    self.message.verbose('extracting: %s' % outfile)
                    flags = os.O_CREAT | os.O_WRONLY
                    if sys.platform == 'win32':
                        flags |= os.O_BINARY
                    if perms == 0:
                        # Zip files created under Windows do not include permissions.
                        fh = os.open(outfile, flags)
                    else:
                        fh = os.open(outfile, flags, perms)
                    try:
                        os.write(fh, zipo.read(zi.filename))
                    finally:
                        os.close(fh)
        finally:
            zipo.close()

    def create_zip(self, zip_file: str, src: str, skip_hidden: bool = False) -> None:
        """
        Create Zip file. If src is a directory archive all contained files and
        subdirectories, if src is a file archive the src file.
        Files and directories names starting with . are skipped
        if skip_hidden is True.
        Throws exception if error occurs.
        """
        zipo = zipfile.ZipFile(zip_file, 'w')
        try:
            if os.path.isfile(src):
                arcname = os.path.basename(src)
                self.message.verbose('archiving: %s' % arcname)
                zipo.write(src, arcname, zipfile.ZIP_DEFLATED)
            elif os.path.isdir(src):
                srcdir = os.path.abspath(src)
                if srcdir[-1] != os.path.sep:
                    srcdir += os.path.sep
                for root, dirs, files in os.walk(srcdir):
                    arcroot = os.path.abspath(root)[len(srcdir):]
                    if skip_hidden:
                        for d in dirs[:]:
                            if d.startswith('.'):
                                self.message.verbose(
                                    'skipping: %s' % os.path.join(arcroot, d)
                                )
                                del dirs[dirs.index(d)]
                    for f in files:
                        filename = os.path.join(root, f)
                        arcname = os.path.join(arcroot, f)
                        if skip_hidden and f.startswith('.'):
                            self.message.verbose('skipping: %s' % arcname)
                            continue
                        self.message.verbose('archiving: %s' % arcname)
                        zipo.write(filename, arcname, zipfile.ZIP_DEFLATED)
            else:
                raise ValueError('src must specify directory or file: %s' % src)
        finally:
            zipo.close()
