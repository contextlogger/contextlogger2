#!/usr/bin/env python
# -*- coding: utf-8 -*-

##############################################################################
# cmd_py2sis.py - Ensymble command line tool, py2sis command
# Copyright 2006, 2007, 2008, 2009 Jussi Ylänen
#
# This file is part of Ensymble developer utilities for Symbian OS(TM).
#
# Ensymble is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Ensymble is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Ensymble; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##############################################################################

import sys
import os
import re
import getopt
import getpass
import locale
import zlib

from utils import sisfile
from utils import sisfield
from utils import symbianutil
from utils import rscfile
from utils import miffile


##############################################################################
# Help texts
##############################################################################

shorthelp = 'Create a SIS package for a "Python for S60" application'
longhelp  = '''py2sis
    [--uid=0x01234567] [--appname=AppName] [--version=1.0.0]
    [--lang=EN,...] [--icon=icon.svg] [--shortcaption="App. Name",...]
    [--caption="Application Name",...] [--drive=C] [extrasdir=root]
    [--textfile=mytext_%C.txt] [--cert=mycert.cer] [--privkey=mykey.key]
    [--passphrase=12345] [--heapsize=min,max] [--caps=Cap1+Cap2+...]
    [--vendor="Vendor Name",...] [--autostart] [--runinstall]
    [--encoding=terminal,filesystem] [--verbose]
    <src> [sisfile]

Create a SIS package for a "Python for S60" application.

Options:
    src          - Source script or directory
    sisfile      - Path of the created SIS file
    uid          - Symbian OS UID for the application
    appname      - Name of the application
    version      - Application version: X.Y.Z or X,Y,Z (major, minor, build)
    lang         - Comma separated list of two-character language codes
    icon         - Icon file in SVG-Tiny format
    shortcaption - Comma separated list of short captions in all languages
    caption      - Comma separated list of long captions in all languages
    drive        - Drive where the package will be installed (any by default)
    extrasdir    - Name of dir. tree placed under drive root (none by default)
    textfile     - Text file (or pattern, see below) to display during install
    cert         - Certificate to use for signing (PEM format)
    privkey      - Private key of the certificate (PEM format)
    passphrase   - Pass phrase of the private key (insecure, use stdin instead)
    caps         - Capability names, separated by "+" (none by default)
    vendor       - Vendor name or a comma separated list of names in all lang.
    autostart    - Application is registered to start on each device boot
    runinstall   - Application is automatically started after installation
    heapsize     - Application heap size, min. and/or max. ("4k,1M" by default)
    encoding     - Local character encodings for terminal and filesystem
    verbose      - Print extra statistics

If no certificate and its private key are given, a default self-signed
certificate is used to sign the SIS file. Software authors are encouraged
to create their own unique certificates for SIS packages that are to be
distributed.

If no icon is given, the Python logo is used as the icon. The Python
logo is a trademark of the Python Software Foundation.

Text to display uses UTF-8 encoding. The file name may contain formatting
characters that are substituted for each selected language. If no formatting
characters are present, the same text will be used for all languages.

    %%           - literal %
    %n           - language number (01 - 99)
    %c           - two-character language code in lowercase letters
    %C           - two-character language code in capital letters
    %l           - language name in English, using only lowercase letters
    %l           - language name in English, using mixed case letters
'''


##############################################################################
# Parameters
##############################################################################

MAXPASSPHRASELENGTH     = 256
MAXCERTIFICATELENGTH    = 65536
MAXPRIVATEKEYLENGTH     = 65536
MAXICONFILESIZE         = 65536
MAXOTHERFILESIZE        = 1024 * 1024 * 8   # Eight megabytes
MAXTEXTFILELENGTH       = 1024


##############################################################################
# Global variables
##############################################################################

debug = False


##############################################################################
# Public module-level functions
##############################################################################

def run(pgmname, argv):
    global debug

    # Determine system character encodings.
    try:
        # getdefaultlocale() may sometimes return None.
        # Fall back to ASCII encoding in that case.
        terminalenc = locale.getdefaultlocale()[1] + ""
    except TypeError:
        # Invalid locale, fall back to ASCII terminal encoding.
        terminalenc = "ascii"

    try:
        # sys.getfilesystemencoding() was introduced in Python v2.3 and
        # it can sometimes return None. Fall back to ASCII if something
        # goes wrong.
        filesystemenc = sys.getfilesystemencoding() + ""
    except (AttributeError, TypeError):
        filesystemenc = "ascii"

    try:
        gopt = getopt.gnu_getopt
    except:
        # Python <v2.3, GNU-style parameter ordering not supported.
        gopt = getopt.getopt

    # Parse command line arguments.
    short_opts = "u:n:r:l:i:s:c:f:x:t:a:k:p:b:d:gRH:e:vh"
    long_opts = [
        "uid=", "appname=", "version=", "lang=", "icon=",
        "shortcaption=", "caption=", "drive=", "extrasdir=", "textfile=",
        "cert=", "privkey=", "passphrase=", "caps=", "vendor=",
        "autostart", "runinstall", "heapsize=",
        "encoding=", "verbose", "debug", "help"
    ]
    args = gopt(argv, short_opts, long_opts)

    opts = dict(args[0])
    pargs = args[1]

    if len(pargs) == 0:
        raise ValueError("no source file name given")

    # Override character encoding of command line and filesystem.
    encs = opts.get("--encoding", opts.get("-e", "%s,%s" % (terminalenc,
                                                            filesystemenc)))
    try:
        terminalenc, filesystemenc = encs.split(",")
    except (ValueError, TypeError):
        raise ValueError("invalid encoding string '%s'" % encs)

    # Get source name, either a Python program or a directory.
    src = pargs[0].decode(terminalenc).encode(filesystemenc)
    if os.path.isdir(src):
        # Remove trailing slashes (or whatever the separator is).
        src = os.path.split(src + os.sep)[0]

        # Use last directory component as the name.
        basename = os.path.basename(src)

        # Source is a directory, recursively collect files it contains.
        srcdir = src
        srcfiles = []
        prefixlen = len(srcdir) + len(os.sep)
        def getfiles(arg, dirname, names):
            for name in names:
                path = os.path.join(dirname, name)
                if not os.path.isdir(path):
                    arg.append(path[prefixlen:])
        os.path.walk(srcdir, getfiles, srcfiles)

        # Read application version and UID3 from default.py.
        version, uid3 = scandefaults(os.path.join(srcdir, "default.py"))
    else:
        if src.lower().endswith(".py"):
            # Use program name without the .py extension.
            basename = os.path.basename(src)[:-3]
        else:
            # Unknown extension, use program name as-is.
            basename = os.path.basename(src)

        # Source is a file, use it.
        srcdir, srcfiles = os.path.split(src)
        srcfiles = [srcfiles]

        # Read application version and UID3 from file.
        version, uid3 = scandefaults(os.path.join(srcdir, srcfiles[0]))

    # Parse version string, use 1.0.0 by default.
    version = opts.get("--version", opts.get("-r", version))
    if version == None:
        version = "1.0.0"
        print ("%s: warning: no application version given, "
               "using %s" % (pgmname, version))
    try:
        version = parseversion(version)
    except (ValueError, IndexError, TypeError):
        raise ValueError("invalid version string '%s'" % version)

    # Determine output SIS file name.
    if len(pargs) == 1:
        # Derive output file name from input file name.
        outfile = "%s_v%d_%d_%d.sis" % (basename, version[0],
                                        version[1], version[2])
    elif len(pargs) == 2:
        outfile = pargs[1].decode(terminalenc).encode(filesystemenc)
        if os.path.isdir(outfile):
            # Output to directory, derive output name from input file name.
            outfile = os.path.join(outfile, "%s_v%d_%d_%d.sis" % (
                basename, version[0], version[1], version[2]))
        if not outfile.lower().endswith(".sis"):
            outfile += ".sis"
    else:
        raise ValueError("wrong number of arguments")

    # Determine application name (install dir.), use basename by default.
    appname = opts.get("--appname", opts.get("-n", basename))
    appname = appname.decode(terminalenc)

    # Auto-generate a test-range UID from application name.
    autouid = symbianutil.uidfromname(appname)

    # Get UID3.
    uid3 = opts.get("--uid", opts.get("-u", uid3))
    if uid3 == None:
        # No UID given, use auto-generated UID.
        uid3 = autouid
        print ("%s: warning: no UID given, using auto-generated "
               "test-range UID 0x%08x" % (pgmname, uid3))
    elif uid3.lower().startswith("0x"):
        # Prefer hex UIDs with leading "0x".
        uid3 = long(uid3, 16)
    else:
        try:
            if len(uid3) == 8:
                # Assuming hex UID even without leading "0x".
                print ('%s: warning: assuming hex UID even '
                       'without leading "0x"' % pgmname)
                uid3 = long(uid3, 16)
            else:
                # Decimal UID.
                uid3 = long(uid3)
                print ('%s: warning: decimal UID converted to 0x%08x' %
                       (pgmname, uid3))
        except ValueError:
            raise ValueError("invalid UID string '%s'" % uid3)

    # Warn against specifying a test-range UID manually.
    if uid3 & 0xf0000000L == 0xe0000000L and uid3 != autouid:
        print ("%s: warning: manually specifying a test-range UID is "
               "not recommended" % pgmname)

    # Determine application language(s), use "EN" by default.
    lang = opts.get("--lang", opts.get("-l", "EN")).split(",")
    numlang = len(lang)

    # Verify that the language codes are correct.
    for l in lang:
        try:
            symbianutil.langidtonum[l]
        except KeyError:
            raise ValueError("%s: no such language code" % l)

    # Get icon file name.
    icon = opts.get("--icon", opts.get("-i", None))
    if icon != None:
        icon = icon.decode(terminalenc).encode(filesystemenc)

        # Read icon file.
        f = file(icon, "rb")
        icondata = f.read(MAXICONFILESIZE + 1)
        f.close()

        if len(icondata) > MAXICONFILESIZE:
            raise ValueError("icon file too large")
    else:
        # No icon given, use a default icon.
        icondata = zlib.decompress(defaulticondata.decode("base-64"))

    # Determine application short caption(s).
    shortcaption = opts.get("--shortcaption", opts.get("-s", ""))
    shortcaption = shortcaption.decode(terminalenc)
    if len(shortcaption) == 0:
        # Short caption not given, use application name.
        shortcaption = [appname] * numlang
    else:
        shortcaption = shortcaption.split(",")

    # Determine application long caption(s), use short caption by default.
    caption = opts.get("--caption", opts.get("-c", ""))
    caption = caption.decode(terminalenc)
    if len(caption) == 0:
        # Caption not given, use short caption.
        caption = shortcaption
    else:
        caption = caption.split(",")

    # Compare the number of languages and captions.
    if len(shortcaption) != numlang or len(caption) != numlang:
        raise ValueError("invalid number of captions")

    # Determine installation drive, any by default.
    drive = opts.get("--drive", opts.get("-f", "any")).upper()
    if drive == "ANY" or drive == "!":
        drive = "!"
    elif drive != "C" and drive != "E":
        raise ValueError("%s: invalid drive letter" % drive)

    # Determine vendor name(s), use "Ensymble" by default.
    vendor = opts.get("--vendor", opts.get("-d", "Ensymble"))
    vendor = vendor.decode(terminalenc)
    vendor = vendor.split(",")
    if len(vendor) == 1:
        # Only one vendor name given, use it for all languages.
        vendor = vendor * numlang
    elif len(vendor) != numlang:
        raise ValueError("invalid number of vendor names")

    extrasdir = opts.get("--extrasdir", opts.get("-x", None))
    if extrasdir != None:
        extrasdir = extrasdir.decode(terminalenc).encode(filesystemenc)
        if extrasdir[-1] == os.sep:
            # Strip trailing slash (or backslash).
            extrasdir = extrasdir[:-1]

        if os.sep in extrasdir:
            raise ValueError("%s: too many path components" % extrasdir)

    # Load text files.
    texts = []
    textfile = opts.get("--textfile", opts.get("-t", None))
    if textfile != None:
        texts = readtextfiles(textfile, lang)

    # Get certificate and its private key file names.
    cert = opts.get("--cert", opts.get("-a", None))
    privkey = opts.get("--privkey", opts.get("-k", None))
    if cert != None and privkey != None:
        # Convert file names from terminal encoding to filesystem encoding.
        cert = cert.decode(terminalenc).encode(filesystemenc)
        privkey = privkey.decode(terminalenc).encode(filesystemenc)

        # Read certificate file.
        f = file(cert, "rb")
        certdata = f.read(MAXCERTIFICATELENGTH + 1)
        f.close()

        if len(certdata) > MAXCERTIFICATELENGTH:
            raise ValueError("certificate file too large")

        # Read private key file.
        f = file(privkey, "rb")
        privkeydata = f.read(MAXPRIVATEKEYLENGTH + 1)
        f.close()

        if len(privkeydata) > MAXPRIVATEKEYLENGTH:
            raise ValueError("private key file too large")
    elif cert == None and privkey == None:
        # No certificate given, use the Ensymble default certificate.
        # defaultcert.py is not imported when not needed. This speeds
        # up program start-up a little.
        from utils import defaultcert
        certdata = defaultcert.cert
        privkeydata = defaultcert.privkey

        print ("%s: warning: no certificate given, using "
               "insecure built-in one" % pgmname)

        # Warn if the UID is in the protected range.
        # Resulting SIS file will probably not install.
        if uid3 < 0x80000000L:
            print ("%s: warning: UID is in the protected range "
                   "(0x00000000 - 0x7ffffff)" % pgmname)
    else:
        raise ValueError("missing certificate or private key")

    # Get pass phrase. Pass phrase remains in terminal encoding.
    passphrase = opts.get("--passphrase", opts.get("-p", None))
    if passphrase == None and privkey != None:
        # Private key given without "--passphrase" option, ask it.
        if sys.stdin.isatty():
            # Standard input is a TTY, ask password interactively.
            passphrase = getpass.getpass("Enter private key pass phrase:")
        else:
            # Not connected to a TTY, read stdin non-interactively instead.
            passphrase = sys.stdin.read(MAXPASSPHRASELENGTH + 1)

            if len(passphrase) > MAXPASSPHRASELENGTH:
                raise ValueError("pass phrase too long")

            passphrase = passphrase.strip()

    # Get capabilities and normalize the names.
    caps = opts.get("--caps", opts.get("-b", ""))
    capmask = symbianutil.capstringtomask(caps)
    caps = symbianutil.capmasktostring(capmask, True)

    # Determine if the application is requested to start on each device boot.
    autostart = False
    if "--autostart" in opts.keys() or "-g" in opts.keys():
        autostart = True

    runinstall = False
    if "--runinstall" in opts.keys() or "-R" in opts.keys():
        runinstall = True

    # Get heap sizes.
    heapsize = opts.get("--heapsize", opts.get("-H", "4k,1M")).split(",", 1)
    try:
        heapsizemin = symbianutil.parseintmagnitude(heapsize[0])
        if len(heapsize) == 1:
            # Only one size given, use it as both.
            heapsizemax = heapsizemin
        else:
            heapsizemax = symbianutil.parseintmagnitude(heapsize[1])
    except (ValueError, TypeError, IndexError):
        raise ValueError("%s: invalid heap size, one or two values expected" %
                         ",".join(heapsize))

    # Warn if the minimum heap size is larger than the maximum heap size.
    # Resulting SIS file will probably not install.
    if heapsizemin > heapsizemax:
        print ("%s: warning: minimum heap size larger than "
               "maximum heap size" % pgmname)

    # Determine verbosity.
    verbose = False
    if "--verbose" in opts.keys() or "-v" in opts.keys():
        verbose = True

    # Determine if debug output is requested.
    if "--debug" in opts.keys():
        debug = True

        # Enable debug output for OpenSSL-related functions.
        import cryptutil
        cryptutil.setdebug(True)

    # Ingredients for successful SIS generation:
    #
    # terminalenc   Terminal character encoding (autodetected)
    # filesystemenc File system name encoding (autodetected)
    # basename      Base for generated file names on host, filesystemenc encoded
    # srcdir        Directory of source files, filesystemenc encoded
    # srcfiles      List of filesystemenc encoded source file names in srcdir
    # outfile       Output SIS file name, filesystemenc encoded
    # uid3          Application UID3, long integer
    # appname       Application name and install directory in device, in Unicode
    # version       A triple-item tuple (major, minor, build)
    # lang          List of two-character language codes, ASCII strings
    # icon          Icon data, a binary string typically containing a SVG-T file
    # shortcaption  List of Unicode short captions, one per language
    # caption       List of Unicode long captions, one per language
    # drive         Installation drive letter or "!"
    # extrasdir     Path prefix for extra files, filesystemenc encoded or None
    # textfile      File name pattern of text file(s) to display during install
    # texts         Actual texts to display during install, one per language
    # cert          Certificate in PEM format
    # privkey       Certificate private key in PEM format
    # passphrase    Pass phrase of private key, terminalenc encoded string
    # caps, capmask Capability names and bitmask
    # vendor        List of Unicode vendor names, one per language
    # autostart     Boolean requesting application autostart on device boot
    # runinstall    Boolean requesting application autorun after installation
    # heapsizemin   Heap that must be available for the application to start
    # heapsizemax   Maximum amount of heap the application can allocate
    # verbose       Boolean indicating verbose terminal output

    if verbose:
        print
        print "Input file(s)       %s"      % " ".join(
            [s.decode(filesystemenc).encode(terminalenc) for s in srcfiles])
        print "Output SIS file     %s"      % (
            outfile.decode(filesystemenc).encode(terminalenc))
        print "UID                 0x%08x"  % uid3
        print "Application name    %s"      % appname.encode(terminalenc)
        print "Version             %d.%d.%d"    % (
            version[0], version[1], version[2])
        print "Language(s)         %s"      % ", ".join(lang)
        print "Icon                %s"      % ((icon and
            icon.decode(filesystemenc).encode(terminalenc)) or "<default>")
        print "Short caption(s)    %s"      % ", ".join(
            [s.encode(terminalenc) for s in shortcaption])
        print "Long caption(s)     %s"      % ", ".join(
            [s.encode(terminalenc) for s in caption])
        print "Install drive       %s"      % ((drive == "!") and
            "<any>" or drive)
        print "Extras directory    %s"      % ((extrasdir and
            extrasdir.decode(filesystemenc).encode(terminalenc)) or "<none>")
        print "Text file(s)        %s"      % ((textfile and
            textfile.decode(filesystemenc).encode(terminalenc)) or "<none>")
        print "Certificate         %s"      % ((cert and
            cert.decode(filesystemenc).encode(terminalenc)) or "<default>")
        print "Private key         %s"      % ((privkey and
            privkey.decode(filesystemenc).encode(terminalenc)) or "<default>")
        print "Capabilities        0x%x (%s)" % (capmask, caps)
        print "Vendor name(s)      %s"      % ", ".join(
            [s.encode(terminalenc) for s in vendor])
        print "Autostart on boot   %s"      % ((autostart and "Yes") or "No")
        print "Run after install   %s"      % ((runinstall and "Yes") or "No")
        print "Heap size in bytes  %d, %d" % (heapsizemin, heapsizemax)
        print

    # Generate SimpleSISWriter object.
    sw = sisfile.SimpleSISWriter(lang, caption, uid3, version,
                                 vendor[0], vendor)

    # Add text file or files to the SIS object. Text dialog is
    # supposed to be displayed before anything else is installed.
    if len(texts) == 1:
        sw.addfile(texts[0], operation = sisfield.EOpText)
    elif len(texts) > 1:
        sw.addlangdepfile(texts, operation = sisfield.EOpText)

    # Generate "Python for S60" resource file.
    rsctarget = u"%s:\\resource\\apps\\%s_0x%08x.rsc" % (drive, appname, uid3)
    string = zlib.decompress(pythons60rscdata.decode("base-64"))
    sw.addfile(string, rsctarget)
    del string

    # Generate application registration resource file.
    regtarget = u"%s:\\private\\10003a3f\\import\\apps\\%s_0x%08x_reg.rsc" % (
        drive, appname, uid3)
    exename = u"%s_0x%08x" % (appname, uid3)
    locpath = u"\\resource\\apps\\%s_0x%08x_loc" % (appname, uid3)
    rw = rscfile.RSCWriter(uid2 = 0x101f8021, uid3 = uid3)
    # STRUCT APP_REGISTRATION_INFO from appinfo.rh
    res = rscfile.Resource(["LONG", "LLINK", "LTEXT", "LONG", "LTEXT", "LONG",
                            "BYTE", "BYTE", "BYTE", "BYTE", "LTEXT", "BYTE",
                            "WORD", "WORD", "WORD", "LLINK"],
                           0, 0, exename, 0, locpath, 1,
                           0, 0, 0, 0, "", 0,
                           0, 0, 0, 0)
    rw.addresource(res)
    string = rw.tostring()
    del rw
    sw.addfile(string, regtarget)
    del string

    # EXE target name
    exetarget = u"%s:\\sys\\bin\\%s_0x%08x.exe" % (drive, appname, uid3)

    # Generate autostart registration resource file, if requested.
    if autostart:
        autotarget = u"%s:\\private\\101f875a\\import\\[%08x].rsc" % (
            drive, uid3)
        rw = rscfile.RSCWriter(uid2 = 0, offset = "    ")
        # STRUCT STARTUP_ITEM_INFO from startupitem.rh
        res = rscfile.Resource(["BYTE", "LTEXT", "WORD",
                                "LONG", "BYTE", "BYTE"],
                               0, exetarget, 0, 0, 0, 0)
        rw.addresource(res)
        string = rw.tostring()
        del rw
        sw.addfile(string, autotarget)
        del string

    # Generate localisable icon/caption definition resource files.
    iconpath = "\\resource\\apps\\%s_0x%08x_aif.mif" % (appname, uid3)
    for n in xrange(numlang):
        loctarget = u"%s:\\resource\\apps\\%s_0x%08x_loc.r%02d" % (
            drive, appname, uid3, symbianutil.langidtonum[lang[n]])
        rw = rscfile.RSCWriter(uid2 = 0, offset = "    ")
        # STRUCT LOCALISABLE_APP_INFO from appinfo.rh
        res = rscfile.Resource(["LONG", "LLINK", "LTEXT",
                                "LONG", "LLINK", "LTEXT",
                                "WORD", "LTEXT", "WORD", "LTEXT"],
                               0, 0, shortcaption[n],
                               0, 0, caption[n],
                               1, iconpath, 0, "")
        rw.addresource(res)
        string = rw.tostring()
        del rw
        sw.addfile(string, loctarget)
        del string

    # Generate MIF file for icon.
    icontarget = "%s:\\resource\\apps\\%s_0x%08x_aif.mif" % (
        drive, appname, uid3)
    mw = miffile.MIFWriter()
    mw.addfile(icondata)
    del icondata
    string = mw.tostring()
    del mw
    sw.addfile(string, icontarget)
    del string

    # Add files to SIS object.
    if len(srcfiles) == 1:
        # Read file.
        f = file(os.path.join(srcdir, srcfiles[0]), "rb")
        string = f.read(MAXOTHERFILESIZE + 1)
        f.close()

        if len(string) > MAXOTHERFILESIZE:
            raise ValueError("%s: input file too large" % srcfiles[0])

        # Add file to the SIS object. One file only, rename it to default.py.
        target = "default.py"
        sw.addfile(string, "%s:\\private\\%08x\\%s" % (drive, uid3, target))
        del string
    else:
        if extrasdir != None:
            sysbinprefix = os.path.join(extrasdir, "sys", "bin", "")
        else:
            sysbinprefix = os.path.join(os.sep, "sys", "bin", "")

        # More than one file, use original path names.
        for srcfile in srcfiles:
            # Read file.
            f = file(os.path.join(srcdir, srcfile), "rb")
            string = f.read(MAXOTHERFILESIZE + 1)
            f.close()

            if len(string) > MAXOTHERFILESIZE:
                raise ValueError("%s: input file too large" % srcfile)

            # Split path into components.
            srcpathcomp = srcfile.split(os.sep)
            targetpathcomp = [s.decode(filesystemenc) for s in srcpathcomp]

            # Check if the file is an E32Image (EXE or DLL).
            filecapmask = symbianutil.e32imagecaps(string)

            # Warn against common mistakes when dealing with E32Image files.
            if filecapmask != None:
                if not srcfile.startswith(sysbinprefix):
                    # Warn against E32Image files outside /sys/bin.
                    print ("%s: warning: %s is an E32Image (EXE or DLL) "
                           "outside %s" % (pgmname, srcfile, sysbinprefix))
                elif (symbianutil.ise32image(string) == "DLL" and
                      (filecapmask & ~capmask) != 0x00000000L):
                    # Warn about insufficient capabilities to load
                    # a DLL from the PyS60 application.
                    print ("%s: warning: insufficient capabilities to "
                           "load %s" % (pgmname, srcfile))

            # Handle the extras directory.
            if extrasdir != None and extrasdir == srcpathcomp[0]:
                # Path is rooted at the drive root.
                targetfile = u"%s:\\%s" % (drive, "\\".join(targetpathcomp[1:]))
            else:
                # Path is rooted at the application private directory.
                targetfile = u"%s:\\private\\%08x\\%s" % (
                    drive, uid3, "\\".join(targetpathcomp))

            # Add file to the SIS object.
            sw.addfile(string, targetfile, capabilities = filecapmask)
            del string

    # Add target device dependency.
    sw.addtargetdevice(0x101f7961L, (0, 0, 0), None,
                       ["Series60ProductID"] * numlang)

    # Add "Python for S60" dependency, version 1.4.0 onwards.
    # NOTE: Previous beta versions of Python for S60 had a
    # different UID3 (0xf0201510).
    sw.adddependency(0x2000b1a0L, (1, 4, 0), None,
                     ["Python for S60"] * numlang)

    # Add certificate.
    sw.addcertificate(privkeydata, certdata, passphrase)

    # Generate an EXE stub and add it to the SIS object.
    string = execstubdata.decode("base-64")
    string = symbianutil.e32imagecrc(string, uid3, uid3, None,
                                     heapsizemin, heapsizemax, capmask)
    if runinstall:
        # To avoid running without dependencies, this has to be in the end.
        sw.addfile(string, exetarget, None, capabilities = capmask,
                   operation = sisfield.EOpRun,
                   options = sisfield.EInstFileRunOptionInstall)
    else:
        sw.addfile(string, exetarget, None, capabilities = capmask)

    del string

    # Generate SIS file out of the SimpleSISWriter object.
    sw.tofile(outfile)


##############################################################################
# Module-level functions which are normally only used by this module
##############################################################################

def scandefaults(filename):
    '''Scan a Python source file for application version string and UID3.'''

    version = None
    uid3    = None

    # Regular expression for the version string. Version may optionally
    # be enclosed in double or single quotes.
    version_ro = re.compile(r'SIS_VERSION\s*=\s*(?:(?:"([^"]*)")|'
                            r"(?:'([^']*)')|(\S+))")

    # Original py2is uses a regular expression
    # r"SYMBIAN_UID\s*=\s*(0x[0-9a-fA-F]{8})".
    # This version is a bit more lenient.
    uid3_ro = re.compile(r"SYMBIAN_UID\s*=\s*(\S+)")

    # First match of each regular expression is used.
    f = file(filename, "rb")
    try:
        while version == None or uid3 == None:
            line = f.readline()
            if line == "":
                break
            if version == None:
                mo = version_ro.search(line)
                if mo:
                    # Get first group that matched in the regular expression.
                    version = filter(None, mo.groups())[0]
            if uid3 == None:
                mo = uid3_ro.search(line)
                if mo:
                    uid3 = mo.group(1)
    finally:
        f.close()
    return version, uid3

def parseversion(version):
    '''Parse a version string: "v1.2.3" or similar.

    Initial "v" can optionally be a capital "V" or omitted altogether. Minor
    and build numbers can also be omitted. Separator can be a comma or a
    period.'''

    version = version.strip().lower()

    # Strip initial "v" or "V".
    if version[0] == "v":
        version = version[1:]

    if "." in version:
        parts = [int(n) for n in version.split(".")]
    else:
        parts = [int(n) for n in version.split(",")]

    # Allow missing minor and build numbers.
    parts.extend([0, 0])

    return parts[0:3]

def readtextfiles(pattern, languages):
    '''Read language dependent text files.

    Files are assumed to be in UTF-8 encoding and re-encoded
    in UCS-2 (UTF-16LE) for Symbian OS to display during installation.'''

    if "%" not in pattern:
        # Only one file, read it.
        filenames = [pattern]
    else:
        filenames = []
        for langid in languages:
            langnum  = symbianutil.langidtonum[langid]
            langname = symbianutil.langnumtoname[langnum]

            # Replace formatting characters in file name pattern.
            filename = pattern
            filename = filename.replace("%n", "%02d" % langnum)
            filename = filename.replace("%c", langid.lower())
            filename = filename.replace("%C", langid.upper())
            filename = filename.replace("%l", langname.lower())
            filename = filename.replace("%L", langname)
            filename = filename.replace("%%", "%")

            filenames.append(filename)

    texts = []

    for filename in filenames:
        f = file(filename, "r") # Read as text.
        text = f.read(MAXTEXTFILELENGTH + 1)
        f.close()

        if len(text) > MAXTEXTFILELENGTH:
            raise ValueError("%s: text file too large" % filename)

        texts.append(text.decode("UTF-8").encode("UTF-16LE"))

    return texts

##############################################################################
# Embedded data: EXE stub, private key and icon, application resource
##############################################################################

# This is the Symbian application stub, which starts the Python interpreter
# and loads default.py. It is represented here as a base-64-encoded string.
# The stub needs to be patched with correct UID3, Secure ID and capabilities.
# After that, a couple of checksums need to be updated. Function
# e32imagecrc(...) in module symbianutils takes care of all that.
#
# This stub will examine process SID and report that as the UID3 for the
# GUI framework. Therefore, no code parts need to be patched at all.
execstubdata = '''
    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
    ey9synTLgyLZTMIy/ElM4mOC8BOZP3wLw+SmeV7oa+yUmY/uwI9n7xn8KZI+CDTufgg07l/KUzWI
    Jn4BwP5Q6fBg2exP0/pQWi9ksJr1hcvWDrrC22B48cTVItH+sDe5SgwD7AYMO4CZ2EMdukMZVhZX
    AwVhcHWFUdeL+deGXXj7xxVulBg3sq8V314S44Q44bccez8DcAfojix/HHqnjja44a6wR1hbfWCu
    LQvopQW3+wGXslht6wU1haLWHs3A26wvfrCJYGKsF+BvFg+4FBYew9YMtYV5gcJGLCLXiwvXjFeB
    4McK8cP/HGIMa/vI4XLBs9YQfjhrxxBuOKI1glrCH6w7b1hwsC2WF88CtTghfGWGO+VK6LSWv9jD
    DGoYHz+37POD/2g1Gg1XV562t7coPbH/PqJcvY9bxTzPaq7uy1HX/a12tyShzvPannurf1dVr9hq
    NXqOsyaZq+t+1qddr9Xkk6ys9Dn+rg19j12r1+Tc3Q6vnYNXrtTLl63+rVXeSYmwP8L0X97Y5p1e
    yVoK4yun5S65XVZnccrZ10WOrHx4/uR7ZaitrK/u5DMjvZG3keDInSNxI8WR48ifI8qRupHnSIL5
    jBwbjyI9hHXj6yPdx9UtrFrtbsVvBW3C3ireOtjLKV8ivs6/T19zyX//'''

# Python logo as a base-64-encoded, zlib-compressed SVG XML data
defaulticondata = '''
    eJyFVF1v20YQfC/Q/3Bl0be74+3tfQZRA1h24gJJa6COij66EiMSdSVDUiW3vz5zRyp2CgMVLHrJ
    /ZjZmRNfv3n8614cu91+2G5mDWnTiG6z3K6GzXrWfLx9q1Lz5sdvv3n93eUv89vfb67E/rgWNx8v
    3v80F41q29943raXt5fi18U7QZrE7bD5p22vfm5E0x8OD6/a9nQ66RPr7W7dvtvdPfTDct+iukV1
    6WwxkUgd0KdXh1VT0ArIH3f77ma3/TTcd7OmZJvnPKkRYL7Zv9qD6wO+szPc+YHeb//eLbtPwO30
    pjuMWFNSmYo1zpi9wNQaYwqzM8zj/bD586VCyjm3NduI07A69GBnzA+N6Lth3R/Od8ehO11sH2eN
    EUYEh7+66LpcHu4OvcCe97Pmew4hZ9eI1az5QEln5yVZ7YxfGsXaJyeNzoGU156dDNqGpIJ2gZes
    g2HsFZhk0tZaxJFKt8cTI4RAiTOMAT7Y2pola5PjFNcxC+u05aVBxmG01TERMkzqXMR0bb22ZJcK
    pd5Ko6JOHNERbJjiqKP1R69DdD3K2OSCjw2CwwZgH0PA8GAL++DLlbGjIm2NRUN2CMlTGVd2Vlgj
    EETQOSfkyUaJa5oaZUHlMe6djoavmbRLR0zxsWBfj2IuRjHffyXtv037Xxuu4oVnM9rgnDZkpTfa
    ulSVgQ1YhYik15zTkzRlBcC7LElz8CpBaliATYJ6bgS6mbwqVgY1mmh15qzOhmLSgpN21lbfnbHS
    FmFLir7YztSPU5fQIkKmqkMsMpswxUVAeyznhQKkwekaT0JwCfXgHyJGR5qmTlvAB89QOOdCH/bh
    SEVbECYjilOoZh1jqka6sVM9m9KPN5MVxYkE7InyYtTzBe3f1s+ovbXaRKhJQIASVLbHS8plYDJw
    cPVjWChnD4K4q/obn6a45svWpu7iVUm6+pjVUwnPLUy1SRLrnFieseGM9fI5k/8hzQFuZOkBwzyy
    xhGtoJWre6LtKu080q41YYxq8olzrpypPo7qS0Wcc8TPFYcTZ0SecctKVn7FYmLc1vdNea/h/2dD
    N2YO'''

# "Python for S60" compiled resource as a base-64-encoded, zlib-compressed data
pythons60rscdata = '''
    eJzL9pIXYACCVnFWhouea4oYtRk4QHwWIGYMqAgEs6E0CEgxnOGAsRnYGRlYgXKcnK4VJal5xZn5
    eYJg8f9AwDDkgQSDAhDaMCQypDPkMhQzVDLUM7QydDNMZJjOMJdhMcNKhvUMWxl2MxxkOM5wluEy
    w02G+wxPGV4zfGT4zvCXgZmRk5GfUZRRmhEAjnEjdg=='''
