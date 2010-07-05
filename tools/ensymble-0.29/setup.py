#!/usr/bin/env python
# -*- coding: utf-8 -*-

from distutils.core import setup
import ensymble

name    = "ensymble"
version = ensymble.__version__
license = ensymble.__license__
author  = ensymble.__author__
maintainer = ensymble.__maintainer__
maintainer_email = ensymble.__email__
long_description = ensymble.__doc__

url = "http://code.google.com/p/ensymble/"
download_url = "%s/detail?name=%s-%s.tar.gz" % (url, name, version)

setup( name = name,
       version = version,
       description = "Tools to make PyS60 applications for Symbian S60 phones",
       author = author,
       maintainer = maintainer,
       maintainer_email = maintainer_email,
       url = url,
       long_description = long_description,
       download_url = download_url,
       packages = ['ensymble', 'ensymble.actions', 'ensymble.utils'],
       scripts  = ['ensymble.py']
    )
