include(qxmpp.pri)

TEMPLATE = subdirs

SUBDIRS = src \
          tests \
          examples \
          doc

CONFIG += ordered

# Documentation generation
docs.commands = $(MAKE) -C doc docs

# Source distribution
QXMPP_ARCHIVE = qxmpp-0.2.92
dist.commands = \
    $(DEL_FILE) -r $$QXMPP_ARCHIVE && \
    svn export . $$QXMPP_ARCHIVE && \
    $(COPY_DIR) doc/html $$QXMPP_ARCHIVE/doc && \
    tar czf $${QXMPP_ARCHIVE}.tar.gz $$QXMPP_ARCHIVE && \
    $(DEL_FILE) -r $$QXMPP_ARCHIVE
dist.depends = docs

# Install rules
htmldocs.files = doc/html
htmldocs.path = $$[QT_INSTALL_PREFIX]/share/doc/qxmpp
htmldocs.CONFIG += no_check_exist directory

QMAKE_EXTRA_TARGETS += dist docs
INSTALLS += htmldocs
