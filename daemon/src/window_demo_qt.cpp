#include "window_demo_qt.hpp"
#include "guilog.h"

#include "application_config.h"
#include "up_uploader.h"

#include "common/QsKineticScroller.hpp"

#include <QAbstractListModel>
#include <QAction>
#include <QApplication>
#include <QErrorMessage>
#include <QListView>
#include <QMenu>
#include <QMenuBar>
#include <QObject>
#include <QString>
#include <QStringListModel>

#include <QtGlobal>

#include <stdio.h>

#define MY_MAX_ROWS 100

static QStringListModel* gModel = NULL;

#if __FEATURE_GUILOG__

void guilog(const QString& s)
{
  int count = gModel->rowCount();
  if (count >= MY_MAX_ROWS) {
    int newCount = count * 3 / 4;
    gModel->removeRows(newCount, count - newCount);
  }

  int ix = 0;
  gModel->insertRow(ix);
  gModel->setData(gModel->index(ix), s, Qt::DisplayRole);
}

void guilog(const char* s)
{
  guilog(QString(s));
}

extern "C" void guilogf(const char* fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
  char buf[256];
  vsnprintf(buf, 256, fmt, argp);
  guilog(buf);
  va_end(argp);
}

#endif

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
  listView = new QListView(this);
  // Required by QsKineticScroller.
  listView->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
  iScroller = new QsKineticScroller(this);
  iScroller->enableKineticScrollFor(listView);
  // The view does not take ownership of the model.
  gModel = new QStringListModel(stringList, listView);
  listView->setModel(gModel);
  listView->setViewMode(QListView::ListMode);
  listView->setSelectionMode(QAbstractItemView::SingleSelection);
  listView->setEditTriggers(QAbstractItemView::NoEditTriggers);

  setCentralWidget(listView);

  QAction *quitAction = new QAction(tr("&Quit"), this);
  quitAction->setShortcuts(QKeySequence::Quit); // C-q on Linux
  quitAction->setStatusTip(tr("Stop logger and quit"));
  connect(quitAction, SIGNAL(triggered()), qApp, SLOT(quit()));
  addAction(quitAction);

  QAction* uploadAction = new QAction(tr("&Upload now"), this);
  uploadAction->setStatusTip(tr("Take log snapshot and upload now"));
  connect(uploadAction, SIGNAL(triggered()), this, SLOT(uploadNow()));

  QMenuBar* actionMenu = menuBar();
  //QMenu* actionMenu = menuBar()->addMenu(tr("&Action"));
  actionMenu->addAction(uploadAction);
  actionMenu->addSeparator();
  actionMenu->addAction(quitAction);
}

static void showFreeGerror(GError* error)
{
  if (error) {
    gchar* s = gx_error_to_string(error);
    gx_error_free(error);
    QErrorMessage::qtHandler()->showMessage(QString(s));
    g_free(s);
  } else {
    QErrorMessage::qtHandler()->showMessage(QString("out of memory"));
  }
}

void MainWindow::uploadNow()
{
#if __FEATURE_UPLOADER__
  up_Uploader* uploader = ac_global_Uploader;
  GError* localError = NULL;
  if (!up_Uploader_upload_now(uploader, &localError)) {
    showFreeGerror(localError);
  }
#endif
}
