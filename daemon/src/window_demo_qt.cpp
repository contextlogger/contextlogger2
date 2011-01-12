#include "window_demo_qt.hpp"
#include "guilog.h"

#include <QAbstractListModel>
#include <QAction>
#include <QApplication>
#include <QListView>
#include <QObject>
#include <QStringListModel>

#include <QtGlobal>

static QStringListModel* gModel = NULL;

#define MAX_ROWS 100

void guilog(const QString& s)
{
  int count = gModel->rowCount();
  if (count >= MAX_ROWS) {
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

#include "common/QsKineticScroller.hpp"

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

  QAction *exitAction = new QAction("Exit", this);
  exitAction->setShortcuts(QKeySequence::Quit);
  connect(exitAction, SIGNAL(triggered()), qApp, SLOT(quit()));
  addAction(exitAction);
}
