#ifndef __iodeviceseq_qt_hpp__
#define __iodeviceseq_qt_hpp__

#include <QIODevice>
#include <QList>
#include <QListIterator>

// A composition of QIODevices, itself a QIODevice.
// Sequentially reads data from each of the devices.
// http://doc.qt.nokia.com/latest/qiodevice.html
class QIODeviceSeq :
  public QIODevice
{
  Q_OBJECT

 public:
  // Note that QList is a shared class, and hence passing like this is
  // efficient. Note that both QFile and QBuffer are subclasses of
  // QIODevice. Ownership of list elements is not taken. Devices must
  // be initialized and ready for reading.
  QIODeviceSeq(const QList<QIODevice*>& aList);
  ~QIODeviceSeq();

 private:
  QList<QIODevice*> iList;
  QListIterator<QIODevice*> iIterator;
  QIODevice* iDevice;
  bool iAtEnd;

public: // QIODevice
  virtual bool isSequential() const;
  virtual qint64 readData(char *data, qint64 maxlen);
  virtual qint64 writeData(const char *data, qint64 len);
  virtual bool atEnd() const;
};

#endif /* __iodeviceseq_qt_hpp__ */

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/
