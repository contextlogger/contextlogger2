#include "iodeviceseq_qt.hpp"

#include "common/assertions.h"

QIODeviceSeq::QIODeviceSeq(const QList<QIODevice*>& aList) :
  iList(aList), iIterator(iList), iDevice(NULL), iAtEnd(false)
{
  // This device is open to begin with, and it need not and cannot be
  // closed.
  setOpenMode(QIODevice::ReadOnly);
}

QIODeviceSeq::~QIODeviceSeq()
{
}

bool QIODeviceSeq::isSequential() const
{
  return true;
}

bool QIODeviceSeq::atEnd() const
{
  return iAtEnd;
}

// Note that we never emit readyRead() as we have all the data to
// begin with, and therefore new data will not become available.
// 
// The documentation for this interface is confusing wrt to what to
// return if there is no more data available; is it 0 or -1. How to
// find out if an error has occurred? readData is not a public API,
// sure, but it is pure virtual and so must be implemented by anyone
// creating a QIODevice.
// 
// The best bet is to look at the source code of QIODevice::read() and
// the implementations of QBuffer and QFile to make sure we get this
// right enough for our use case. QBuffer never gives a read error.
// QFile returns -1 only for actual errors, not for EOF, so we do the
// same.
qint64 QIODeviceSeq::readData(char *data, qint64 maxlen)
{
  if (iAtEnd) {
    setErrorString("read past end");
    return -1;
  }

  forever {
    if (!iDevice) {
      if (!iIterator.hasNext()) {
	iAtEnd = true;
	return 0;
      }
      iDevice = iIterator.next();
      assert(iDevice);
    }

    qint64 r = iDevice->read(data, maxlen);
    switch (r)
      {
      case -1:
        {
          setErrorString(iDevice->errorString());
	  return -1;
        }
      case 0:
        {
	  // No more data from this device.
	  iDevice = NULL;
          break;
        }
      default:
        {
	  // Got some data
	  return r;
        }
      }
  }

  assert(0 && "logic error");
  return -1;
}

// pure virtual, so must implement even though not supported
qint64 QIODeviceSeq::writeData(const char *data, qint64 len)
{
  return -1;
}

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
