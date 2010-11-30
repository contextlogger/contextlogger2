#include "sa_qt_sensors.hpp"

#include "er_errors.h"
#include "ld_logging.h"

// http://doc.qt.nokia.com/qtmobility-1.1.0/qtsensors.html

ClQtEventSensorBase::ClQtEventSensorBase(const QByteArray & type,
					 ac_AppContext* aAppContext) :
  QSensor(type), iAppContext(aAppContext)
{
}

void ClQtEventSensorBase::run()
{
  // Signals cannot be "overridden" directly like methods, but we can
  // connect to slots locally.
  connect(this, SIGNAL(activeChanged()), this, SLOT(handleActiveChanged()));
  connect(this, SIGNAL(busyChanged()), this, SLOT(handleBusyChanged()));
  connect(this, SIGNAL(readingChanged()), this, SLOT(handleReadingChanged()));
  connect(this, SIGNAL(sensorError(int)), this, SLOT(handleSensorError(int)));

  if (!isBusy())
    start();
}

void ClQtEventSensorBase::handleActiveChanged()
{
  const char* status = (isActive() ? "activated" : "disactivated");
  log_db_log_status(GetLogDb(), NULL, 
		    "STATUS: %s sensor %s", Name(), status);
}

void ClQtEventSensorBase::handleBusyChanged()
{
  const char* status = (isBusy() ? "busy" : "available");
  log_db_log_status(GetLogDb(), NULL, 
		    "STATUS: %s sensor hardware %s", Name(), status);
  if (!isActive() && !isBusy())
    start();
}

void ClQtEventSensorBase::handleSensorError(int errCode)
{
  log_db_log_status(GetLogDb(), NULL, 
		    "ERROR: error in %s sensor: %d", 
		    Name(), errCode);
}

/*
void ClQtEventSensorBase::handleReadingChanged()
{
  logt("ClQtEventSensorBase: reading changed");
  }*/

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
