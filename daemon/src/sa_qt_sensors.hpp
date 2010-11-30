#ifndef __sa_qt_sensors_h__
#define __sa_qt_sensors_h__

#include "application_config.h"

#if __USE_QT_SENSORS__

/** QtSensors related utilities. 
 */

#include "ac_app_context.h"

#include <qsensor.h>

QTM_USE_NAMESPACE

class ClQtEventSensorBase :
  public QSensor
{
  Q_OBJECT

 public:
  ClQtEventSensorBase(const QByteArray & type,
		      ac_AppContext* aAppContext);

 protected:
  ac_AppContext* iAppContext; // not owned
  
 protected:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  virtual const char* Name() const = 0;

  void run();

 public slots:
  void handleActiveChanged();
  void handleBusyChanged();
  void handleSensorError(int errCode);
  virtual void handleReadingChanged() = 0;
};

#endif /* __USE_QT_SENSORS__ */

#endif /* __sa_qt_sensors_h__ */

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
