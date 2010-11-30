#ifndef __sa_sensor_tap_h__
#define __sa_sensor_tap_h__

#include "application_config.h"

#if __TAP_ENABLED__

#include "ac_app_context.h"

#include <QObject>

#include <qtapsensor.h>

QTM_USE_NAMESPACE

// Note: We cannot say NONSHARABLE_CLASS here as it would confuse "moc".
class Sensor_tap :
  public QTapSensor
{
  Q_OBJECT

 public:
  Sensor_tap(ac_AppContext* aAppContext);

 private:
  ac_AppContext* iAppContext; // not owned
  
 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  const char* Name() const { return "tap"; }

 private slots:
  void handleActiveChanged();
  void handleBusyChanged();
  void handleReadingChanged();
  void handleSensorError(int errCode);
};

#endif /* __TAP_ENABLED__ */

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __TAP_ENABLED__
#define DECLARE_SENSOR_tap Sensor_tap* iSensor_tap
#define SENSOR_TAP_DESTROY DELETE_Z(self->iSensor_tap)
#define SENSOR_TAP_CREATE sa_typical_qt_sensor_create(self->iSensor_tap = q_check_ptr(new Sensor_tap(self->ac)), "tap sensor initialization")
#define SENSOR_TAP_START SENSOR_TAP_CREATE
#define SENSOR_TAP_STOP SENSOR_TAP_DESTROY
#define SENSOR_TAP_IS_RUNNING (self->iSensor_tap != NULL)
#define SENSOR_TAP_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_tap
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __sa_sensor_tap_h__ */

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
