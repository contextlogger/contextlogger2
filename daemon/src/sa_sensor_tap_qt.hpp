#ifndef __sa_sensor_tap_h__
#define __sa_sensor_tap_h__

#include "application_config.h"

#if __WITH_TAP_SENSORS__

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
  Sensor_tap(ac_AppContext* aAppContext,
	     bool aDouble, const char* aName);

 private:
  ac_AppContext* iAppContext; // not owned
  bool iDouble;
  const char* iName; // not owned
  
 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  const char* Name() const { return iName; }

 private slots:
  void handleActiveChanged();
  void handleBusyChanged();
  void handleReadingChanged();
  void handleSensorError(int errCode);
};

#endif /* __WITH_TAP_SENSORS__ */

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)

#if __SINGLETAP_ENABLED__
#define DECLARE_SENSOR_singletap Sensor_tap* iSensor_singletap
#define SENSOR_SINGLETAP_DESTROY DELETE_Z(self->iSensor_singletap)
#define SENSOR_SINGLETAP_CREATE sa_typical_qt_sensor_create(self->iSensor_singletap = q_check_ptr(new Sensor_tap(self->ac, false, "singletap")), "singletap sensor initialization")
#define SENSOR_SINGLETAP_START SENSOR_SINGLETAP_CREATE
#define SENSOR_SINGLETAP_STOP SENSOR_SINGLETAP_DESTROY
#define SENSOR_SINGLETAP_IS_RUNNING (self->iSensor_singletap != NULL)
#define SENSOR_SINGLETAP_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_singletap
#endif // __SINGLETAP_ENABLED__

#if __DOUBLETAP_ENABLED__
#define DECLARE_SENSOR_doubletap Sensor_tap* iSensor_doubletap
#define SENSOR_DOUBLETAP_DESTROY DELETE_Z(self->iSensor_doubletap)
#define SENSOR_DOUBLETAP_CREATE sa_typical_qt_sensor_create(self->iSensor_doubletap = q_check_ptr(new Sensor_tap(self->ac, true, "doubletap")), "doubletap sensor initialization")
#define SENSOR_DOUBLETAP_START SENSOR_DOUBLETAP_CREATE
#define SENSOR_DOUBLETAP_STOP SENSOR_DOUBLETAP_DESTROY
#define SENSOR_DOUBLETAP_IS_RUNNING (self->iSensor_doubletap != NULL)
#define SENSOR_DOUBLETAP_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_doubletap
#endif // __DOUBLETAP_ENABLED__

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
