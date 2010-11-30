#ifndef __sa_sensor_light_h__
#define __sa_sensor_light_h__

#include "application_config.h"

#if __LIGHT_ENABLED__

#include "ac_app_context.h"
#include "sa_qt_sensors.hpp"

QTM_USE_NAMESPACE

// Note: We cannot say NONSHARABLE_CLASS here as it would confuse "moc".
class Sensor_light :
  public ClQtEventSensorBase
{
  Q_OBJECT

 public:
  Sensor_light(ac_AppContext* aAppContext);

 private:
  virtual const char* Name() const;

 private:
  virtual void handleReadingChanged();
};

#endif /* __LIGHT_ENABLED__ */

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __LIGHT_ENABLED__
#define DECLARE_SENSOR_light Sensor_light* iSensor_light
#define SENSOR_LIGHT_DESTROY DELETE_Z(self->iSensor_light)
#define SENSOR_LIGHT_CREATE sa_typical_qt_sensor_create(self->iSensor_light = q_check_ptr(new Sensor_light(self->ac)), "light sensor initialization")
#define SENSOR_LIGHT_START SENSOR_LIGHT_CREATE
#define SENSOR_LIGHT_STOP SENSOR_LIGHT_DESTROY
#define SENSOR_LIGHT_IS_RUNNING (self->iSensor_light != NULL)
#define SENSOR_LIGHT_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_light
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __sa_sensor_light_h__ */

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