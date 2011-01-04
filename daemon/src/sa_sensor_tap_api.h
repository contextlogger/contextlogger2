#ifndef __sa_sensor_tap_api_h__
#define __sa_sensor_tap_api_h__

#include "ac_app_context.h"
#include "application_config.h"

class Sensor_tap;

Sensor_tap* new_Sensor_tap(ac_AppContext* aAppContext,
			   bool aDouble, const char* aName);

void delete_Sensor_tap(Sensor_tap* obj);

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)

#if __SINGLETAP_ENABLED__
#define DECLARE_SENSOR_singletap Sensor_tap* iSensor_singletap
#define SENSOR_SINGLETAP_DESTROY delete_Sensor_tap(self->iSensor_singletap); self->iSensor_singletap = NULL;
#define SENSOR_SINGLETAP_CREATE sa_typical_qt_sensor_create(self->iSensor_singletap = new_Sensor_tap(self->ac, false, "singletap"), "singletap sensor initialization")
#define SENSOR_SINGLETAP_START SENSOR_SINGLETAP_CREATE
#define SENSOR_SINGLETAP_STOP SENSOR_SINGLETAP_DESTROY
#define SENSOR_SINGLETAP_IS_RUNNING (self->iSensor_singletap != NULL)
#define SENSOR_SINGLETAP_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_singletap
#endif // __SINGLETAP_ENABLED__

#if __DOUBLETAP_ENABLED__
#define DECLARE_SENSOR_doubletap Sensor_tap* iSensor_doubletap
#define SENSOR_DOUBLETAP_DESTROY delete_Sensor_tap(self->iSensor_doubletap); self->iSensor_doubletap = NULL;
#define SENSOR_DOUBLETAP_CREATE sa_typical_qt_sensor_create(self->iSensor_doubletap = new_Sensor_tap(self->ac, true, "doubletap"), "doubletap sensor initialization")
#define SENSOR_DOUBLETAP_START SENSOR_DOUBLETAP_CREATE
#define SENSOR_DOUBLETAP_STOP SENSOR_DOUBLETAP_DESTROY
#define SENSOR_DOUBLETAP_IS_RUNNING (self->iSensor_doubletap != NULL)
#define SENSOR_DOUBLETAP_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_doubletap
#endif // __DOUBLETAP_ENABLED__

#endif // defined(SA_ARRAY_INTEGRATION)

#endif /* __sa_sensor_tap_api_h__ */

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
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

