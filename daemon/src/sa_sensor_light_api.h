#ifndef __sa_sensor_light_api_h__
#define __sa_sensor_light_api_h__

#include "ac_app_context.h"
#include "application_config.h"

class Sensor_light;

Sensor_light* new_Sensor_light(ac_AppContext* aAppContext);

void delete_Sensor_light(Sensor_light* obj);

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __LIGHT_ENABLED__
#define DECLARE_SENSOR_light Sensor_light* iSensor_light
#define SENSOR_LIGHT_DESTROY delete_Sensor_light(self->iSensor_light); self->iSensor_light = NULL;
#define SENSOR_LIGHT_CREATE sa_typical_qt_sensor_create(self->iSensor_light = new_Sensor_light(self->ac), "light sensor initialization")
#define SENSOR_LIGHT_START SENSOR_LIGHT_CREATE
#define SENSOR_LIGHT_STOP SENSOR_LIGHT_DESTROY
#define SENSOR_LIGHT_IS_RUNNING (self->iSensor_light != NULL)
#define SENSOR_LIGHT_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_light
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __sa_sensor_light_api_h__ */

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
