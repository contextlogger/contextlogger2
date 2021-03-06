#ifndef __sa_sensor_proximity_api_h__
#define __sa_sensor_proximity_api_h__

#include "ac_app_context.h"
#include "application_config.h"

class Sensor_proximity;

Sensor_proximity* new_Sensor_proximity(ac_AppContext* aAppContext);

void delete_Sensor_proximity(Sensor_proximity* obj);

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __PROXIMITY_ENABLED__
#define DECLARE_SENSOR_proximity Sensor_proximity* iSensor_proximity
#define SENSOR_PROXIMITY_DESTROY delete_Sensor_proximity(self->iSensor_proximity); self->iSensor_proximity = NULL;
#define SENSOR_PROXIMITY_CREATE 
#define SENSOR_PROXIMITY_START sa_typical_qt_sensor_create(self->iSensor_proximity = new_Sensor_proximity(self->ac), "proximity sensor initialization")
#define SENSOR_PROXIMITY_STOP SENSOR_PROXIMITY_DESTROY
#define SENSOR_PROXIMITY_IS_RUNNING (self->iSensor_proximity != NULL)
#define SENSOR_PROXIMITY_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_proximity
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __sa_sensor_proximity_api_h__ */

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
