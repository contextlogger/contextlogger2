#include "sa_sensor_light_qt.hpp"
#include "sa_sensor_light_api.h"

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"

#include <qambientlightsensor.h>

Sensor_light::Sensor_light(ac_AppContext* aAppContext) :
  ClQtEventSensorBase(QAmbientLightSensor::type, aAppContext)
{
  run();
}

const char* Sensor_light::Name() const
{
  return "light";
}

void Sensor_light::handleReadingChanged()
{
  QAmbientLightReading *data(static_cast<QAmbientLightReading*>(reading()));
  if (data) {
    int level = data->lightLevel();
    guilogf("light: %d", level);
    log_db_log_light(GetLogDb(), level, NULL);
  }
}

Sensor_light* new_Sensor_light(ac_AppContext* aAppContext)
{
  return q_check_ptr(new Sensor_light(aAppContext));
}

void delete_Sensor_light(Sensor_light* obj)
{
  delete obj;
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
