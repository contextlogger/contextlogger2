#include "sa_sensor_light_qt.hpp"

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"

Sensor_light::Sensor_light(ac_AppContext* aAppContext) :
  iAppContext(aAppContext), iSensor(NULL)
{
  iSensor = q_check_ptr(new QAmbientLightSensor());
  connect(iSensor, SIGNAL(readingChanged()), this, SLOT(handleSensorEv()));
  connect(iSensor, SIGNAL(sensorError(int)), this, SLOT(handleSensorError(int)));
  iSensor->start();
}

Sensor_light::~Sensor_light()
{
  delete iSensor;
}

void Sensor_light::handleSensorEv()
{
  QAmbientLightReading* data(iSensor->reading()); // not owned
  if (data) {
    int level = data->lightLevel();
    logg("got light reading: %d", level);
    log_db_log_light(GetLogDb(), level, NULL);
  }
}

void Sensor_light::handleSensorError(int errCode)
{
  if (iSensor->isActive()) {
    log_db_log_status(GetLogDb(), NULL, 
		      "WARNING: transient error %d in light sensor", errCode);
  } else {
    log_db_log_status(GetLogDb(), NULL, 
		      "ERROR: light sensor stopped due to error %d", errCode);
  }
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
