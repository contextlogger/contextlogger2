#include "sa_sensor_proximity_qt.hpp"
#include "sa_sensor_proximity_api.h"

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"

#include <qproximitysensor.h>

// http://doc.qt.nokia.com/qtmobility-1.1.0/qproximitysensor.html

Sensor_proximity::Sensor_proximity(ac_AppContext* aAppContext) :
  ClQtEventSensorBase(QProximitySensor::type, aAppContext),
  iNumProximityData(0)
{
  run();
}

Sensor_proximity::~Sensor_proximity()
{
  logAndClear();
}

const char* Sensor_proximity::Name() const
{
  return "proximity";
}

void Sensor_proximity::handleReadingChanged()
{
  QProximityReading *data(static_cast<QProximityReading*>(reading()));
  if (data) {
    bool isClose = data->close();
    guilogf("proximity: %s", isClose ? "close" : "far");
    //log_db_log_proximity(GetLogDb(), isClose, NULL);
    record(isClose);
  }
}

void Sensor_proximity::record(bool value)
{
  time_t now = time(NULL); 
  if (now == -1)
    return; // error getting time

  TProximityData& rec = iProximityData[iNumProximityData];
  rec.t = now;
  rec.v = value;
  iNumProximityData++;

  if (iNumProximityData == MAX_NUM_ProximityData)
    logAndClear();
}

void Sensor_proximity::makeString()
{
  iString.clear();
  time_t base = iProximityData[0].t;
  iString.append("{base: ");
  iString.append(QByteArray::number(base));
  iString.append(", events: [");
  int i = 0;
  while (i < iNumProximityData) {
    TProximityData& rec = iProximityData[i];
    if (i != 0)
      iString.append(", ");
    iString.append("[");
    iString.append(QByteArray::number(rec.t - base));
    iString.append(", ");
    iString.append(rec.v ? "true" : "false");
    iString.append("]");
    i++;
  }
  iString.append("]}");
  iNumProximityData = 0;
}

void Sensor_proximity::logAndClear()
{
  if (iNumProximityData > 0) {
    // xxx er_errors could use a variant for logging C++ exceptions
    try {
      makeString();
    } catch (const std::exception &ex) {
      er_log_none(er_FATAL, "proximity sensor: %s", ex.what());
      return;
    }
    const char* data = iString.constData();
    log_db_log_proximityset(GetLogDb(), data, NULL);
  }
}

Sensor_proximity* new_Sensor_proximity(ac_AppContext* aAppContext)
{
  return q_check_ptr(new Sensor_proximity(aAppContext));
}

void delete_Sensor_proximity(Sensor_proximity* obj)
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
