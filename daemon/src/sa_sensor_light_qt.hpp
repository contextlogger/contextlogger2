#ifndef __sa_sensor_light_qt_hpp__
#define __sa_sensor_light_qt_hpp__

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

#endif /* __sa_sensor_light_qt_hpp__ */

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
