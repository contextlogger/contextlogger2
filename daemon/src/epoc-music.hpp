#ifndef __epoc_music_hpp__
#define __epoc_music_hpp__

#include "application_config.h"

#if __MUSIC_ENABLED__

#include "ac_app_context.h"
#include "ld_log_db.h"
#include "utils_cl2.h"

// Plug-in APIs.
#if __HAVE__MPLAYERREMOTECONTROL__
// http://wiki.forum.nokia.com/index.php/Music_Player_Remote_Control_API
// http://wiki.forum.nokia.com/index.php/KIS001005_-_Music_Player_Remote_Control_API_does_not_work_in_all_S60_3rd_Edition%2C_FP1_and_FP2_devices
#include <mplayerremotecontrol.h>
#include <mplayerconstants.h>
#elif __HAVE_MPXPLAYBACKUTILITY__
// MPX Playback Utility API http://wiki.forum.nokia.com/index.php/TSS001652_-_Using_MPX_Playback_Utility_API
#include <mpxplaybackutility.h>
#include <mpxplaybackobserver.h>
#endif

#include <e32std.h>

class MyTrackInfo;

NONSHARABLE_CLASS(CSensor_music) :
  public CBase,
  public MMPXPlaybackObserver,
  public MMPXPlaybackCallback
{
 public: 
  static CSensor_music* NewLC(ac_AppContext* aAppContext);
  static CSensor_music* NewL(ac_AppContext* aAppContext);
  virtual ~CSensor_music();

 private:
  CSensor_music(ac_AppContext* aAppContext);
  void ConstructL();

 private:
  ac_AppContext* iAppContext; // not owned
  MMPXPlaybackUtility* iPlaybackUtility; // owned
  MyTrackInfo* iOldTrackInfo; // owned
  MyTrackInfo* iTrackInfo; // owned
  TInt iOldPbState;

 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  void CloseSession();

  void RequestMediaL();

 private: // MMPXPlaybackObserver
  virtual void HandlePlaybackMessage(CMPXMessage* aMsg, TInt errCode);

 private: // MMPXPlaybackCallback
  virtual void HandlePropertyL(TMPXPlaybackProperty aProperty,
			       TInt aValue, 
			       TInt aError) {}
    
  virtual void HandleSubPlayerNamesL(TUid aPlayer, 
				     const MDesCArray* aSubPlayers,
				     TBool aComplete,
				     TInt aError) {}
    
  virtual void HandleMediaL(const CMPXMedia& aProperties,
			    TInt aError);

};

#endif // __MUSIC_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __MUSIC_ENABLED__
#define DECLARE_SENSOR_music CSensor_music* iSensor_music
#define SENSOR_MUSIC_DESTROY DELETE_Z(self->iSensor_music)
#define SENSOR_MUSIC_CREATE 
#define SENSOR_MUSIC_START sa_typical_symbian_sensor_create(self->iSensor_music = CSensor_music::NewL(self->ac), "music sensor initialization")
#define SENSOR_MUSIC_STOP SENSOR_MUSIC_DESTROY
#define SENSOR_MUSIC_IS_RUNNING (self->iSensor_music != NULL)
#define SENSOR_MUSIC_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_music
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_music_hpp__ */

/**

epoc-music.hpp

Copyright 2009-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
