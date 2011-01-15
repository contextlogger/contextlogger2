#include "epoc-music.hpp"

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

CSensor_music* CSensor_music::NewLC(ac_AppContext* aAppContext)
{
  CSensor_music* obj = new (ELeave) CSensor_music(aAppContext);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  return obj;
}

CSensor_music* CSensor_music::NewL(ac_AppContext* aAppContext)
{
  CSensor_music* obj = CSensor_music::NewLC(aAppContext);
  CleanupStack::Pop(obj);
  return obj;
}

CSensor_music::~CSensor_music()
{
  if (iPlaybackUtility) {
    //TRAP_IGNORE(iPlaybackUtility->CommandL(EPbCmdClose));
    //iPlaybackUtility->CancelRequest();
    iPlaybackUtility->Close();
    // We appear to be getting USER 42 if we delete iPlaybackUtility.
    // Hopefully Close() is enough.
    //delete iPlaybackUtility; // not
  }
  ClearTrackInfo();
}

CSensor_music::CSensor_music(ac_AppContext* aAppContext) :
  iAppContext(aAppContext)
{
}

void CSensor_music::ConstructL()
{
  iPlaybackUtility = MMPXPlaybackUtility::NewL(KPbModeActivePlayer, this);

  if ((iPlaybackUtility->StateL() != EPbStateNotInitialised) &&
      (iPlaybackUtility->StateL() != EPbStateInitialising)) {
    // Request info for anything already playing.
    RequestMediaL();
  }
}

#include <mpxmessagegeneraldefs.h>
#include <mpxplaybackmessage.h>

void CSensor_music::HandlePlaybackMessage(CMPXMessage* aMessage, TInt errCode)
{
  if (errCode) {
    guilogf("music: error %d", errCode);
    er_log_symbian(0, errCode, "INACTIVATE: music data fetch error");
    iPlaybackUtility->Close();
    return;
  }

  TMPXMessageId msgId(*aMessage->Value<TMPXMessageId>(KMPXMessageGeneralId));
  //guilogf("music: message ID %d", msgId);
  if (msgId == KMPXMessageGeneral) {
     TInt eventId = aMessage->ValueTObjectL<TInt>(KMPXMessageGeneralEvent);
     //guilogf("music: event ID %d", eventId);
     switch (eventId)
       {
       case TMPXPlaybackMessage::ENoEvent:
	 {
	   guilogf("music: ENoEvent");
	   break;
	 }
       case TMPXPlaybackMessage::EError:
	 {
	   guilogf("music: EError");
	   break;
	 }
       case TMPXPlaybackMessage::ECommandReceived: /* seen often */
	 {
	   guilogf("music: ECommandReceived");
	   break;
	 }
       case TMPXPlaybackMessage::ECommandComplete:
	 {
	   guilogf("music: ECommandComplete");
	   break;
	 }
       case TMPXPlaybackMessage::EPropertyChanged: /* seen often */
	 {
	   TMPXPlaybackProperty property(aMessage->ValueTObjectL<TMPXPlaybackProperty>(KMPXMessageGeneralType));
	   TInt value(aMessage->ValueTObjectL<TInt>(KMPXMessageGeneralData));
	   const char* propStr;
	   switch (property)
	     {
	     case EPbPropertyVolume:
	       {
		 propStr = "EPbPropertyVolume";
		 break;
	       }
	     case EPbPropertyMaxVolume:
	       {
		 propStr = "EPbPropertyMaxVolume";
		 break;
	       }
	     case EPbPropertyVolumeRamp:
	       {
		 propStr = "EPbPropertyVolumeRamp";
		 break;
	       }
	     case EPbPropertyMute:
	       {
		 propStr = "EPbPropertyMute";
		 break;
	       }
	     case EPbPropertyBalance:
	       {
		 propStr = "EPbPropertyBalance";
		 break;
	       }
	     case EPbPropertyEmbeddedMode:
	       {
		 propStr = "EPbPropertyEmbeddedMode";
		 break;
	       }
	     case EPbPropertyCrossFade:
	       {
		 propStr = "EPbPropertyCrossFade";
		 break;
	       }
	     case EPbPropertyRandomMode:
	       {
		 propStr = "EPbPropertyRandomMode";
		 break;
	       }
	     case EPbPropertyRepeatMode:
	       {
		 propStr = "EPbPropertyRepeatMode";
		 break;
	       }
	     case EPbPropertyAccessPoint:
	       {
		 propStr = "EPbPropertyAccessPoint";
		 break;
	       }
	     case EPbPropertyPosition:
	       {
#if 0
		 propStr = "EPbPropertyPosition";
		 break;
#else
                 // These events are very frequent as track position
                 // progress is frequently reported.
		 return;
#endif
	       }
	     case EPbPropertyDuration:
	       {
		 propStr = "EPbPropertyDuration";
		 break;
	       }
	     case EPbPropertySongValid:
	       {
		 propStr = "EPbPropertySongValid";
		 break;
	       }
	     case EPbPropertyRemote:
	       {
		 propStr = "EPbPropertyRemote";
		 break;
	       }
	     case EPbPropertySupportedFeatures:
	       {
		 propStr = "EPbPropertySupportedFeatures";
		 break;
	       }
	     default:
	       {
	         propStr = "<unknown>";
	         break;
	       }
	     }
	   guilogf("music: EPropertyChanged: '%s' = %d", propStr, value);
	   break;
	 }
       case TMPXPlaybackMessage::EStateChanged: /* seen often */
	 {
	   TInt state(aMessage->ValueTObjectL<TMPXPlaybackState>(KMPXMessageGeneralType));
	   if (state == iOldPbState)
	     // We seem to get repeats, avoid that.
	     return;
	   iOldPbState = state;
	   const char* stateStr;
	   switch (state)
	     {
	     case EPbStateNotInitialised:
	       {
		 stateStr = "EPbStateNotInitialised";
		 break;
	       }
	     case EPbStateInitialising:
	       {
		 stateStr = "EPbStateInitialising";
		 break;
	       }
	     case EPbStatePlaying:
	       {
		 stateStr = "EPbStatePlaying";
		 break;
	       }
	     case EPbStatePaused:
	       {
		 stateStr = "EPbStatePaused";
		 break;
	       }
	     case EPbStateStopped:
	       {
		 stateStr = "EPbStateStopped";
		 break;
	       }
	     case EPbStateSeekingForward:
	       {
		 stateStr = "EPbStateSeekingForward";
		 break;
	       }
	     case EPbStateSeekingBackward:
	       {
		 stateStr = "EPbStateSeekingBackward";
		 break;
	       }
	     case EPbStateShuttingDown:
	       {
		 stateStr = "EPbStateShuttingDown";
		 break;
	       }
	     case EPbStateBuffering:
	       {
		 stateStr = "EPbStateBuffering";
		 break;
	       }
	     case EPbStateDownloading:
	       {
		 stateStr = "EPbStateDownloading";
		 break;
	       }
	     default:
	       {
	         stateStr = "<unknown>";
	         break;
	       }
	     }
	   //guilogf("music: EStateChanged %d", state);
	   guilogf("music: playback state %s (%d)", stateStr, state);
	   break;
	 }
       case TMPXPlaybackMessage::ESongCorrupt:
	 {
	   guilogf("music: ESongCorrupt");
	   break;
	 }
       case TMPXPlaybackMessage::ESongContainerChanged:
	 {
	   guilogf("music: ESongContainerChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EInitializeComplete: /* seen often */
	 {
	   guilogf("music: EInitializeComplete");
	   break;
	 }
       case TMPXPlaybackMessage::ESongChanged:
	 {
	   guilogf("music: ESongChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EPlayerChanged:
	 {
	   guilogf("music: EPlayerChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EActivePlayerChanged:
	 {
	   guilogf("music: EActivePlayerChanged");
	   break;
	 }
       case TMPXPlaybackMessage::ESubPlayersChanged:
	 {
	   guilogf("music: ESubPlayersChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EPlayerSelectionChanged:
	 {
	   guilogf("music: EPlayerSelectionChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadStarted:
	 {
	   guilogf("music: EDownloadStarted");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadUpdated:
	 {
	   guilogf("music: EDownloadUpdated");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadComplete:
	 {
	   guilogf("music: EDownloadComplete");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadPositionChanged:
	 {
	   guilogf("music: EDownloadPositionChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadStateChanged:
	 {
	   guilogf("music: EDownloadStateChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadCmdPauseDownload:
	 {
	   guilogf("music: EDownloadCmdPauseDownload");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadCmdResumeDownload:
	 {
	   guilogf("music: EDownloadCmdResumeDownload");
	   break;
	 }
       case TMPXPlaybackMessage::EDownloadCmdCancelDownload:
	 {
	   guilogf("music: EDownloadCmdCancelDownload");
	   break;
	 }
       case TMPXPlaybackMessage::EAccessoryChanged:
	 {
	   guilogf("music: EAccessoryChanged");
	   break;
	 }
       case TMPXPlaybackMessage::EMediaChanged: /* seen often */
	 {
	   guilogf("music: EMediaChanged");
	   RequestMediaL();
	   break;
	 }
       case TMPXPlaybackMessage::ESkipping:
	 {
	   guilogf("music: ESkipping");
	   break;
	 }
       case TMPXPlaybackMessage::ESkipEnd:
	 {
	   guilogf("music: ESkipEnd");
	   break;
	 }
       case TMPXPlaybackMessage::EPlayerUnavailable:
	 {
	   guilogf("music: EPlayerUnavailable");
	   break;
	 }
       case TMPXPlaybackMessage::EPlaylistUpdated:
	 {
	   guilogf("music: EPlaylistUpdated");
	   RequestMediaL();
	   break;
	 }
       case TMPXPlaybackMessage::EReachedEndOfPlaylist:
	 {
	   guilogf("music: EReachedEndOfPlaylist");
	   break;
	 }
       default:
         {
	   guilogf("music: unknown event");
           break;
         }
       }
  }
}

#include <mpxmediageneraldefs.h>
#include <mpxmediamusicdefs.h>

// adapted from code in Music Player Remote Python extension
void CSensor_music::RequestMediaL()
{
  MMPXSource* source(iPlaybackUtility->Source());
  if (source)
    {
      RArray<TMPXAttribute> attrs;
      CleanupClosePushL(attrs);
      attrs.Append(KMPXMediaGeneralUri);
      attrs.Append(KMPXMediaGeneralTitle);
      attrs.Append(KMPXMediaMusicArtist);
      // The two-arg method is deprecated, we assume passing NULL as
      // the third arg has the same semantics. The arg is
      // CMPXAttributeSpecs* aSpecs (typedef CMPXMedia
      // CMPXAttributeSpecs).
      CMPXAttributeSpecs* specs = NULL;
      source->MediaL(attrs.Array(), *this, specs);
      CleanupStack::PopAndDestroy(&attrs);
    }
}

// adapted from code in Music Player Remote Python extension
// also see mpxmedia.h
void CSensor_music::HandleMediaL(const CMPXMedia& aMedia,
				 TInt errCode)
{
  //guilogf("HandleMediaL (%d)", errCode);

  if (errCode)
    return;

  ClearTrackInfo();

  if (aMedia.IsSupported(KMPXMediaGeneralTitle))
    {
      iTitle = ConvToUtf8ZL(aMedia.ValueText(KMPXMediaGeneralTitle));
    }
  else if (aMedia.IsSupported(KMPXMediaGeneralUri))
    {
      const TDesC& text = aMedia.ValueText(KMPXMediaGeneralUri);
#if 0
      TParsePtrC filePath(text);
      iTitle = ConvToUtf8ZL(filePath.Name());
#else
      iTitle = ConvToUtf8ZL(text);
#endif
    }
  if (aMedia.IsSupported(KMPXMediaMusicArtist)) {
    iArtist = ConvToUtf8ZL(aMedia.ValueText(KMPXMediaMusicArtist));
  }

  const char* title = (iTitle ? (const char*)(iTitle->Ptr()) : NULL);
  const char* artist = (iArtist ? (const char*)(iArtist->Ptr()) : NULL);

  if (title)
    guilogf("music: title '%s'", title);
  if (artist)
    guilogf("music: artist '%s'", artist);
}

void CSensor_music::ClearTrackInfo()
{
  DELETE_Z(iTitle);
  DELETE_Z(iArtist);
}

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
