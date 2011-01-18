/** 

  Music artist and track logging

  References:

  Sample source code for example in the [Music Player Remote https://www.iyouit.eu/portal/Software.aspx] PyS60 extension.

  As for APIs, see the [Media Player Engine API http://wiki.forum.nokia.com/index.php/Media_Player_Engine_API]. Related, possibly interesting API: [Audio Metadata Reader API http://wiki.forum.nokia.com/index.php/Audio_Metadata_Reader_API]. [Music Player Remote Control API http://wiki.forum.nokia.com/index.php/Music_Player_Remote_Control_API] sounds highly related despite the name, but do note [this http://wiki.forum.nokia.com/index.php/KIS001005_-_Music_Player_Remote_Control_API_does_not_work_in_all_S60_3rd_Edition%2C_FP1_and_FP2_devices] issue. For more recent devices see [MPX Playback Utility API http://wiki.forum.nokia.com/index.php/TSS001652_-_Using_MPX_Playback_Utility_API].

 */

#include "epoc-music.hpp"

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

NONSHARABLE_CLASS(MyTrackInfo)
{
 public:
  MyTrackInfo() : iUrl(0), iArtist(0), iTitle(0), iAlbum(0) {}

  HBufC8* iUrl; // owned
  HBufC8* iArtist; // owned
  HBufC8* iTitle; // owned
  HBufC8* iAlbum; // owned

  void Reset() {
    DELETE_Z(iUrl);
    DELETE_Z(iTitle);
    DELETE_Z(iArtist);
    DELETE_Z(iAlbum);
  }

  ~MyTrackInfo() { Reset(); }

  int Cmp(const MyTrackInfo& another) const;
};

static int CmpHBufC8(const HBufC8* a, const HBufC8* b)
{
  if (!a && !b) return 0;
  if (a && !b) return 1;
  if (!a && b) return -1;
  return (*a).Compare(*b);
}

int MyTrackInfo::Cmp(const MyTrackInfo& another) const
{
  return CmpHBufC8(iUrl, another.iUrl) ||
    CmpHBufC8(iTitle, another.iTitle) ||
    CmpHBufC8(iArtist, another.iArtist) ||
    CmpHBufC8(iAlbum, another.iAlbum);
}

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
  CloseSession();
  delete iTrackInfo;
  delete iOldTrackInfo;
}

void CSensor_music::CloseSession()
{
  if (iPlaybackUtility) {
    //TRAP_IGNORE(iPlaybackUtility->CommandL(EPbCmdClose));
    //iPlaybackUtility->CancelRequest();
    iPlaybackUtility->Close();
    // We appear to be getting USER 42 if we delete iPlaybackUtility.
    // Hopefully Close() is enough.
    //delete iPlaybackUtility; // not
    iPlaybackUtility = NULL;
  }
}

CSensor_music::CSensor_music(ac_AppContext* aAppContext) :
  iAppContext(aAppContext)
{
}

void CSensor_music::ConstructL()
{
  iTrackInfo = new (ELeave) MyTrackInfo;
  iOldTrackInfo = new (ELeave) MyTrackInfo;

  iPlaybackUtility = MMPXPlaybackUtility::NewL(KPbModeActivePlayer, this);

  if ((iPlaybackUtility->StateL() != EPbStateNotInitialised) &&
      (iPlaybackUtility->StateL() != EPbStateInitialising)) {
    // Request info for anything already playing.
    RequestMediaL();
  }
}

#include <mpxmessagegeneraldefs.h>
#include <mpxplaybackmessage.h>

#include <glib/gprintf.h>

void CSensor_music::HandlePlaybackMessage(CMPXMessage* aMessage, TInt errCode)
{
  if (errCode) {
    guilogf("music: error %d", errCode);
    er_log_symbian(0, errCode, "INACTIVATE: music data fetch error");
    CloseSession();
    return;
  }

  int eventId = -1; // logged
  const char* detail = NULL; // logged
#define DETAILBUFSIZE 50
  char detailBuf[DETAILBUFSIZE];
#define DETAILFMT(f...) g_snprintf(detailBuf, DETAILBUFSIZE, f)

  TMPXMessageId msgId(*aMessage->Value<TMPXMessageId>(KMPXMessageGeneralId));
  //guilogf("music: message ID %d", msgId);
  if (msgId != KMPXMessageGeneral) {
    return;
  } else {
     eventId = aMessage->ValueTObjectL<TInt>(KMPXMessageGeneralEvent);
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
	   //guilogf("music: ECommandReceived");
	   break;
	 }
       case TMPXPlaybackMessage::ECommandComplete:
	 {
	   guilogf("music: ECommandComplete");
	   break;
	 }
       case TMPXPlaybackMessage::EPropertyChanged: /* seen often */
	 {
	   TMPXPlaybackProperty propId(aMessage->ValueTObjectL<TMPXPlaybackProperty>(KMPXMessageGeneralType));
	   TInt value(aMessage->ValueTObjectL<TInt>(KMPXMessageGeneralData));
	   const char* propStr;
	   switch (propId)
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
	   DETAILFMT("{property: %d, value: %d}", propId, value);
	   detail = detailBuf;
	   guilogf("music: EPropertyChanged: '%s' = %d", propStr, value);
	   break;
	 }
       case TMPXPlaybackMessage::EStateChanged: /* seen often */
	 {
	   TInt stateId(aMessage->ValueTObjectL<TMPXPlaybackState>(KMPXMessageGeneralType));
	   if (stateId == iOldPbState)
	     // We seem to get repeats, avoid that.
	     return;
	   iOldPbState = stateId;
	   const char* stateStr;
	   switch (stateId)
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
	   DETAILFMT("{state: %d}", stateId);
	   detail = detailBuf;
	   //guilogf("music: EStateChanged %d", stateId);
	   guilogf("music: playback state %s (%d)", stateStr, stateId);
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

     log_db_log_musicplayer(GetLogDb(), eventId, detail, NULL);
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
      attrs.Append(KMPXMediaMusicAlbum);
      // The two-arg method is deprecated, we assume passing NULL as
      // the third arg has the same semantics. The arg is
      // CMPXAttributeSpecs* aSpecs (typedef CMPXMedia
      // CMPXAttributeSpecs).
      CMPXAttributeSpecs* specs = NULL;
      source->MediaL(attrs.Array(), *this, specs);
      CleanupStack::PopAndDestroy(&attrs);
    }
}

#define SWAP(t,x,y) { t _tmp = x; x = y; y = _tmp; }

// adapted from code in Music Player Remote Python extension
// also see mpxmedia.h
void CSensor_music::HandleMediaL(const CMPXMedia& aMedia,
				 TInt errCode)
{
  //guilogf("HandleMediaL (%d)", errCode);

  if (errCode)
    return;

  SWAP(MyTrackInfo*, iOldTrackInfo, iTrackInfo); // current becomes old

  iTrackInfo->Reset(); // clear space for new data

  HBufC8*& iUrl = iTrackInfo->iUrl;
  HBufC8*& iArtist = iTrackInfo->iArtist;
  HBufC8*& iTitle = iTrackInfo->iTitle;
  HBufC8*& iAlbum = iTrackInfo->iAlbum;

  if (aMedia.IsSupported(KMPXMediaGeneralUri)) {
    const TDesC& text = aMedia.ValueText(KMPXMediaGeneralUri);
    /*
    TParsePtrC filePath(text);
    iUrl = ConvToUtf8ZL(filePath.Name());
    */
    iUrl = ConvToUtf8ZL(text);
  }
  if (aMedia.IsSupported(KMPXMediaGeneralTitle)) {
    iTitle = ConvToUtf8ZL(aMedia.ValueText(KMPXMediaGeneralTitle));
  }
  if (aMedia.IsSupported(KMPXMediaMusicArtist)) {
    iArtist = ConvToUtf8ZL(aMedia.ValueText(KMPXMediaMusicArtist));
  }
  if (aMedia.IsSupported(KMPXMediaMusicAlbum)) {
    iAlbum = ConvToUtf8ZL(aMedia.ValueText(KMPXMediaMusicAlbum));
  }

  if (iTrackInfo->Cmp(*iOldTrackInfo) == 0)
    return; // no duplicates

  const char* url = (iUrl ? (const char*)(iUrl->Ptr()) : NULL);
  const char* title = (iTitle ? (const char*)(iTitle->Ptr()) : NULL);
  const char* artist = (iArtist ? (const char*)(iArtist->Ptr()) : NULL);
  const char* album = (iAlbum ? (const char*)(iAlbum->Ptr()) : NULL);

  log_db_log_musictrack(GetLogDb(), url, title, artist, album, NULL);

  if (url)
    guilogf("music: URL '%s'", url);
  if (title)
    guilogf("music: title '%s'", title);
  if (artist)
    guilogf("music: artist '%s'", artist);
  if (album)
    guilogf("music: album '%s'", album);
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
