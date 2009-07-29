//
// key_codes.py
//
// S60 Python key code constants
// Recommended usage: from key_codes import *
//
// Copyright (c) 2005 - 2007 Nokia Corporation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#ifndef __epoc_key_codes_hpp__
#define __epoc_key_codes_hpp__

#include <e32keys.h>

// The following are not included in the Symbian headers.
#define EKey0 0x30
#define EKey1 0x31 
#define EKey2 0x32
#define EKey3 0x33
#define EKey4 0x34
#define EKey5 0x35
#define EKey6 0x36
#define EKey7 0x37
#define EKey8 0x38
#define EKey9 0x39
#define EKeyStar 0x2a
#define EKeyHash 0x23

#define EKeyLeftSoftkey EKeyDevice0
#define EKeyRightSoftkey EKeyDevice1
#define EKeySelect EKeyDevice3
#define EKeyEdit EKeyLeftShift
#define EKeyMenu EKeyApplication0

#define ALL_KEY_CODES				\
  {						\
   EKeyBackspace,				\
   EKeyTab,					\
   EKeyLineFeed,				\
   EKeyVerticalTab,				\
   EKeyFormFeed,				\
   EKeyEnter,					\
   EKeyEscape,					\
   EKeySpace,					\
   EKeyDelete,					\
   EKeyPrintScreen,				\
   EKeyPause,					\
   EKeyHome,					\
   EKeyEnd,					\
   EKeyPageUp,					\
   EKeyPageDown,				\
   EKeyInsert,					\
   EKeyLeftArrow,				\
   EKeyRightArrow,				\
   EKeyUpArrow,					\
   EKeyDownArrow,				\
   EKeyLeftShift,				\
   EKeyRightShift,				\
   EKeyLeftAlt,					\
   EKeyRightAlt,				\
   EKeyLeftCtrl,				\
   EKeyRightCtrl,				\
   EKeyLeftFunc,				\
   EKeyRightFunc,				\
   EKeyCapsLock,				\
   EKeyNumLock,					\
   EKeyScrollLock,				\
   EKeyF1,					\
   EKeyF2,					\
   EKeyF3,					\
   EKeyF4,					\
   EKeyF5,					\
   EKeyF6,					\
   EKeyF7,					\
   EKeyF8,					\
   EKeyF9,					\
   EKeyF10,					\
   EKeyF11,					\
   EKeyF12,					\
   EKeyF13,					\
   EKeyF14,					\
   EKeyF15,					\
   EKeyF16,					\
   EKeyF17,					\
   EKeyF18,					\
   EKeyF19,					\
   EKeyF20,					\
   EKeyF21,					\
   EKeyF22,					\
   EKeyF23,					\
   EKeyF24,					\
   EKeyOff,					\
   EKeyIncContrast,				\
   EKeyDecContrast,				\
   EKeyBacklightOn,				\
   EKeyBacklightOff,				\
   EKeyBacklightToggle,				\
     EKeySliderDown,				\
     EKeySliderUp,				\
   EKeyMenu,					\
   EKeyDictaphonePlay,				\
   EKeyDictaphoneStop,				\
   EKeyDictaphoneRecord,			\
   EKeyHelp,					\
   EKeyDial,					\
   EKeyScreenDimension0,			\
   EKeyScreenDimension1,			\
   EKeyScreenDimension2,			\
   EKeyScreenDimension3,			\
   EKeyIncVolume,				\
   EKeyDecVolume,				\
   EKeyDevice0,					\
   EKeyDevice1,					\
   EKeyDevice2,					\
   EKeyDevice3,					\
   EKeyDevice4,					\
   EKeyDevice5,					\
   EKeyDevice6,					\
   EKeyDevice7,					\
   EKeyDevice8,					\
   EKeyDevice9,					\
   EKeyDeviceA,					\
   EKeyDeviceB,					\
   EKeyDeviceC,					\
   EKeyDeviceD,					\
   EKeyDeviceE,					\
   EKeyDeviceF,					\
   EKeyApplication0,				\
   EKeyApplication1,				\
   EKeyApplication2,				\
   EKeyApplication3,				\
   EKeyApplication4,				\
   EKeyApplication5,				\
   EKeyApplication6,				\
   EKeyApplication7,				\
   EKeyApplication8,				\
   EKeyApplication9,				\
   EKeyApplicationA,				\
   EKeyApplicationB,				\
   EKeyApplicationC,				\
   EKeyApplicationD,				\
   EKeyApplicationE,				\
   EKeyApplicationF,				\
   EKeyYes,					\
   EKeyNo,					\
   EKeyIncBrightness,				\
   EKeyDecBrightness,				\
   EKeyKeyboardExtend,				\
   EKeyDevice10,				\
   EKeyDevice11,				\
   EKeyDevice12,				\
   EKeyDevice13,				\
   EKeyDevice14,				\
   EKeyDevice15,				\
   EKeyDevice16,				\
   EKeyDevice17,				\
   EKeyDevice18,				\
   EKeyDevice19,				\
   EKeyDevice1A,				\
   EKeyDevice1B,				\
   EKeyDevice1C,				\
   EKeyDevice1D,				\
   EKeyDevice1E,				\
   EKeyDevice1F,				\
   EKeyApplication10,				\
   EKeyApplication11,				\
   EKeyApplication12,				\
   EKeyApplication13,				\
   EKeyApplication14,				\
   EKeyApplication15,				\
   EKeyApplication16,				\
   EKeyApplication17,				\
   EKeyApplication18,				\
   EKeyApplication19,				\
   EKeyApplication1A,				\
   EKeyApplication1B,				\
   EKeyApplication1C,				\
   EKeyApplication1D,				\
   EKeyApplication1E,				\
   EKeyApplication1F,				\
EStdKeyBackspace,				\
  EStdKeyTab,					\
  EStdKeyEnter,					\
  EStdKeyEscape,				\
  EStdKeySpace,					\
  EStdKeyPrintScreen,				\
  EStdKeyPause,					\
  EStdKeyHome,					\
  EStdKeyEnd,					\
  EStdKeyPageUp,				\
  EStdKeyPageDown,				\
  EStdKeyInsert,				\
  EStdKeyDelete,				\
  EStdKeyLeftArrow,				\
  EStdKeyRightArrow,				\
  EStdKeyUpArrow,				\
  EStdKeyDownArrow,				\
  EStdKeyLeftShift,				\
  EStdKeyRightShift,				\
  EStdKeyLeftAlt,				\
  EStdKeyRightAlt,				\
  EStdKeyLeftCtrl,				\
  EStdKeyRightCtrl,				\
  EStdKeyLeftFunc,				\
  EStdKeyRightFunc,				\
  EStdKeyCapsLock,				\
  EStdKeyNumLock,				\
  EStdKeyScrollLock,				\
  EStdKeyF1,					\
  EStdKeyF2,					\
  EStdKeyF3,					\
  EStdKeyF4,					\
  EStdKeyF5,					\
  EStdKeyF6,					\
  EStdKeyF7,					\
  EStdKeyF8,					\
  EStdKeyF9,					\
  EStdKeyF10,					\
  EStdKeyF11,					\
  EStdKeyF12,					\
  EStdKeyF13,					\
  EStdKeyF14,					\
  EStdKeyF15,					\
  EStdKeyF16,					\
  EStdKeyF17,					\
  EStdKeyF18,					\
  EStdKeyF19,					\
  EStdKeyF20,					\
  EStdKeyF21,					\
  EStdKeyF22,					\
  EStdKeyF23,					\
  EStdKeyF24,					\
  EStdKeyXXX,					\
  EStdKeyComma,					\
  EStdKeyFullStop,				\
  EStdKeyForwardSlash,				\
  EStdKeyBackSlash,				\
  EStdKeySemiColon,				\
  EStdKeySingleQuote,				\
  EStdKeyHash,					\
  EStdKeySquareBracketLeft,			\
  EStdKeySquareBracketRight,			\
  EStdKeyMinus,					\
  EStdKeyEquals,				\
  EStdKeyNkpForwardSlash,			\
  EStdKeyNkpAsterisk,				\
  EStdKeyNkpMinus,				\
  EStdKeyNkpPlus,				\
  EStdKeyNkpEnter,				\
  EStdKeyNkp1,					\
  EStdKeyNkp2,					\
  EStdKeyNkp3,					\
  EStdKeyNkp4,					\
  EStdKeyNkp5,					\
  EStdKeyNkp6,					\
  EStdKeyNkp7,					\
  EStdKeyNkp8,					\
  EStdKeyNkp9,					\
  EStdKeyNkp0,					\
  EStdKeyNkpFullStop,				\
  EStdKeyMenu,					\
  EStdKeyBacklightOn,				\
  EStdKeyBacklightOff,				\
  EStdKeyBacklightToggle,			\
  EStdKeyIncContrast,				\
  EStdKeyDecContrast,				\
     EStdKeySliderDown,			\
      EStdKeySliderUp,			\
  EStdKeyDictaphonePlay,			\
  EStdKeyDictaphoneStop,			\
  EStdKeyDictaphoneRecord,			\
  EStdKeyHelp,					\
  EStdKeyOff,					\
  EStdKeyDial,					\
  EStdKeyIncVolume,				\
  EStdKeyDecVolume,				\
  EStdKeyDevice0,				\
  EStdKeyDevice1,				\
  EStdKeyDevice2,				\
  EStdKeyDevice3,				\
  EStdKeyDevice4,				\
  EStdKeyDevice5,				\
  EStdKeyDevice6,				\
  EStdKeyDevice7,				\
  EStdKeyDevice8,				\
  EStdKeyDevice9,				\
  EStdKeyDeviceA,				\
  EStdKeyDeviceB,				\
  EStdKeyDeviceC,				\
  EStdKeyDeviceD,				\
  EStdKeyDeviceE,				\
  EStdKeyDeviceF,				\
  EStdKeyApplication0,				\
  EStdKeyApplication1,				\
  EStdKeyApplication2,				\
  EStdKeyApplication3,				\
  EStdKeyApplication4,				\
  EStdKeyApplication5,				\
  EStdKeyApplication6,				\
  EStdKeyApplication7,				\
  EStdKeyApplication8,				\
  EStdKeyApplication9,				\
  EStdKeyApplicationA,				\
  EStdKeyApplicationB,				\
  EStdKeyApplicationC,				\
  EStdKeyApplicationD,				\
  EStdKeyApplicationE,				\
  EStdKeyApplicationF,				\
  EStdKeyYes,					\
  EStdKeyNo,					\
  EStdKeyIncBrightness,				\
  EStdKeyDecBrightness,				\
  EStdKeyKeyboardExtend,			\
  EStdKeyDevice10,				\
  EStdKeyDevice11,				\
  EStdKeyDevice12,				\
  EStdKeyDevice13,				\
  EStdKeyDevice14,				\
  EStdKeyDevice15,				\
  EStdKeyDevice16,				\
  EStdKeyDevice17,				\
  EStdKeyDevice18,				\
  EStdKeyDevice19,				\
  EStdKeyDevice1A,				\
  EStdKeyDevice1B,				\
  EStdKeyDevice1C,				\
  EStdKeyDevice1D,				\
  EStdKeyDevice1E,				\
  EStdKeyDevice1F,				\
  EStdKeyApplication10,				\
  EStdKeyApplication11,				\
  EStdKeyApplication12,				\
  EStdKeyApplication13,				\
  EStdKeyApplication14,				\
  EStdKeyApplication15,				\
  EStdKeyApplication16,				\
  EStdKeyApplication17,				\
  EStdKeyApplication18,				\
  EStdKeyApplication19,				\
  EStdKeyApplication1A,				\
  EStdKeyApplication1B,				\
  EStdKeyApplication1C,				\
  EStdKeyApplication1D,				\
  EStdKeyApplication1E,				\
  EStdKeyApplication1F,				\
  EKey0,					\
  EKey1,					\
  EKey2,					\
  EKey3,					\
  EKey4,					\
  EKey5,					\
  EKey6,					\
  EKey7,					\
  EKey8,					\
  EKey9,					\
  EKeyStar,					\
  EKeyHash,					\
     0						\
}

#define SELECT_KEY_CODES				\
  {						\
   EKeyLeftArrow,				\
   EKeyRightArrow,				\
   EKeyUpArrow,					\
   EKeyDownArrow,				\
   EKeyMenu,					\
   EKeyIncVolume,				\
   EKeyDecVolume,				\
   EKeyYes,					\
   EKeyNo,					\
  EKey0,					\
  EKey1,					\
  EKey2,					\
  EKey3,					\
  EKey4,					\
  EKey5,					\
  EKey6,					\
  EKey7,					\
  EKey8,					\
  EKey9,					\
  EKeyStar,					\
  EKeyHash,					\
     EKeyLeftSoftkey,				\
     EKeyRightSoftkey,				\
     EKeySelect,				\
     EKeyEdit,					\
     0						\
}

#endif /* __epoc_key_codes_hpp__ */
