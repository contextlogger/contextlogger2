// -*- c++ -*-

#ifndef CL2APPCONTAINER_H
#define CL2APPCONTAINER_H

#include <coecntrl.h>

class CEikLabel;

class CCl2appContainer : public CCoeControl, MCoeControlObserver
{
public:
  void ConstructL(const TRect& aRect);
  ~CCl2appContainer();

private:
  void SizeChanged();
  TInt CountComponentControls() const;
  CCoeControl* ComponentControl(TInt aIndex) const;
  void Draw(const TRect& aRect) const;
  void HandleControlEventL(CCoeControl* aControl, TCoeEvent aEventType);
};

#endif // CL2APPCONTAINER_H
