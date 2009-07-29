#include <eiklabel.h>
#include "cl2appcontainer.h"

void CCl2appContainer::ConstructL(const TRect& aRect)
{
  CreateWindowL();
  SetRect(aRect);
  ActivateL();
}

CCl2appContainer::~CCl2appContainer()
{
}

void CCl2appContainer::SizeChanged()
{
}

TInt CCl2appContainer::CountComponentControls() const
{
  return 0;
}

CCoeControl* CCl2appContainer::ComponentControl(TInt aIndex) const
{
  switch (aIndex)
    {
    default:
      return NULL;
    }
}

void CCl2appContainer::Draw(const TRect& aRect) const
{
  CWindowGc& gc = SystemGc();
  gc.SetPenStyle(CGraphicsContext::ENullPen);
  gc.SetBrushColor(KRgbWhite);
  gc.SetBrushStyle(CGraphicsContext::ESolidBrush);
  gc.DrawRect(aRect);
}

void CCl2appContainer::HandleControlEventL
(CCoeControl* /*aControl*/, TCoeEvent /*aEventType*/)
{
}
