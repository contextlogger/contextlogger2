// 
// This code is derived from Symbian Ltd provided and copyrighted
// example code from year 2000. Note, however, the boilerplate nature
// of most Symbian client-server code, and therefore the lack of
// originality in such code.
// 

// Here we define an active object that runs as part of the CL2
// daemon, implementing a Symbian-client server providing access to
// select functionality of the daemon.
// 
// The server runs whenever the CL2 daemon does, and too bad if a
// client tries to use the API when CL2 is not running; this then is
// not a transient server, as one would call it in Symbian vocabulary,
// and not necessarily something that is always running either.

#ifndef __epoc_cliapi_server_hpp__
#define __epoc_cliapi_server_hpp__

#include <e32base.h>

// This is a Symbian active object that receives client requests from
// the kernel and directs them to the appropriate session.
// 
// Either the (inherited) StartL or Start method may be used to
// actually get the server running. Naturally there also is a Cancel
// method, as this class is derived from CActive.
class CCliapiServer : public CServer2
{
public:
  static CCliapiServer* NewLC();
  static CCliapiServer* NewL();

  // An overload of an inherited method.
  // 
  // The StartL (or Start) method specifies the name of the server;
  // the name is required when clients try to connect. We provide an
  // overload of Start that sets the name to be what the client
  // library expects.
  TInt Start();

  void AddSession();
  void DropSession();
  void Send(const TDesC& aMessage); // xxx may not want this

private:
  CCliapiServer();
  void ConstructL();
  CSession2* NewSessionL(const TVersion& aVersion, 
			 const RMessage2& aMessage) const;

private:
  TInt iSessionCount;
};

#endif /* __epoc_cliapi_server_hpp__ */
