/*
 
Copyright (c) 2009 David Bolcsfoldi

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*/

#include <es_sock.h>
#include <in_sock.h>

#include "active_sock.h"
#include "activesocket.h"
#include "lauxlib.h"

_LIT8(KSocketServ, "active_socket.serv\0");
_LIT8(KSocketType, "active_socket.socket\0");

const TInt KDefaultBufferSize = 4096;

using namespace ALua;

static TUint to_addr_family(const char* aAddr)
	{
	TPtrC8 addr(reinterpret_cast<const unsigned char*>(aAddr), User::StringLength(reinterpret_cast<const unsigned char*>(aAddr)));
	_LIT8(KInet, "inet");
	
	if (addr == KInet) 
		{
		return KAfInet;
		}
	else
		{
		// Raise error	
		return 0;
		}
	}	

static TUint to_sock_type(const char* aType)
	{
	TPtrC8 type(reinterpret_cast<const unsigned char*>(aType), User::StringLength(reinterpret_cast<const unsigned char*>(aType)));

	_LIT8(KStream, "stream");
	_LIT8(KDatagram, "datagram");

	if (type == KStream) 
		{
		return KSockStream; 
		}
	else if (type == KDatagram) 
		{
		return KSockDatagram;
		}
	else
		{
		// Raise error
		return 0;
		}
	}

static TUint to_protocol(const char* aProtocol)
	{
	TPtrC8 protocol(reinterpret_cast<const unsigned char*>(aProtocol), User::StringLength(reinterpret_cast<const unsigned char*>(aProtocol)));

	_LIT8(KTcp, "tcp");
	_LIT8(KUdp, "udp");

	if (protocol == KTcp) 
		{
		return KProtocolInetTcp;
		}
	else if(protocol == KUdp)
		{
		return KProtocolInetUdp;
		}
	else
		{
		// Raise error
		return 0;
		}
	}

static TInt socket_open(lua_State* L)
	{
	lua_pushstring(L, reinterpret_cast<const char*>(KSocketServ().Ptr()));
	lua_gettable(L, LUA_REGISTRYINDEX);

	TInt handle = luaL_checkint(L, -1);

	// Remove handle from stack
	lua_pop(L, 1);

	RSocketServ s;
	s.SetHandle(handle);

	TUint addrFamily = to_addr_family(luaL_optstring(L, 1, "inet"));
	TUint sockType = to_sock_type(luaL_optstring(L, 2, "stream"));
	TUint protocol = to_protocol(luaL_optstring(L, 3, "tcp"));

	// Allocate socket using user data so that we can return it from the function,
	// and have it collected using __gc
	CActiveSocket** socket = static_cast<CActiveSocket**>(lua_newuserdata(L, sizeof(CActiveSocket*)));
	TRAPD(err, *socket = CActiveSocket::NewL(s, addrFamily, sockType, protocol));
	
	if (err != KErrNone)
		{
		// Raise massive error!
		}
	
	// Set up metadata table

	lua_pushstring(L, reinterpret_cast<const char*>(KSocketType().Ptr()));
	lua_gettable(L, LUA_REGISTRYINDEX);
	lua_setmetatable(L, 1);

	return 1;
	}

static TInt socket_connect(lua_State* L)
	{
	CActiveSocket** socket = static_cast<CActiveSocket**>(luaL_checkudata(L, -3, reinterpret_cast<const char*>(KSocketType().Ptr())));

	size_t len = 0;
	const char* u = luaL_checklstring(L, -2, &len);
	TPtrC8 uri(reinterpret_cast<const unsigned char*>(u), len);

	TInt port = luaL_checklong(L, -1);
	return (*socket)->Connect(*static_cast<CActiveFunction*>(active_ctx(L)), uri, port);
	}

static TInt socket_read(lua_State* L)
	{
	CActiveSocket** socket = static_cast<CActiveSocket**>(luaL_checkudata(L, 1, reinterpret_cast<const char*>(KSocketType().Ptr())));
	TInt bufferSize = luaL_optlong(L, 2, KDefaultBufferSize);

	return (*socket)->Read(*static_cast<CActiveFunction*>(active_ctx(L)), bufferSize);
	}

static TInt socket_write(lua_State* L)
	{
	CActiveSocket** socket = static_cast<CActiveSocket**>(luaL_checkudata(L, -2, reinterpret_cast<const char*>(KSocketType().Ptr())));
	
	size_t len = 0;
	const char* u = luaL_checklstring(L, -1, &len);
	TPtrC8 data(reinterpret_cast<const unsigned char*>(u), len);

	return (*socket)->Write(*static_cast<CActiveFunction*>(active_ctx(L)), data);
	}

static TInt socket_close(lua_State* L)
	{
	CActiveSocket** socket = static_cast<CActiveSocket**>(luaL_checkudata(L, -1, reinterpret_cast<const char*>(KSocketType().Ptr())));
	return (*socket)->Close();
	}

static TInt socket_gc(lua_State* L)
	{
	CActiveSocket** socket = static_cast<CActiveSocket**>(luaL_checkudata(L, -1, reinterpret_cast<const char*>(KSocketType().Ptr())));
	TInt ret = (*socket)->Close();
	delete *socket;
	return ret;
	}

static const struct luaL_Reg active_socket[] =
	{
	{"open", socket_open},
	{"connect", socket_connect},
	{"read", socket_read},
	{"write", socket_write},
	{"close", socket_close},
	{NULL, NULL}
	};

EXPORT_C int active_sock_open(lua_State* L)
	{
	RSocketServ sockServ;

	if (sockServ.Connect() != KErrNone)
		{
		return 0;
		}

	/* Register socket server with global context */
	lua_pushstring(L, reinterpret_cast<const char*>(KSocketServ().Ptr()));
	lua_pushnumber(L, sockServ.Handle());
	lua_settable(L, LUA_REGISTRYINDEX);

	luaL_register(L, "socket", active_socket);

	luaL_newmetatable(L, reinterpret_cast<const char*>(KSocketType().Ptr()));
	lua_pushcfunction(L, socket_gc);
	lua_setfield(L, -2, "__gc");
	
	lua_pushstring(L, reinterpret_cast<const char*>(KSocketType().Ptr()));
	lua_settable(L, LUA_REGISTRYINDEX);

	return 1;
	}

