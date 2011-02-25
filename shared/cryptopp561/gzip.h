#ifndef CRYPTOPP_GZIP_H
#define CRYPTOPP_GZIP_H

#include "zdeflate.h"
#include "crc.h"

NAMESPACE_BEGIN(CryptoPP)

/// GZIP Compression (RFC 1952)
class Gzip : public Deflator
{
public:
	Gzip(BufferedTransformation *attachment=NULL, unsigned int deflateLevel=DEFAULT_DEFLATE_LEVEL, unsigned int log2WindowSize=DEFAULT_LOG2_WINDOW_SIZE, bool detectUncompressible=true)
		: Deflator(attachment, deflateLevel, log2WindowSize, detectUncompressible) {}
	Gzip(const NameValuePairs &parameters, BufferedTransformation *attachment=NULL)
		: Deflator(parameters, attachment) {}

protected:
	enum {MAGIC1=0x1f, MAGIC2=0x8b,   // flags for the header
		  DEFLATED=8, FAST=4, SLOW=2};

	void WritePrestreamHeader();
	void ProcessUncompressedData(const byte *string, size_t length);
	void WritePoststreamTail();

	word32 m_totalLen;
	CRC32 m_crc;
};

NAMESPACE_END

#endif
