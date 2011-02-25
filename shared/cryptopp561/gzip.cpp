// gzip.cpp - written and placed in the public domain by Wei Dai

#include "config.h"
#include "gzip.h"

NAMESPACE_BEGIN(CryptoPP)

void Gzip::WritePrestreamHeader()
{
	m_totalLen = 0;
	m_crc.Restart();

	AttachedTransformation()->Put(MAGIC1);
	AttachedTransformation()->Put(MAGIC2);
	AttachedTransformation()->Put(DEFLATED);
	AttachedTransformation()->Put(0);		// general flag
	AttachedTransformation()->PutWord32(0);	// time stamp
	byte extra = (GetDeflateLevel() == 1) ? FAST : ((GetDeflateLevel() == 9) ? SLOW : 0);
	AttachedTransformation()->Put(extra);
	AttachedTransformation()->Put(GZIP_OS_CODE);
}

void Gzip::ProcessUncompressedData(const byte *inString, size_t length)
{
	m_crc.Update(inString, length);
	m_totalLen += (word32)length;
}

void Gzip::WritePoststreamTail()
{
	SecByteBlock crc(4);
	m_crc.Final(crc);
	AttachedTransformation()->Put(crc, 4);
	AttachedTransformation()->PutWord32(m_totalLen, LITTLE_ENDIAN_ORDER);
}

NAMESPACE_END
