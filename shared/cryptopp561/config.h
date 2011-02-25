#ifndef CRYPTOPP_CONFIG_H
#define CRYPTOPP_CONFIG_H

// ***************** Important Settings ********************

// define this if running on a little-endian CPU
// Linux on Intel and Symbian on ARM are both little endian
#	define IS_LITTLE_ENDIAN

// define this if you want to disable all OS-dependent features,
// such as sockets and OS-provided random number generators
#define NO_OS_DEPENDENCE

// Define this to 1 to enforce the requirement in FIPS 186-2 Change Notice 1 that only 1024 bit moduli be used
#ifndef DSA_1024_BIT_MODULUS_ONLY
#	define DSA_1024_BIT_MODULUS_ONLY 1
#endif

// ***************** Less Important Settings ***************

#define CRYPTOPP_DISABLE_AESNI 1

#define GZIP_OS_CODE 0

// Define this if, for the linear congruential RNG, you want to use
// the original constants as specified in S.K. Park and K.W. Miller's
// CACM paper.
// #define LCRNG_ORIGINAL_NUMBERS

// choose which style of sockets to wrap (mostly useful for cygwin which has both)
#define PREFER_BERKELEY_STYLE_SOCKETS
// #define PREFER_WINDOWS_STYLE_SOCKETS

// set the name of Rijndael cipher, was "Rijndael" before version 5.3
#define CRYPTOPP_RIJNDAEL_NAME "AES"

// ***************** Important Settings Again ********************
// But the defaults should be ok.

// namespace support is now required
#ifdef NO_NAMESPACE
#	error namespace support is now required
#endif

#ifdef CRYPTOPP_DOXYGEN_PROCESSING
// Avoid putting "CryptoPP::" in front of everything in Doxygen output
#	define CryptoPP
#	define NAMESPACE_BEGIN(x)
#	define NAMESPACE_END
// Get Doxygen to generate better documentation for these typedefs
#	define DOCUMENTED_TYPEDEF(x, y) class y : public x {};
#else
#	define NAMESPACE_BEGIN(x) namespace x {
#	define NAMESPACE_END }
#	define DOCUMENTED_TYPEDEF(x, y) typedef x y;
#endif
#define ANONYMOUS_NAMESPACE_BEGIN namespace {
#define USING_NAMESPACE(x) using namespace x;
#define DOCUMENTED_NAMESPACE_BEGIN(x) namespace x {
#define DOCUMENTED_NAMESPACE_END }

// What is the type of the third parameter to bind?
// For Unix, the new standard is ::socklen_t (typically unsigned int), and the old standard is int.
// Unfortunately there is no way to tell whether or not socklen_t is defined.
// To work around this, TYPE_OF_SOCKLEN_T is a macro so that you can change it from the makefile.
#ifndef TYPE_OF_SOCKLEN_T
#	if defined(_WIN32) || defined(__CYGWIN__)
#		define TYPE_OF_SOCKLEN_T int
#	else
#		define TYPE_OF_SOCKLEN_T ::socklen_t
#	endif
#endif

typedef unsigned char byte;		// put in global namespace to avoid ambiguity with other byte typedefs

NAMESPACE_BEGIN(CryptoPP)

typedef unsigned short word16;
typedef unsigned int word32;

#if defined(_MSC_VER) || defined(__BORLANDC__)
	typedef unsigned __int64 word64;
	#define W64LIT(x) x##ui64
#else
	typedef unsigned long long word64;
	#define W64LIT(x) x##ULL
#endif

// define large word type, used for file offsets and such
typedef word64 lword;
const lword LWORD_MAX = W64LIT(0xffffffffffffffff);

#ifdef __GNUC__
	#define CRYPTOPP_GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#endif

// define hword, word, and dword. these are used for multiprecision integer arithmetic
// Intel compiler won't have _umul128 until version 10.0. See http://softwarecommunity.intel.com/isn/Community/en-US/forums/thread/30231625.aspx
#if (defined(_MSC_VER) && (!defined(__INTEL_COMPILER) || __INTEL_COMPILER >= 1000) && (defined(_M_X64) || defined(_M_IA64))) || (defined(__DECCXX) && defined(__alpha__)) || (defined(__INTEL_COMPILER) && defined(__x86_64__)) || (defined(__SUNPRO_CC) && defined(__x86_64__))
	typedef word32 hword;
	typedef word64 word;
#else
	#define CRYPTOPP_NATIVE_DWORD_AVAILABLE
	#if defined(__alpha__) || defined(__ia64__) || defined(_ARCH_PPC64) || defined(__x86_64__) || defined(__mips64) || defined(__sparc64__)
		#if defined(__GNUC__) && !defined(__INTEL_COMPILER) && !(CRYPTOPP_GCC_VERSION == 40001 && defined(__APPLE__)) && CRYPTOPP_GCC_VERSION >= 30400
			// GCC 4.0.1 on MacOS X is missing __umodti3 and __udivti3
			// mode(TI) division broken on amd64 with GCC earlier than GCC 3.4
			typedef word32 hword;
			typedef word64 word;
			typedef __uint128_t dword;
			typedef __uint128_t word128;
			#define CRYPTOPP_WORD128_AVAILABLE
		#else
			// if we're here, it means we're on a 64-bit CPU but we don't have a way to obtain 128-bit multiplication results
			typedef word16 hword;
			typedef word32 word;
			typedef word64 dword;
		#endif
	#else
		// being here means the native register size is probably 32 bits or less
		#define CRYPTOPP_BOOL_SLOW_WORD64 1
		typedef word16 hword;
		typedef word32 word;
		typedef word64 dword;
	#endif
#endif
#ifndef CRYPTOPP_BOOL_SLOW_WORD64
	#define CRYPTOPP_BOOL_SLOW_WORD64 0
#endif

const unsigned int WORD_SIZE = sizeof(word);
const unsigned int WORD_BITS = WORD_SIZE * 8;

NAMESPACE_END

#ifndef CRYPTOPP_L1_CACHE_LINE_SIZE
	// This should be a lower bound on the L1 cache line size. It's used for defense against timing attacks.
	#if defined(_M_X64) || defined(__x86_64__)
		#define CRYPTOPP_L1_CACHE_LINE_SIZE 64
	#else
		// L1 cache line size is 32 on Pentium III and earlier
		#define CRYPTOPP_L1_CACHE_LINE_SIZE 32
	#endif
#endif

#if defined(_MSC_VER)
	#if _MSC_VER == 1200
		#include <malloc.h>
	#endif
	#if _MSC_VER > 1200 || defined(_mm_free)
		#define CRYPTOPP_MSVC6PP_OR_LATER		// VC 6 processor pack or later
	#else
		#define CRYPTOPP_MSVC6_NO_PP			// VC 6 without processor pack
	#endif
#endif

#ifndef CRYPTOPP_ALIGN_DATA
	#if defined(CRYPTOPP_MSVC6PP_OR_LATER)
		#define CRYPTOPP_ALIGN_DATA(x) __declspec(align(x))
	#elif defined(__GNUC__)
		#define CRYPTOPP_ALIGN_DATA(x) __attribute__((aligned(x)))
	#else
		#define CRYPTOPP_ALIGN_DATA(x)
	#endif
#endif

#ifndef CRYPTOPP_SECTION_ALIGN16
	#if defined(__GNUC__) && !defined(__APPLE__)
		// the alignment attribute doesn't seem to work without this section attribute when -fdata-sections is turned on
		#define CRYPTOPP_SECTION_ALIGN16 __attribute__((section ("CryptoPP_Align16")))
	#else
		#define CRYPTOPP_SECTION_ALIGN16
	#endif
#endif

#define CRYPTOPP_FASTCALL

// VC60 workaround: it doesn't allow typename in some places
#define CPP_TYPENAME typename

// VC60 workaround: can't cast unsigned __int64 to float or double
#define CRYPTOPP_VC6_INT64

#define CRYPTOPP_NO_VTABLE

#ifndef CRYPTOPP_DISABLE_UNCAUGHT_EXCEPTION
#define CRYPTOPP_UNCAUGHT_EXCEPTION_AVAILABLE
#endif

#ifdef CRYPTOPP_DISABLE_X86ASM		// for backwards compatibility: this macro had both meanings
#define CRYPTOPP_DISABLE_ASM
#define CRYPTOPP_DISABLE_SSE2
#endif

#if !defined(CRYPTOPP_DISABLE_ASM) && ((defined(_MSC_VER) && defined(_M_IX86)) || (defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))))
	// C++Builder 2010 does not allow "call label" where label is defined within inline assembly
	#define CRYPTOPP_X86_ASM_AVAILABLE

	#if !defined(CRYPTOPP_DISABLE_SSE2) && (defined(CRYPTOPP_MSVC6PP_OR_LATER) || CRYPTOPP_GCC_VERSION >= 30300)
		#define CRYPTOPP_BOOL_SSE2_ASM_AVAILABLE 1
	#else
		#define CRYPTOPP_BOOL_SSE2_ASM_AVAILABLE 0
	#endif

	// SSSE3 was actually introduced in GNU as 2.17, which was released 6/23/2006, but we can't tell what version of binutils is installed.
	// GCC 4.1.2 was released on 2/13/2007, so we'll use that as a proxy for the binutils version.
	#if !defined(CRYPTOPP_DISABLE_SSSE3) && (_MSC_VER >= 1400 || CRYPTOPP_GCC_VERSION >= 40102)
		#define CRYPTOPP_BOOL_SSSE3_ASM_AVAILABLE 1
	#else
		#define CRYPTOPP_BOOL_SSSE3_ASM_AVAILABLE 0
	#endif
#endif

#if !defined(CRYPTOPP_DISABLE_ASM) && defined(_MSC_VER) && defined(_M_X64)
	#define CRYPTOPP_X64_MASM_AVAILABLE
#endif

#if !defined(CRYPTOPP_DISABLE_ASM) && defined(__GNUC__) && defined(__x86_64__)
	#define CRYPTOPP_X64_ASM_AVAILABLE
#endif

#if !defined(CRYPTOPP_DISABLE_SSE2) && (defined(CRYPTOPP_MSVC6PP_OR_LATER) || defined(__SSE2__))
	#define CRYPTOPP_BOOL_SSE2_INTRINSICS_AVAILABLE 1
#else
	#define CRYPTOPP_BOOL_SSE2_INTRINSICS_AVAILABLE 0
#endif

#if !defined(CRYPTOPP_DISABLE_SSSE3) && !defined(CRYPTOPP_DISABLE_AESNI) && CRYPTOPP_BOOL_SSE2_INTRINSICS_AVAILABLE && (CRYPTOPP_GCC_VERSION >= 40400 || _MSC_FULL_VER >= 150030729 || __INTEL_COMPILER >= 1110)
	#define CRYPTOPP_BOOL_AESNI_INTRINSICS_AVAILABLE 1
#else
	#define CRYPTOPP_BOOL_AESNI_INTRINSICS_AVAILABLE 0
#endif

#if CRYPTOPP_BOOL_SSE2_INTRINSICS_AVAILABLE || CRYPTOPP_BOOL_SSE2_ASM_AVAILABLE || defined(CRYPTOPP_X64_MASM_AVAILABLE)
	#define CRYPTOPP_BOOL_ALIGN16_ENABLED 1
#else
	#define CRYPTOPP_BOOL_ALIGN16_ENABLED 0
#endif

// how to allocate 16-byte aligned memory (for SSE2)
#if defined(CRYPTOPP_MSVC6PP_OR_LATER)
	#define CRYPTOPP_MM_MALLOC_AVAILABLE
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
	#define CRYPTOPP_MALLOC_ALIGNMENT_IS_16
#elif defined(__linux__) || defined(__sun__) || defined(__CYGWIN__)
	#define CRYPTOPP_MEMALIGN_AVAILABLE
#else
	#define CRYPTOPP_NO_ALIGNED_ALLOC
#endif

// how to disable inlining
#if defined(_MSC_VER) && _MSC_VER >= 1300
#	define CRYPTOPP_NOINLINE_DOTDOTDOT
#	define CRYPTOPP_NOINLINE __declspec(noinline)
#elif defined(__GNUC__)
#	define CRYPTOPP_NOINLINE_DOTDOTDOT
#	define CRYPTOPP_NOINLINE __attribute__((noinline))
#else
#	define CRYPTOPP_NOINLINE_DOTDOTDOT ...
#	define CRYPTOPP_NOINLINE 
#endif

// how to declare class constants
#if (defined(_MSC_VER) && _MSC_VER <= 1300) || defined(__INTEL_COMPILER)
#	define CRYPTOPP_CONSTANT(x) enum {x};
#else
#	define CRYPTOPP_CONSTANT(x) static const int x;
#endif

#if defined(_M_X64) || defined(__x86_64__)
	#define CRYPTOPP_BOOL_X64 1
#else
	#define CRYPTOPP_BOOL_X64 0
#endif

// see http://predef.sourceforge.net/prearch.html
#if defined(_M_IX86) || defined(__i386__) || defined(__i386) || defined(_X86_) || defined(__I86__) || defined(__INTEL__)
	#define CRYPTOPP_BOOL_X86 1
#else
	#define CRYPTOPP_BOOL_X86 0
#endif

#if CRYPTOPP_BOOL_X64 || CRYPTOPP_BOOL_X86 || defined(__powerpc__)
	#define CRYPTOPP_ALLOW_UNALIGNED_DATA_ACCESS
#endif

#define CRYPTOPP_VERSION 561

// ***************** DLL related ********************

#define CRYPTOPP_DLL
#define CRYPTOPP_API

#define CRYPTOPP_DLL_TEMPLATE_CLASS template class CRYPTOPP_DLL

#define CRYPTOPP_STATIC_TEMPLATE_CLASS template class

#endif
