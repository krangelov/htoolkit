#include "Internals.h"
#include <stdlib.h>

size_t
pslen(PortString s) {
	size_t len = 0;
	while (*s++ != 0) len++;
	return len;
}

PortString
psdup(PortString s) {
	size_t len = pslen(s);
	PortString copy = malloc((len+1)*sizeof(WCHAR));
	memcpy(copy, s, (len+1)*sizeof(WCHAR));
	return copy;
}

int
pscmp(PortString s1, PortString s2) {
    while(*s1)
    {
    	// if characters differ or end of second string is reached
        if (*s1 != *s2)
            break;

		// move to next pair of characters
        s1++;
        s2++;
    }

    // return the ASCII difference after converting char* to unsigned char*
    return *(const int16_t*)s1 - *(const int16_t*)s2;
}

void
pscpy(PortString s1, PortString s2) {
	memcpy(s1, s2, (pslen(s2)+1)*sizeof(WCHAR));
}

void
pscat(PortString s1, PortString s2) {
	memcpy(s1+pslen(s1), s2, (pslen(s2)+1)*sizeof(WCHAR));
}


void
ftops(double f, WCHAR *szOutput)
{
	char buffer[64];
	char *s = gcvt(f, 10, buffer);
	while (*s)
		*szOutput++ = (WCHAR) *s++;
    *szOutput = L'\0';
}
