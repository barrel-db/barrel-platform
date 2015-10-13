--- jsprf.cpp.orig	2011-03-31 12:11:03.000000000 +0200
+++ jsprf.cpp	2011-03-31 12:11:09.000000000 +0200
@@ -58,6 +58,8 @@
 */
 #ifdef HAVE_VA_COPY
 #define VARARGS_ASSIGN(foo, bar)        VA_COPY(foo,bar)
+#elif defined(va_copy)
+#define VARARGS_ASSIGN(foo, bar)        va_copy(foo,bar)
 #elif defined(HAVE_VA_LIST_AS_ARRAY)
 #define VARARGS_ASSIGN(foo, bar)        foo[0] = bar[0]
 #else
