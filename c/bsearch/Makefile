#########################################
# VARIABLES - overridable by make flags #
#########################################
# Some useful CFLAGS/LDFLAGS:
#  -march=native - optimize for *my architecture*
#  -march=cpu - optimize for this specific cpu
#  -msse, -msse2, -msse3, -mmmx, -m3dnow - SIMD
#  -D_FORTIFY_SOURCE=2 - enable run-time buffer overflow detection
#  -fasynchronous-unwind-tables - increased reliability of backtraces
#  -fexceptions - table-based thread cancellation
#  -fpie -Wl,-pie - full ASLR (address space layout randomization)
#  -fpic -shared - no text relocations for shared libraries
#  -fsanitize=signed-integer-overflow - abort on signed integer overflow
#  -finstrument-functions - adds calls to user-supplied profiling functions at entry- and exit-points
#  -Wfloat-equal - warning on equality-checking floats
#  -Wundef - warn if uninitialized identifiers are used in #if
#  -Wshadow - warn when a variable is shadowed
#  -Wpointer-arith - warn if something depends on the size of a function or void*
#  -Wcast-align - warn when a pointer is cast such that an alignment of the target is increased
#  -Wstrict-prototypes - warn when a function is declared without types
#  -Wstrict-overflow=5 - warns about cases where compiler optimizes based on the assumtion that a signed overflow does not occur
#  -Wwrite-strings - give string constants the type const char[len] so that copying the address of one into a non-const char* pointer gets a warning
#  -Wswitch-default - warn when a switch does not have a default case
#  -Wswitch-enum - warn when a switch lacks a case for an enum case
#  -Wconversion - warn for implicit conversations that may alter a value
#  -Wunreachable-code - warn for unreachable code
#  -Wformat=2 - printf format warnings
#  -fplugin=annobin - generate data for hardining 
#  -fstack-clash-protection - increased reliability of stack overflow detection
#  -fstack-protector  - stack smashing protection
#  -fstack-protector-all  - stack smashing protection
#  -fstack-protector-strong  - stack smashing protection
#  -grecord-gcc-switches - store compiler flags in debugging info
#  -mcet -fcf-protection - control flow integrity protection
#  -Werror=format-security - reject potentially unsafe format string arguments
#  -Wl,-z,defs - detect and reject underlinking
#  -Wl,-z,now - disable lazy binding
#  -Wl,-z,relro - read-only segments after relocation
#  -Wdouble-promition - very useful in embedded spaces to make sure you're using the correct float type
#  -fno-common - guards against clashing global names that can cause issues
#  -fstack-usage - makes compiler emit a .su file with stack usage info
#  -Wstack-usage=<stack_limit> - limit stack usage to <stack_limit> bytes
#  -save-temps - makes compiler leave behind the results from preprocessor and assembly
#  -fprofile-generate=<dir> - generate profile for profile-guided optimization
#  -fprofile-use=<dir> - use this profile for profile-guided optimization
#  -flto - link time optimization
#  
# However, note that
#  -fsanitize=address (address sanitizer, was previously named -fmudflap) disable ABI
#   compatibility with future library versions, so for long-term use across multiple OSs, this
#   can have unforseen consequences
# -O0 may improve debugging experience, but disables any hardening that depend on optimizations.
CFLAGS         = -Iinclude -Isrc -Wall -Wextra \
				 -Wno-implicit-fallthrough -Wno-unused-const-variable \
				 -std=c11 -g3 -Os -D_FORTIFY_SOURCE=2 -fexceptions \
				 -fasynchronous-unwind-tables -fpie -Wl,-pie \
				 -fstack-protector-strong -grecord-gcc-switches \
				 -Werror=format-security \
				 -Werror=implicit-function-declaration -Wl,-z,defs -Wl,-z,now \
				 -Wl,-z,relro $(EXTRA_CFLAGS)
LDFLAGS        = $(EXTRA_LDFLAGS)
LDLIBS         = $(EXTRA_LDLIBS)
DESTDIR        = /
PREFIX         = /usr/local
RAGEL          = ragel
RAGELFLAGS     = -G2 $(EXTRA_RAGELFLAGS)
INSTALL        = install
BEAR           = bear
COMPLEXITY     = complexity
CFLOW          = cflow
SED            = sed
NEATO          = neato
CTAGS          = ctags
UPX            = upx
STRIP          = strip
SCAN_BUILD     = scan-build
Q              = @
CC_COLOR       = \033[0;34m
LD_COLOR       = \033[0;33m
TEST_COLOR     = \033[0;35m
INSTALL_COLOR  = \033[0;32m
NO_COLOR       = \033[m


bsearch: bsearch.o


vpath %.c src/


%: %.o
	@printf "$(LD_COLOR)LD$(NO_COLOR) $@\n"
	$(Q)$(CROSS_COMPILE)$(CC) $(LDFLAGS) -o $@ $^ $(LOADLIBES) $(LDLIBS)


.PHONY: clean
clean:
	rm -f *.o

.PHONY: distclean
distclean: clean
	rm -f psearch
