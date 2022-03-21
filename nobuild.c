#define NOBUILD_IMPLEMENTATION
#include "nobuild.h"

int main(int argc, char* argv[])
{
	GO_REBUILD_URSELF(argc, argv);
	MKDIRS("build", "lib");
	MKDIRS("build", "bin");
// compiling libbr
	CMD(
		"cc", 
		"-Wno-deprecated-declarations", 
		"-Wno-tautological-constant-out-of-range-compare",
		"-c",
		"-o", PATH("build", "lib", "core.o"), 
		"-I", "include", 
		PATH("src", "core.c")
	); 
	CMD(
		"cc", 
		"-shared", 
		"-o", PATH("build", "lib", "libbr.dylib"), 
		PATH("build", "lib", "core.o")
	);
// compiling libbrb
	CMD(
		"cc", 
		"-c",
		"-o", PATH("build", "lib", "brb.o"), 
		"-I", "include",
		"-DLIBBRB=1",
		PATH("src", "brex.c")
	); 
	CMD(
		"cc", 
		"-shared", 
		"-o", PATH("build", "lib", "libbrb.dylib"), 
		PATH("build", "lib", "core.o"),
		PATH("build", "lib", "brb.o")
	);
	CMD("rm", PATH("build", "lib", "brb.o"));
	CMD("rm", PATH("build", "lib", "core.o"));

	CMD(
		"cc",
		"-I", "include",
		"-L", PATH("build", "lib"),
		"-lbr",
		"-o", PATH("build", "bin", "brs"),
		PATH("src", "brs.c")
	);
	CMD(
		"cc",
		"-I", "include",
		"-L", PATH("build", "lib"),
		"-lbrb",
		"-o", PATH("build", "bin", "brex"),
		PATH("src", "brex.c")
	);
	/*CMD(
		"cc",
		"-I", "include",
		"-L", PATH("build", "lib"),
		"-lbrb",
		"-o", PATH("build", "bin", "brn"),
		PATH("src", "brn.c")
	);*/
}
