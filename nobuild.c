#define NOBUILD_IMPLEMENTATION
#include "nobuild.h"
#include "stdbool.h"

int main(int argc, char* argv[])
{
	GO_REBUILD_URSELF(argc, argv);
	if (!PATH_EXISTS("build")) {
		MKDIRS("build", "lib");
		MKDIRS("build", "bin");
	}

#	define CORE_MODIFIED mod_flags[0]
#	define BRS_MODIFIED  mod_flags[1]
#	define BRBX_MODIFIED mod_flags[2]
#	define BRBC_MODIFIED mod_flags[3]
	bool mod_flags[] = {
		is_path1_modified_after_path2(PATH("src", "core.c"), PATH("build", "lib", "libbr.dylib")),
		is_path1_modified_after_path2(PATH("src", "brs.c"), PATH("build", "bin", "brs")),
		is_path1_modified_after_path2(PATH("src", "brbx.c"), PATH("build", "bin", "brbx")),
		is_path1_modified_after_path2(PATH("src", "brbc.c"), PATH("build", "bin", "brbc"))
	};

// compiling libbr
	if (CORE_MODIFIED || BRBX_MODIFIED) {
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
			PATH("src", "brbx.c")
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
			"-lbrb",
			"-o", PATH("build", "bin", "brbx"),
			PATH("src", "brbx.c")
		);
	}

	if (BRS_MODIFIED) {
		CMD(
			"cc",
			"-I", "include",
			"-L", PATH("build", "lib"),
			"-lbr",
			"-o", PATH("build", "bin", "brs"),
			PATH("src", "brs.c")
		);
	}
	
	if (BRBC_MODIFIED) {
		CMD(
			"cc",
			"-I", "include",
			"-L", PATH("build", "lib"),
			"-lbrb",
			"-o", PATH("build", "bin", "brbc"),
			PATH("src", "brbc.c")
		);
	}
}
