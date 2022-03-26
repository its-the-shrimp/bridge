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
#	define BRB_MODIFIED  mod_flags[1]
#	define BRS_MODIFIED  mod_flags[2]
#	define BRBX_MODIFIED mod_flags[3]
#	define BRBC_MODIFIED mod_flags[4]
	bool mod_flags[] = {
		is_path1_modified_after_path2(PATH("src", "core.c"), PATH("build", "lib", "libbr.dylib")),
		is_path1_modified_after_path2(PATH("src", "brb.c"), PATH("build", "lib", "libbrb.dylib")),
		is_path1_modified_after_path2(PATH("src", "brs.c"), PATH("build", "bin", "brs")),
		is_path1_modified_after_path2(PATH("src", "brbx.c"), PATH("build", "bin", "brbx")),
		is_path1_modified_after_path2(PATH("src", "brbc.c"), PATH("build", "bin", "brbc"))
	};

	if (argc > 1 ? !strcmp("-f", argv[1]) : false) {
		for (int i = sizeof(mod_flags); i >= 0; i--) { mod_flags[i] = 1; }
	} else {
		FOREACH_FILE_IN_DIR(header_name, "include",
			if (header_name[0] != '.' ? is_path1_modified_after_path2(PATH("include", header_name), PATH("build", "lib", "libbr.dylib")) : 0) {
				for (int i = sizeof(mod_flags); i >= 0; i--) { mod_flags[i] = 1; }
				printf("true true %s\n", header_name);
				goto compilation;
			}
		);
	}
compilation:

// compiling libbr
	if (CORE_MODIFIED || BRB_MODIFIED) {
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
			PATH("src", "brb.c")
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

	if (BRBX_MODIFIED) {
		CMD(
			"cc",
			"-I", "include",
			"-L", PATH("build", "lib"),
			"-lbrb",
			"-o", PATH("build", "bin", "brbx"),
			PATH("src", "brbx.c")
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

	if (!PATH_EXISTS("/usr/local/bin/brs")) {
		CMD("sudo", "ln", "-sF", "$PWD/build/bin/brs", "/usr/local/bin/brs");
	}
	if (!PATH_EXISTS("/usr/local/bin/brbx")) {
		CMD("sudo", "ln", "-sF", "$PWD/build/bin/brbx", "/usr/local/bin/brbx");
	}
	if (!PATH_EXISTS("/usr/local/bin/brbc")) {
		CMD("sudo", "ln", "-sF", "$PWD/build/bin/brbc", "/usr/local/bin/brbc");
	}
}
