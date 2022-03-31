#define NOBUILD_IMPLEMENTATION
#include "nobuild.h"
#include "stdbool.h"

char* CWD(void) {
#ifdef _WIN32_

#else
	return getcwd(NULL, 256);
#endif
}

int main(int argc, char* argv[])
{
	Cstr cwd = CWD();
	Cstr LIBPATH = PATH(cwd, "build", "lib");
	Cstr BINPATH = PATH(cwd, "build", "bin");
	Cstr INCLUDEPATH = PATH(cwd, "include");
	Cstr SRCPATH = PATH(cwd, "src");
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
		is_path1_modified_after_path2(PATH("src", "core.c"), PATH(LIBPATH, "libbr.dylib")),
		is_path1_modified_after_path2(PATH("src", "brb.c"), PATH(LIBPATH, "libbrb.dylib")),
		is_path1_modified_after_path2(PATH("src", "brs.c"), PATH(BINPATH, "brs")),
		is_path1_modified_after_path2(PATH("src", "brbx.c"), PATH(BINPATH, "brbx")),
		is_path1_modified_after_path2(PATH("src", "brbc.c"), PATH(BINPATH, "brbc"))
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
			"-o", PATH(LIBPATH, "core.o"), 
			"-I", INCLUDEPATH, 
			PATH(SRCPATH, "core.c")
		); 
		CMD(
			"cc", 
			"-shared", 
			"-o", PATH(LIBPATH, "libbr.dylib"), 
			PATH(LIBPATH, "core.o")
		);
	// compiling libbrb
		CMD(
			"cc", 
			"-c",
			"-o", PATH(LIBPATH, "brb.o"), 
			"-I", INCLUDEPATH,
			PATH(SRCPATH, "brb.c")
		); 
		CMD(
			"cc", 
			"-shared", 
			"-o", PATH(LIBPATH, "libbrb.dylib"), 
			PATH(LIBPATH, "core.o"),
			PATH(LIBPATH, "brb.o")
		);
		CMD("rm", PATH(LIBPATH, "brb.o"));
		CMD("rm", PATH(LIBPATH, "core.o"));
	}

	if (BRS_MODIFIED) {
		CMD(
			"cc",
			"-I", INCLUDEPATH,
			"-L", LIBPATH,
			"-lbr",
			"-o", PATH(BINPATH, "brs"),
			PATH(SRCPATH, "brs.c")
		);
	}

	if (BRBX_MODIFIED) {
		CMD(
			"cc",
			"-I", INCLUDEPATH,
			"-L", LIBPATH,
			"-lbrb",
			"-o", PATH(BINPATH, "brbx"),
			PATH(SRCPATH, "brbx.c")
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
