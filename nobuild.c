#define NOBUILD_IMPLEMENTATION
#include "nobuild.h"

int main(int argc, char* argv[])
{
	GO_REBUILD_URSELF(argc, argv);
	MKDIRS("build", "lib");
	MKDIRS("build", "bin");
	CMD(
		"clang", 
		"-Wno-deprecated-declarations", 
		"-Wno-tautological-constant-out-of-range-compare",
		"-c",
		"-o", PATH("build", "lib", "core.o"), 
		"-I", "include", 
		PATH("src", "core.c")
	); 
	CMD(
		"clang", 
		"-shared", 
		"-o", PATH("build", "lib", "libbr.dylib"), 
		PATH("build", "lib", "core.o")
	);
	CMD("rm", PATH("build", "lib", "core.o"));

	FOREACH_FILE_IN_DIR(filename, "src", 
		if (ENDS_WITH(filename, ".c") && strcmp(filename, "core.c")) {
			CMD(
				"clang", 
				"-L", PATH("build", "lib"), 
				"-lbr", 
				"-I", "include", 
				"-o", PATH("build", "bin", NOEXT(filename)), 
				PATH("src", filename)
			);
		}
	);
}
