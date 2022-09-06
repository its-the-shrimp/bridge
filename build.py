#!python3

import sys
import os
import subprocess
from pathlib import Path

compile_all: bool = False
is_release: bool = False

for arg in sys.argv[1:]:
	if arg == "--all":
		compile_all = True
	elif arg == "--release":
		is_release = True
	else:
		print(f"error: unknown option `{arg}`", file=sys.stderr) 
		sys.exit(1)

def is_outdated(path: Path, *other_paths) -> bool:
	if compile_all or not path.exists(): return True
	path_mtime = path.stat().st_mtime
	for other_path in other_paths:
		if path_mtime < other_path.stat().st_mtime: return True

	return False

def exec_cmd(*args):
	print("[CMD] ", *args)
	proc_data = subprocess.run(args, stderr=subprocess.PIPE)
	if proc_data.returncode:
		print("[ERR]:\n" + proc_data.stderr.decode("utf-8") + "\n[END]")
		sys.exit(1)

PWD: Path = Path(".")
LIB: Path = PWD/"build"/"lib"
BIN: Path = PWD/"build"/"bin"
SRC: Path = PWD/"src"
INCLUDE: Path = PWD/"include"
GLOBAL: Path = Path("/usr")/"local"/"bin"
CFLAGS: list[str] = [
	"-std=c11",
	"-I", "include",
	"-Wall", "-Wextra", "-pedantic",
	"-Wno-initializer-overrides", "-Wno-nullability-completeness", "-Wno-extra-semi", "-Wno-unused-parameter", "-Wswitch-enum",
	"-Wno-gnu-designator", # TODO: remove this option
	"-Werror", "-Wfatal-errors",
	"-O3"
]
LFLAGS: list[str] = ["-L", LIB, "-lbrb"]
if not is_release:
	CFLAGS[-1] = "-O0"
	CFLAGS.append("-g")
	CFLAGS.append("-fsanitize=address")

if not LIB.exists(): LIB.mkdir(parents=True)
if not BIN.exists(): BIN.mkdir(parents=True)

if is_outdated(LIB/"libbrb.dylib", *SRC.glob("brb_*"), *INCLUDE.glob("*")):
	exec_cmd("cc", *CFLAGS, "-c", *SRC.glob("brb_*.c"))
	exec_cmd("cc", *CFLAGS, "-shared", "-o", LIB/"libbrb.dylib", *PWD.glob("brb_*.o"))
	for path in PWD.glob("brb_*.o"): path.unlink()

# if is_outdated(BIN/"bridge", SRC/"brc.c", *INCLUDE.glob("*")):
#	exec_cmd("cc", *CFLAGS, *LFLAGS, "-o", BIN/"bridge", SRC/"brc.c")

#if is_outdated(BIN/"brbd", SRC/"brbd.c", *INCLUDE.glob("*")):
#	exec_cmd("cc", *CFLAGS, *LFLAGS, "-o", BIN/"brbd", SRC/"brbd.c")
