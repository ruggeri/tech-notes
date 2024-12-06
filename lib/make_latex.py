import os
from pathlib import Path
import subprocess
import sys

TECH_NOTES_DIR = "/Users/ruggeri/repos/ruggeri/tech-notes"

def run_pdflatex(source_path: Path):
  source_dir = source_path.parent

  env_additions = {
    "TEXINPUTS": f"{TECH_NOTES_DIR}/math/stys:{source_dir}:",
    "max_print_line": "100000"
  }

  cmd = [
    "pdflatex",
    "--file-line-error",
    "--synctex=1",
    "--halt-on-error",
    "--output-directory", source_dir,
    source_path,
  ]
  cmd_env = dict(os.environ)
  cmd_env.update(env_additions)

  print(str(source_path))
  #print(env_additions)
  #print(cmd)

  result = subprocess.run(
    cmd,
    env=cmd_env,
    stdin=subprocess.DEVNULL,
    stdout=subprocess.PIPE
  )

  if result.returncode != 0:
    print(str(result.stdout, encoding="utf-8"))
    sys.exit(1)

def clean(p: Path):
  suffixes = [".aux", ".log", ".out", ".synctex.gz"]
  for suffix in suffixes:
    subprocess.run([
      "rm",
      p.with_suffix(suffix)
    ])

def run(source_paths: list[Path]):
  source_paths = [p.absolute() for p in source_paths]

  for source_path in source_paths:
    run_pdflatex(source_path)
    clean(source_path)

#echo "$FILES" | \
#  grep -v "texlive" | \
#  grep --context=5 "^l"

#./clean-latex
