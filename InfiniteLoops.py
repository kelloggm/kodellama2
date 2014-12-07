# Owner MattIrv
# Tool for making the ocaml version accept infinite loops
# Find/replace adapted from http://stackoverflow.com/questions/39086/search-and-replace-a-line-in-a-file-in-python

from tempfile import mkstemp
from shutil import move
from os import remove, close

# Create temp file
fh, abs_path = mkstemp()
file_path = 'ocaml/Evals.ml'
new_file = open(abs_path, 'w')
old_file = open(file_path)
for line in old_file:
	new_file.write(line.replace("  | S n' ->", "  | n' ->"))
# Close temp file
new_file.close()
close(fh)
old_file.close()
# Remove original file
remove(file_path)
# Move new file
move(abs_path, file_path)
