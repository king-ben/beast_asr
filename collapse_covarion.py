import os, fnmatch
def findReplace(directory, find, replace, filePattern):
    for path, dirs, files in os.walk(os.path.abspath(directory)):
        for filename in fnmatch.filter(files, filePattern):
            filepath = os.path.join(path, filename)
            with open(filepath) as f:
                s = f.read()
            s = s.replace(find, replace)
            with open(filepath, "w") as f:
                f.write(s)

findReplace("", "\t2", "\t0", "asr_logger*")
findReplace("", "\t3", "\t1", "asr_logger*")
findReplace("", "\n2", "\n0", "asr_logger*")
findReplace("", "\n3", "\n1", "asr_logger*")
