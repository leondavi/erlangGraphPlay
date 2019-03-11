from traces_extraction import *




CurrentTrace = trace("data/small_check.csv","Check")

CurrentTrace.extract_statistics()
CurrentTrace.print_to_file()