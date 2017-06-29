# -------------------------
# Set Work Dir
# -------------------------
set_work_dir <- function() {
  dirname(sys.frame(1)$ofile)
}
set_work_dir
