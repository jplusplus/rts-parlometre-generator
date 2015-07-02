BUILD_DIR=`pwd`
VENDOR_DIR="$BUILD_DIR/vendor"
CRAN_MIRROR="http://cran.revolutionanalytics.com"

R -s <<RPROG
  Sys.setenv(BUILD_DIR="$BUILD_DIR")
  setwd("$BUILD_DIR")
  r <- getOption("repos");
  r["CRAN"] <- "$CRAN_MIRROR";
  options(repos=r);
  `cat $BUILD_DIR/init.r`
RPROG
