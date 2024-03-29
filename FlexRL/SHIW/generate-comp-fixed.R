# R script for downloading and processing the historical "Components" data set 
# from the Bank of Italy's Survey of Household Income and Wealth.
# 
# MIT License
# 
# Copyright 2019 Neil Marchant: https://github.com/ngmarchant/shiw
#
# Permission is hereby granted, free of charge, to any person obtaining a copy 
# of this software and associated documentation files (the "Software"), to deal 
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in 
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE.

# Where to write the output CSV file
outFile <- "SHIWdata.csv"

getwd()

# Download the historical database from the Bank of Italy's website
temp <- tempfile()
download.file("https://www.bancaditalia.it/statistiche/tematiche/indagini-famiglie-imprese/bilanci-famiglie/distribuzione-microdati/documenti/storico/storico_ascii.zip?language_id=1", temp, mode="wb")
unzip(temp, "comp.csv")

comp <- read.csv("comp.csv", stringsAsFactors = FALSE)
unlink(temp)

# Now we infer unique identifiers for participants across waves

# Filter out years before 1989, since they do not contain NORDP
comp <- comp[comp$ANNO >= 1989,]

# Infer ANASC (date of birth) based on current year and age (since ANASC 
# has a lot of missing values)
comp["ANASCI"] <- comp$ANNO - comp$ETA

# Add column for corrected NORD (household member id) which will be constant 
# across waves
comp["NORDC"] <- NA

# Pre-sort
comp <- comp[order(comp$NQUEST, comp$ANNO, comp$NORD),]

# Loop through records by NQUEST, ANNO, then NORD, computing a corrected 
# NORDC based on NORDP (household member id in the previous wave)
# Initialize
i <- 1
prev.NQUEST <- -1
prev.ANNO <- -1
prev.NORDCs <- new.env(hash = TRUE)
this.NORDCs <- new.env(hash = TRUE)
ctr.NORDC <- 1
first.ANNO <- TRUE
comp.nrow <- nrow(comp)
# For printing progress
increment5 <- floor(0.025*comp.nrow) # 2.5% of rows
countdown <- increment5
while (i <= comp.nrow) {
  if (prev.NQUEST != comp$NQUEST[i]) {
    # Starting a new NQUEST
    prev.NORDCs <- new.env(hash = TRUE)
    this.NORDCs <- new.env(hash = TRUE)
    ctr.NORDC <- 1
    first.ANNO <- TRUE
    prev.NQUEST <- comp$NQUEST[i]
    prev.ANNO <- comp$ANNO[i]
  } else {
    # Still in same NQUEST
    if (prev.ANNO != comp$ANNO[i]) {
      # Starting a new ANNO
      prev.NORDCs <- this.NORDCs
      this.NORDCs <- new.env(hash = TRUE)
      first.ANNO <- FALSE
      prev.ANNO <- comp$ANNO[i]
    }
  }
  # Need to convert to strings, so they can be used as keys for prev.NORDCs, this.NORDCs
  NORD <- as.character(comp$NORD[i])
  NORDP <- as.character(comp$NORDP[i])
  if (is.na(NORDP) || first.ANNO) {
    # A new household member (never seen before)
    this.NORDC <- ctr.NORDC
    ctr.NORDC <- ctr.NORDC + 1
  } else {
    # Household member should exist in the previous wave
    prev.NORDC <- prev.NORDCs[[NORDP]]
    if (is.null(prev.NORDC)) {
      # NORDP references a household member in the previous wave that doesn't 
      # exist. Assume household member is new.
      this.NORDC <- ctr.NORDC
      ctr.NORDC <- ctr.NORDC + 1
    } else this.NORDC <- prev.NORDC
  }
  this.NORDCs[[NORD]] <- this.NORDC
  comp$NORDC[i] <- this.NORDC
  i <- i + 1
  # Show progress
  countdown <- countdown - 1
  if (countdown == 0) {
    countdown <- increment5
    cat(".")
  }
}

comp$ID = do.call(paste, c(comp[,c("NQUEST", "NORDC")], list(sep="_")))

# Write the "fixed" dataset to disk.
write.csv(comp, outFile, row.names = FALSE, na = "NA", quote = FALSE)
