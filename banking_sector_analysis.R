# Install quantmod if not already installed
if(!requireNamespace("quantmod", quietly = TRUE))
  install.packages("quantmod")
# Load the quantmod package
library(quantmod)


# ============================================================
# XLF (Financial Select Sector SPDR Fund)
# ============================================================

# Specify date range
start_date <- "2025-12-01"
end_date <- Sys.Date()

# Download XLF data from Yahoo Finance
getSymbols("XLF", src = "yahoo",
           from = start_date, to = end_date)

# Inspect XLF
class(XLF)
head(XLF)
tail(XLF)
dim(XLF)

# Charts
plot(XLF, multi.panel = TRUE, yaxis.same = FALSE)
chartSeries(XLF)
chartSeries(XLF["2025-12/"])

# Convert to data frame
xlf_prices <- data.frame(
  Date = index(XLF),
  Open = as.numeric(Op(XLF)),
  High = as.numeric(Hi(XLF)),
  Low = as.numeric(Lo(XLF)),
  Close = as.numeric(Cl(XLF)),
  Volume = as.numeric(Vo(XLF)),
  Adjusted = as.numeric(Ad(XLF))
)
head(xlf_prices)
tail(xlf_prices)

# Export to CSV
write.csv(xlf_prices, "xlf_prices.csv", row.names = FALSE)


# ============================================================
# JPM (JPMorgan Chase & Co.)
# ============================================================

# Download JPM data from Yahoo Finance
getSymbols("JPM", src = "yahoo",
           from = start_date, to = end_date)

# Inspect JPM
class(JPM)
head(JPM)
tail(JPM)
dim(JPM)

# Charts
plot(JPM, multi.panel = TRUE, yaxis.same = FALSE)
chartSeries(JPM)
chartSeries(JPM["2025-12/"])

# Convert to data frame
jpm_prices <- data.frame(
  Date = index(JPM),
  Open = as.numeric(Op(JPM)),
  High = as.numeric(Hi(JPM)),
  Low = as.numeric(Lo(JPM)),
  Close = as.numeric(Cl(JPM)),
  Volume = as.numeric(Vo(JPM)),
  Adjusted = as.numeric(Ad(JPM))
)
head(jpm_prices)
tail(jpm_prices)

# Export to CSV
write.csv(jpm_prices, "jpm_prices.csv", row.names = FALSE)
