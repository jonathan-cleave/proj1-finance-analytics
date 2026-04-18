options(scipen=999)
library(readxl)
library(quantmod)
library(xts)
setwd("C:/Users/jonat/Documents/Calpoly MSBA/Spring/Finance Analytics/Scripts")

##########################################
# 10-Year Treasury vs 2-Year
##########################################
getSymbols(c("DGS10","DGS2"), src = "FRED")

merged_data <- na.omit(merge(DGS10, DGS2))
colnames(merged_data) <- c("DGS10", "DGS2")

plot_data <- merged_data["2025-12-01/2026-04-13"]

x <- index(plot_data)
y1 <- as.numeric(plot_data$DGS10)
y2 <- as.numeric(plot_data$DGS2)

par(mar = c(6, 4, 4, 2))

plot(x, y1, type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "Yield in %",
     ylim = range(c(y1, y2), na.rm = TRUE),
     main = "10-year vs 2-year yields Around Iran Conflict",
     xaxt = "n")

lines(x, y2, col = "blue", lwd = 2)

mid_month <- seq(from = as.Date(format(min(x), "%Y-%m-15")),
                 to   = as.Date(format(max(x), "%Y-%m-15")),
                 by   = "1 month")

month_starts <- seq(from = as.Date(format(min(x), "%Y-%m-01")),
                    to   = as.Date(format(max(x), "%Y-%m-01")),
                    by   = "1 month")

# minor ticks
axis.Date(1, at = x, labels = FALSE, tcl = -0.2)

# major ticks
axis.Date(1, at = month_starts, labels = FALSE, tcl = -0.6)
axis.Date(1, at = mid_month,
          labels = rep("15th", length(mid_month)),
          tcl = -0.6, cex.axis = 0.8)

# month labels
text(x = month_starts,
     y = par("usr")[3] - 0.08,
     labels = format(month_starts, "%b %d"),
     xpd = TRUE,
     cex = 1.0)

legend("topleft",
       inset = 0.02,
       legend = c("10-Year Treasury Yield",
                  "2-Year Treasury Yield",
                  "War Start"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 2),
       bty = "n")

event_date <- x[which.min(abs(x - as.Date("2026-02-28")))]
abline(v = event_date, col = "red", lty = 2, lwd = 2)

points(x[which.min(y1)], min(y1), col="black", pch=19)
text(event_date, min(y1), "Flight to Safety", pos=4)

before <- DGS10["2026-02-14/2026-02-27"]
after <- DGS10["2026-02-28/2026-03-13"]
mean_before <- mean(before, na.rm = TRUE)
mean_after <- mean(after, na.rm = TRUE)
mean_before
mean_after
mean_after - mean_before




#############################
#           XLF             #
#############################
getSymbols(c("XLF","KRE"), src = "yahoo")

merged_data <- na.omit(merge(Cl(XLF), Cl(KRE)))
colnames(merged_data) <- c("XLF", "KRE")

plot_data <- merged_data["2025-12-01/2026-04-14"]

x <- index(plot_data)
y1 <- as.numeric(plot_data$XLF)
y2 <- as.numeric(plot_data$KRE)

norm1 <- y1 / y1[1] * 100
norm2 <- y2 / y2[1] * 100

par(mar = c(7, 4, 4, 2))  # extra space for date labels

plot(x, norm1, type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "Indexed Value (100 = Start)",
     ylim = range(c(norm1, norm2), na.rm = TRUE),
     main = "XLF vs KRE Around Iran Conflict",
     xaxt = "n")

lines(x, norm2, col = "blue", lwd = 2)

mid_month <- seq(from = as.Date(format(min(x), "%Y-%m-15")),
                 to   = as.Date(format(max(x), "%Y-%m-15")),
                 by   = "1 month")


# minor ticks for each trading day
axis.Date(1, at = x, labels = FALSE, tcl = -0.2)

# major ticks at first of each month
month_starts <- seq(from = as.Date(format(min(x), "%Y-%m-01")),
                    to   = as.Date(format(max(x), "%Y-%m-01")),
                    by   = "1 month")

axis.Date(1, at = mid_month,
          labels = rep("15th", length(mid_month)),
          tcl = -0.6, cex=0.8)

axis.Date(1, at = month_starts, labels = FALSE, tcl = -0.6)

# manual labels so "Dec 01" definitely appears
text(x = month_starts,
     y = par("usr")[3] - 1.2,
     labels = format(month_starts, "%b %d"),
     xpd = TRUE, cex = 1.1)

legend("topright",
       inset = 0.02,
       legend = c("KRE (Regional Banks)",
                  "XLF (Large Banks)", 
                  "War Start",
                  "High Divergence"),
       col = c("blue", "black", "red", NA),
       lty = c(1, 1, 2, NA),
       lwd = c(2, 2, 2, NA),
       fill = c(NA, NA, NA, adjustcolor("orange", alpha.f = 0.75)),
       border = NA,
       bty = "n")

event_date <- x[which.min(abs(x - as.Date("2026-02-28")))]
abline(v = event_date, col = "red", lty = 2, lwd = 2)

gap <- abs(norm1 - norm2)

# Define "most separated" as top 25% of gaps
threshold <- quantile(gap, 0.75)

# Indices where gap is large
idx <- which(gap >= threshold)

# Split into continuous runs
runs <- split(idx, cumsum(c(1, diff(idx) != 1)))

# Get plot boundaries
usr <- par("usr")

# Add shaded vertical bands
for (r in runs) {
  rect(xleft   = x[min(r)],
       ybottom = usr[3],
       xright  = x[max(r)],
       ytop    = usr[4],
       col     = adjustcolor("orange", alpha.f = 0.15),
       border  = NA)
}

lines(x, norm1, col = "black", lwd = 2)
lines(x, norm2, col = "blue", lwd = 2)



####################################
#               VIX                #
####################################
getSymbols(c("^VIX", "JPM"), src = "yahoo")

merged_data <- na.omit(merge(Cl(VIX), Cl(JPM)))
colnames(merged_data) <- c("VIX", "JPM")

plot_data <- merged_data["2025-12-01/2026-04-13"]

x <- index(plot_data)
vix <- as.numeric(plot_data$VIX)
jpm <- as.numeric(plot_data$JPM)

# Normalize both to 100 at the start
vix_norm <- vix / vix[1] * 100
jpm_norm <- jpm / jpm[1] * 100

plot(x, vix_norm,
     type = "l",
     col = "purple",
     lwd = 2,
     xlab = "Date",
     ylab = "Indexed Value (100 = Start)",
     ylim = range(c(vix_norm, jpm_norm), na.rm = TRUE),
     main = "VIX vs JPM Around Iran Conflict", xaxt = "n")

lines(x, jpm_norm, col = "black", lwd = 2)

event_date <- x[which.min(abs(x - as.Date("2026-02-28")))]
abline(v = event_date, col = "red", lty = 2, lwd = 2)

legend("topleft",
       legend = c("VIX (Fear)", "JPM", "War Start"),
       col = c("purple", "black", "red"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 2),
       bty = "n")

mid_month <- seq(from = as.Date(format(min(x), "%Y-%m-15")),
                 to   = as.Date(format(max(x), "%Y-%m-15")),
                 by   = "1 month")


# minor ticks for each trading day
axis.Date(1, at = x, labels = FALSE, tcl = -0.2)

# major ticks at first of each month
month_starts <- seq(from = as.Date(format(min(x), "%Y-%m-01")),
                    to   = as.Date(format(max(x), "%Y-%m-01")),
                    by   = "1 month")

axis.Date(1, at = mid_month,
          labels = rep("15th", length(mid_month)),
          tcl = -0.6, cex=0.8)

axis.Date(1, at = month_starts, labels = FALSE, tcl = -0.6)

# manual labels so "Dec 01" definitely appears
text(x = month_starts,
     y = par("usr")[3] - 1.2,
     labels = format(month_starts, "%b %d"),
     xpd = TRUE, cex = 1.1)


####################################
#          Deposits                #
####################################

getSymbols("DPSACBW027SBOG", src = "FRED")

plot_data <- DPSACBW027SBOG["2025-12-01/2026-04-13"]

x <- index(plot_data)
y <- diff(as.numeric(plot_data))   # FIXED: compute change
x <- x[-1]

plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "Date",
     ylab = "Weekly Change in Deposits (Billions USD)",
     main = "Weekly Change in Bank Deposits Around Iran Conflict",
     ylim = c(min(y), max(y) * 1.2))

# Zero line
abline(h = 0, col = "black", lty = 2)

# Event line
event_date <- as.Date("2026-02-28")
abline(v = event_date, col = "red", lty = 2, lwd = 2)

# Labels
text(max(x), max(y)*1.1, "Deposits (Inflows)", pos = 2, col = "darkgreen")
text(max(x), min(y)*0.9, "Withdrawals (Outflows)", pos = 2, col = "red")

legend("topleft",
       legend = c("Deposit Change", "Zero Line", "War Start"),
       col = c("blue", "black", "red"),
       lty = c(1, 2, 2),
       lwd = c(2, 1, 2),
       bty = "n")

neg_idx <- which(y < 0)

points(x[neg_idx], y[neg_idx], col = "red", pch = 16)