library(quantmod)

TDXStock <- function (file) {
  # 从通达信导出的 txt 文件中读取数据
  #
  # 需对直接导出的数据做一点修改，删除成交量之后的列数据
  as.xts(read.zoo(file, sep = "", format = "%Y/%m/%d",
           header = FALSE, index.column = 1,
           skip = 3, fileEncoding = "GBK",
           col.names = c("Date", "Open", "High", "Low", "Close", "Volume"),
           colClasses = c("character", rep("numeric",5))))
}

AbsMomentum <- function(data, lag, r = 0) {
  # 对给定 OHCL 数据，回测绝对动量策略，返回添加交易记录和净值的 OHCL 数据
  #
  # 传入的 data 可以是日线、周线、月线年线数据
  # r 为每个周期空仓的收益
  #
  # 策略：比较每个交易点的价格与回退 lag 个交易点的价格
  # - 若当前价格更高或相等，全部买入或继续持有
  # - 若当前价格更低，则全部卖出

  close <- Cl(data)
  # hold 表示每个交易点持仓情况情况，1 表示全仓，0 表示空仓
  hold <- ifelse(close > Lag(close, lag), 1, 0)
  hold[1:lag] <- 0 # 初始空仓

  # 计算净值：
  # ROC 计算「连续」变化时返回的每个元素是 log(今天收盘价/昨天收盘价)
  # 用自然对数 log 是为了后面计算净值更加方便
  # 乘以 hold 得到的是持仓时的净值变化，注意需要对 hold Lag 1 个位置，
  # 否则相当于能够预测涨跌，将涨买入、将跌卖出，收益会非常惊人
  roc <- ROC(close) * Lag(hold)
  # 添加空仓收益
  roc <- roc + ifelse(hold == 0, log(1+r), 0)
  # 利用 ab = exp(log(a) + log(b)) = exp(log(ab))，用初始净值 V_1 = 1
  # 净值 = V_n/V_1
  #      = V_n/V_{n-1} * V_{n-1}/V_{n-2} ... V_2/V_1
  #      = exp(log(V_n/V_{n-1} * ... * V_2/V_1 ))
  #      = exp(log(V_n/V_{n-1}) + log(V_{n-1}/V_{n-2} + ...))
  roc[1] <- 0 # 设定初始净值为 1
  netVal <- exp(cumsum(roc))

  # 打印每年收益率
  print(paste("Lag", lag))
  yearRetStrategy <- apply.yearly(netVal, function(v) {
    round(ROC(c(first(v), last(v)), type = "discrete")[2] * 100, 2)
  })
  yearRetHold <- apply.yearly(data, function(v) {
    round(ROC(c(Op(first(v)), Cl(last(v))), type = "discrete")[2] * 100, 4)
  })
  yearRet <- cbind(yearRetHold, yearRetStrategy)
  names(yearRet) <- c("buyHold", "absMomemtum")
  print(yearRet)

  # 交易点：0 表示不操作，1 表示买入，-1 表示卖出
  trade <- hold - Lag(hold)
  trade[1] <- 0

  tradeRecord <- cbind(close, hold, trade, netVal)
  names(tradeRecord) <- c("Close", "hold", "trade", "netValue")
  tradeRecord
}

PlotStock <- function(data, title, ylab.left = "价格") {
  # 画收盘价
  par(mar = c(5,4,4,4) + 0.1, family = "AdobeHeitiStd-Regular")
  plot(Cl(data), main = title, ylab = ylab.left, type = "l")
}

PlotTrade <- function(trades, color = NULL) {
  # 不清空 plot，画净值以及交易点

  n <- (ncol(trades) - 1) / 3
  if (is.null(color)) {
    color = 1:n + 3
  }

  # 画收益
  netVal <- trades[, 1 + 3*(1:n)]
  par(new = T)
  matplot(netVal, main = "", xlab = "", ylab = "",
          axes = F, type = "l", lty = 1, col = color)

  # 右侧添加 Y 轴
  axis(side = 4)
  mtext("净值", side = 4, line = 3)

  # 画出交易点
  buy <- trades$Close
  sell <- trades$Close
  for (i in 1:n) {
    tradePoint <- trades[, 3*i]
    netVal <- trades[, 1 + 3*i]
    buy <- cbind(buy, netVal[tradePoint == 1])
    sell <- cbind(sell, netVal[tradePoint == -1])
  }
  matpoints(buy[, 1:n+1], col = "red", cex = 0.6, pch = 17)
  matpoints(sell[, 1:n+1], col = "green", cex = 0.6, pch = 16)
}

ShowStrategy <- function(name, data, lags, r, ylab.left = "价格") {
  # 对指定的 lags 回测策略并画图
  PlotStock(data, name)

  cols <- 1:length(lags) + 3
  trades <- NULL
  for (i in 1:length(lags)) {
    td <- AbsMomentum(data, lags[i], r)
    if (is.null(trades)) {
      trades <- td
    } else {
      trades <- cbind(trades, td$hold, td$trade, td$netValue)
    }
  }

  PlotTrade(trades, cols)

  # xtsExtra 在 OS X 上从源码编译安装过于麻烦而未采用，故自己添加 legend
  legend(x = "topleft", bty = "n", cex = 0.7 * par("cex"),
         legend = c(name, paste("lag", lags)),
         lty = rep(1, 1 + length(lags)),
         col = c("black", cols))
}
