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
  # 对给定 OHCL 数据，回测绝对动量策略，返回添加交易记录和收益的 OHCL 数据
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

  # 计算收益：
  # ROC 计算「连续」变化时返回的每个元素是 log(今天收盘价/昨天收盘价)
  # 用自然对数 log 是为了后面计算总的收益率更加方便
  # 乘以 hold 得到的是持仓时的收益变化，注意需要对 hold Lag 1 个位置，
  # 否则相当于能够预测涨跌，将涨买入、将跌卖出，收益会非常惊人
  ret <- ROC(close) * Lag(hold)
  # 添加空仓收益
  ret <- ret + ifelse(hold == 0, log(1+r), 0)
  ret[1] <- 0
  # 总收益 = 最后持仓价/最初持仓价
  #        = close_n/close_{n-1} * close_{n-1}/close_{n-2} ... close_2/close_1
  # 利用 exp(log(a) + log(b)) = exp(log(ab)) = ab
  ret <- exp(cumsum(ret))

  # 交易点：0 表示不操作，1 表示买入，-1 表示卖出
  trade <- hold - Lag(hold)
  trade[1] <- 0

  tradeRecord <- cbind(close, hold, trade, ret)
  names(tradeRecord) <- c("Close", "hold", "trade", "return")
  tradeRecord
}

PlotStock <- function(data, title, ylab.left = "价格") {
  # 画收盘价
  par(mar = c(5,4,4,4) + 0.1, family = "AdobeHeitiStd-Regular")
  plot(Cl(data), main = title, ylab = ylab.left, type = "l")
}

PlotReturn <- function(trades, color = NULL) {
  # 不清空 plot，画收益以及交易点

  n <- (ncol(trades) - 1) / 3
  if (is.null(color)) {
    color = 1:n + 1
  }

  # 画收益
  ret <- trades[, 1 + 3*(1:n)]
  par(new = T)
  matplot(ret, main = "", xlab = "", ylab = "",
          axes = F, type = "l", lty = 1,
          col = color)

  # 右侧添加 Y 轴
  axis(side = 4)
  mtext("收益", side = 4, line = 3)

  # 画出交易点
  for (i in 1:n) {
    tradePoint <- trades[, 3*i]
    ret <- trades[, 1 + 3*i]
    # 红色标记买点
    points(ret[tradePoint == 1], col = "red", pch = 17)
    # 绿色标记卖点
    points(ret[tradePoint == -1], col = "green", pch = 17)
  }
}

ShowStrategy <- function(name, data, lags, r, ylab.left = "价格") {
  # 对指定的 lags 回测策略并画图
  PlotStock(data, name)

  cols <- 1:length(lags) + 2
  trades <- NULL
  for (i in 1:length(lags)) {
    td <- AbsMomentum(data, lags[i], r)
    if (is.null(trades)) {
      trades <- td
    } else {
      trades <- cbind(trades, td$hold, td$trade, td$return)
    }
  }

  PlotReturn(trades, NULL)

  # xtsExtra 在 OS X 上从源码编译安装过于麻烦而未采用，所以需自己添加 legend
  legend(x = "topleft", cex = 0.8 * par("cex"),
         legend = c(name, paste("lag", lags)),
         lty = rep(1, 1 + length(lags)),
         col = c("black", cols))
}
