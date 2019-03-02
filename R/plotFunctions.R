#' plotFunctions
#'
#' 绘制带参数的函数（组）
#'
#' @param fun 要绘制的函数
#' @param params 函数的参数
#' @param n 采样点
#' @param x x的区间
#' @param size 线宽
#' @param end_spaces_ratio 最右侧留空百分比（占x轴的总长度）
#' @import ggplot2
#' @import directlabels
#'
#' @return ggplot2对象
#' @export
#'
#' @examples
plotFunctions = function (fun, params = NULL, n = 101, x = c(1, 5), size = 0.5,
                          end_spaces_ratio = 0) {

  # x_points 是函数在指定范围内的取值点。
  # 例如n = 101, x = c(1, 5)，就是在1 ~ 5内取 101个点。
  x_points = seq(x[1], x[2], length.out = n)


  # 生成的参数格式是 data.frame(x, y, params:str)
  if (is.null(params)) {
    df = data.frame(x = x_points, y = fun(x_points), params = "")
  }
  else {
    df = makeDfWithParams(fun, params, x_points)
  }

  ggplot(df, aes(x = x, y = y, group = params, color = params)) +
    geom_line(size = size) + xlim(c(x[1], x[2] + (x[2] -  x[1]) * end_spaces_ratio)) +
    geom_dl(aes(label = (params),  color = params), method = list(dl.trans(x = x + 0.1), "last.qp")) +
    theme_textbook()

}

