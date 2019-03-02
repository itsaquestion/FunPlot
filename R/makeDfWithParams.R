#' makeDfWithParams
#'
#' 根据参数，为ggplot生成某个函数的绘图用的df = data.frame(x, y, params:str)
#'
#' @param fun 要绘制的函数
#' @param params 函数参数df，类似params = data.frame(alpha = 1:10, beta = 1:10)
#' @param x_points x轴上的取样点
#' @import tibble
#' @import MyUtils
#' @return 绘图用的df = data.frame(x, y, params:str)
#' @export
#'
#' @examples
makeDfWithParams = function(fun, params, x_points) {
  params = tibble::as_tibble(params)
  df = MyUtils::rowMap(params, function(pp) {
    y_points = do.call(fun, c(list(x = x_points), as.list(pp)))

    p_names = names(params)

    params_str = ""
    if (ncol(pp) > 1) {
      for (i in 1:ncol(pp)) {
        part = paste(p_names[i], pp[i], sep = '=')
        if (i == 1) {
          params_str = part

        } else {
          params_str = paste(params_str, part, sep = ',')
        }
      }
    } else {
      params_str = paste(pp)
    }

    #params_str = glue('${params_str}$')
    data.frame(x = x_points, y = y_points, params = params_str)
  })
  df
}
