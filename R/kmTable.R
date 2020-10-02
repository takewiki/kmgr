#' 判断表格是否排序
#'
#' @param sortable 表格是否可以排序
#'
#' @return  返回值
#' @export
#'
#' @examples
#' wikiTableSort();
wikiTableSort <- function(sortable=TRUE) {
  if (sortable ==  TRUE){
    res <- " sortable"
    }else{
    res<-""
    }
  return(res);

}


#' 将数据框转化为wikiTable可以识别的格式
#'
#' @param df 数据框
#' @param sortable  是否排序
#' @param file 输出文件目录
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' write.wikiTable();
write.wikiTable <- function(df=iris,sortable=TRUE,file='./wikiTable.txt') {
  #获取类别
  suffix <- wikiTableSort(sortable);
  wiki_class <- paste("wikitable",suffix,sep="");
  class_setting <- paste('{| class="',wiki_class,'"',sep="");
  row_sep <-"|-";

  #获取标题
  header <- names(df);
  #将所有列转换为文件类型，还是数据框
  df <- df_as_character(df);
  h1 <- paste(header,collapse  =" !! ");
  h1_setting <-paste("  ! ",h1,sep="");
  #设置表体；
  ncount <- nrow(df);
  wiki_body <- character(ncount*2);
  for ( i in 1:ncount){
    wiki_body[i*2-1] <-row_sep;
    b1 <- paste(df[i,],collapse  =" || ");
    b1_setting <-paste("  | ",b1,sep="");
    wiki_body[i*2] <-b1_setting;
  }
  wiki_tail <-"|}"
  write.txt(class_setting,file);
  write.txt(row_sep,file);
  write.txt(h1_setting,file);
  write.txt(wiki_body,file);
  write.txt(wiki_tail,file)
}


