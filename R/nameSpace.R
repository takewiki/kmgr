#' 优化cat,用于输入到文档
#'
#' @param x R对象
#' @param file 目前文件
#'
#' @return  直接输出到文件，没有返回值
#' @export
#'
#' @examples
#' ns_cat('hello world',"./helloworld.txt");
ns_cat <- function(x,file)
{
  cat(x,file=file,append = T,sep="\n");
}
# 为mediawiki添加命名空间并进行权限管理----
#' 为mediawiki添加命名空间并进行权限管理
#'
#' @param NS_Name 名称空间,英文名
#' @param NS_Title 名称空间，中文名
#' @param NS_Id   名称空间内码，必须为偶数
#' @param file    输出配置文件到文档
#'
#' @return 无返回值，直接输出到file指定文档
#' @export
#'
#' @examples
#' nameSpace_add();
nameSpace_add <- function(
                     NS_Name='RD',
                     NS_Title='棱星',
                     NS_Id=100,
                     file="./res.txt"
                     ){
  title_Id <- NS_Id +1;
  tip <-paste('#create namespace for ',NS_Name,' with id start from ',NS_Id ,' end with ',title_Id,sep='');
  ns_cat(tip,file=file);
  author_info <-paste("#created by hawken.hu on ",as.character(Sys.Date()),sep="");
  ns_cat(author_info,file=file);
  def_ns <-paste("define('NS_",NS_Name,"',",NS_Id,");",sep="");
  ns_cat(def_ns,file=file);
  def_ns_talk <-paste("define('NS_",NS_Name,"_TALK',",title_Id,");",sep="");
  ns_cat(def_ns_talk,file=file);
  NS_Title_cat <-paste("$wgExtraNamespaces[NS_",NS_Name,"] = '",NS_Title,"';",sep="");
  ns_cat(NS_Title_cat,file=file);
  NS_talk_cat <-paste("$wgExtraNamespaces[NS_",NS_Name,"_TALK] = '",NS_Title,"讨论';",sep="");
  ns_cat(NS_talk_cat,file=file);
  opt1 <-paste("$wgGroupPermissions['",NS_Title,"超管']['read']=true;",sep="");
  ns_cat(opt1,file = file);
  opt2 <-paste("$wgGroupPermissions['",NS_Title,"超管']['edit']=true;",sep="");
  ns_cat(opt2,file = file);
  opt3 <-paste("$wgGroupPermissions['",NS_Title,"超管']['delete']=true;",sep="");
  ns_cat(opt3,file = file);
  opt4 <-paste("$wgGroupPermissions['",NS_Title,"管理']['read']=true;",sep="");
  ns_cat(opt4,file = file);
  opt5 <-paste("$wgGroupPermissions['",NS_Title,"管理']['delete']=true;",sep="");
  ns_cat(opt5,file = file);
  opt6 <-paste("$wgGroupPermissions['",NS_Title,"制单']['read']=true;",sep="");
  ns_cat(opt6,file = file);
  opt7 <-paste("$wgGroupPermissions['",NS_Title,"制单']['edit']=true;",sep="");
  ns_cat(opt7,file = file);
  opt8 <-paste("$wgGroupPermissions['",NS_Title,"订阅']['read']=true;",sep="");
  ns_cat(opt8,file = file);
  q1 <- paste("$wgNamespaceProtection[NS_",NS_Name,"] = array('queryNamespace",NS_Name,"');",sep="");
  ns_cat(q1,file=file);
  q2 <- paste("$wgGroupPermissions['",NS_Title,"制单']['queryNamespace",NS_Name,"']=true;",sep="");
  ns_cat(q2,file=file);
  q3 <- paste("$wgGroupPermissions['sysop']['queryNamespace",NS_Name,"']=true;",sep="");
  ns_cat(q3,file=file);
  ldopt1 <- paste("$wgNamespacePermissionLockdown[NS_",NS_Name,"]['edit'] = array('",
                  NS_Title,"制单','",NS_Title,"管理','",NS_Title,"超管','sysop');",sep="");
  ns_cat(ldopt1,file=file);
  ldopt2 <- paste("$wgNamespacePermissionLockdown[NS_",NS_Name,"]['read'] = array('",
                  NS_Title,"订阅','",NS_Title,"制单','",NS_Title,"管理','",
                  NS_Title,"超管','sysop');",sep="");
  ns_cat(ldopt2,file=file);
  sub1 <-paste("$wgNamespacesWithSubpages[NS_",NS_Name,"] = true;",sep="");
  ns_cat(sub1,file=file);
  sub2 <-paste("$wgNamespacesWithSubpages[NS_",NS_Name,"_TALK] = true;",sep="");
  ns_cat(sub2,file=file);


}










