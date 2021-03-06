% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nameSpace.R
\name{nameSpace_add}
\alias{nameSpace_add}
\title{为mediawiki添加命名空间并进行权限管理}
\usage{
nameSpace_add(NS_Name = "RD", NS_Title = "棱星", NS_Id = 100,
  file = "./res.txt")
}
\arguments{
\item{NS_Name}{名称空间,英文名}

\item{NS_Title}{名称空间，中文名}

\item{NS_Id}{名称空间内码，必须为偶数}

\item{file}{输出配置文件到文档}
}
\value{
无返回值，直接输出到file指定文档
}
\description{
为mediawiki添加命名空间并进行权限管理
}
\examples{
nameSpace_add();
}
