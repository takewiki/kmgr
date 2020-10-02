# library(kmgr)

nameSpace_demoBatch(file="./data-raw/res.txt");


data <- nameSpace_dataTemplate();
data;
library(openxlsx);
write.xlsx(data,'./data-raw/data_test.xlsx');
