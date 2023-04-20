# 下面的代码是把squeeze的injection_info.csv转成DejaVu所需的faults.csv格式
# injection_info.csv to faults.csv

file_root="D:/PSqueeze-master/A/new_dataset_A_week_12_n_elements_3_layers_3/"

data=read.csv(paste0(file_root,"injection_info.csv"))

#target=c("a", "b", "c", "d")

target=c("i", "e", "c", "p","l")

# 把i=i00&e=e01&c=c02转成i00&e01&c02
for(i in 1:nrow(data)){
  result=NULL
  collect = FALSE
  
  options(scipen = 200)  #禁掉科学计数法
  
  for(j in 1:nchar(data$set[i])){
    check=substr(data$set[i],j,j)
    if(collect == TRUE){
      result=paste0(result,check)
    }
    if(check == '='){
      collect = TRUE
    }
    if(check == '&' || check == ';'){
      collect = FALSE
    }
  }
  data$set[i]=result
}

names(data)=c('root_cause_node','timestamp')

write.csv(data,"D:/NMS/faults.csv",row.names = FALSE)

# # injection_info.csv to faults.csv finish

###下面的代码是取score>0.2的叶节点e建树并计算每个节点分数，
###分数计算过程，例，计算e00&c01的score：
###1.遍历每一行，查看节点name是否包含e00&c01，
###2.分别累加所有包含e00&c01的叶节点的真实值和预测值,作为e00&c01的真实值和预测值，之后计算score
###3.注意代码里算score的时候还会额外乘根因组合的元素数量，例如e00&c01的元素数量是2，e00&c01&p02的元素数量是3
###4.还会额外乘根因的出现次数
###5.如果你不希望额外乘元素数量和出现次数，设置simple_score=TRUE
### ------------------------------------------------------------------------------
#install.packages("readr")
### ------------------------------------------------------------------------------

library("readr")

#为TRUE，则最后所有score>0.2的节点以及对应的分数、时间戳都会被写到一个CSV文件里
#注意代码里写文件的模式是“w+”，而不是"w"
file_root="D:/PSqueeze-master/A/new_dataset_A_week_12_n_elements_3_layers_3/"
save_result=TRUE
score_th=0.2
save_path="D:/NMS/metrics.csv"

# 如果只是在debug代码，默认只跑3个时间戳的csv，如果要跑所有400多个CSV，设置debug=FALSE
debug=TRUE
tail_index=3

# 如果你只是希望得到csv文件，而不关心此方法输出的根因以及对应的recall、precision、F1,设置csv_only=TRUE
csv_only=FALSE

# 如果你不希望额外乘元素数量和出现次数，设置simple_score=TRUE
simple_score=FALSE

# 取score最高的前25个搜索根因
num_sample=25
# 最后输出多少结果
num_output=3

# score差距在1%以内，如果追求简洁性，succinctness=TRUE，用e00&c01&p02代替e00&c01，或者追求查全，succinctness=FALSE，用e00&c01代替e00&c01&p02
correction=0.1
succinctness=FALSE

seed_kind = 0
if(seed_kind==0){
  seed=c('i','e','c','p','l') 
}
if(seed_kind==1){
  seed=c('a','b','c','d')  
}
resultData=read.csv("D:/NMS/faults.csv")
#resultData=read.csv(paste0(file_root,"faults.csv"))
getDataFile=NULL

TP=0
FP=0
FN=0

if(debug==FALSE){
  tail_index=nrow(resultData)
}

for(i in 1:tail_index){
  print(paste0("iter--- ",as.character(i),"/",as.character(tail_index)," ---start---",as.character(resultData$timestamp[i])))
  
  options(scipen = 200)  #禁掉科学计数法
  
  filename=as.character(resultData$timestamp[i]) #数字转字符串
  
  
  # paste0 函数拼接字符串,不要手滑写成paste，少一个0会调用错误的函数导致拼接出的文件路径多了空格,file not found
  getDataFile=read.csv(paste0(file_root,filename,".csv"))
  
  # 会把getDataFile$real+getDataFile$predict==0，也就是导致计算score时分母为0的行删掉
  getDataFile=getDataFile[-which(getDataFile$real+getDataFile$predict==0),]
  
  # 拼接字符串形成node_name
  for(j in 1:nrow(getDataFile)){
    
    if(seed_kind==0){
      name = paste0(getDataFile$i[j],'&',getDataFile$e[j],'&',getDataFile$c[j],'&',getDataFile$p[j],'&',getDataFile$l[j]) 
    }
    if(seed_kind==1){
      name = paste0(getDataFile$a[j],'&',getDataFile$b[j],'&',getDataFile$c[j],'&',getDataFile$d[j]) 
    }
    
    getDataFile$timestamp[j]=resultData$timestamp[i]
    #getDataFile$metrics_type[j]="score"
    getDataFile$name[j]=name
  }
  
  # 计算score时公式的分子，注意这里的diff也是处理score=1，也就是real=0但只要predict非0时score恒等于1，处理这种情况时只能看差距的绝对值了
  getDataFile$diff=abs(getDataFile$real-getDataFile$predict)
  
  getDataFile$score=(getDataFile$diff/(getDataFile$real+getDataFile$predict))
  
  # 要把name里带"unknow"字样的行删掉
  for (j in 1:nrow(getDataFile)) {
    if("unknow" %in% getDataFile$name[j]){
      getDataFile$score[j]=0
    }
  }
  
  # 把分数小于0.2，也就是score_th的行删掉
  getDataFile=getDataFile[-which(getDataFile$score<score_th),]
  getDataFile=getDataFile[-which(getDataFile$diff<2),]
  # 把score作为一级排序指标,diff作为二级排序指标，降序排列
  getDataFile=getDataFile[order(getDataFile$score,getDataFile$diff,decreasing = TRUE),]
  
  # score=1的情况都需要看一下，此外再保留min(num_sample, nrow(getDataFile))个
  count=0
  for(j in 1:nrow(getDataFile)){
    if(count<=num_sample){
      if(getDataFile$score[j]<1){
        count=count+1
      }
    }
    else{
      count=j
      break
    }
  }
  
  node_names=c()
  num_seed = length(seed)
  for(j in 1:nrow(getDataFile)){
    if(seed_kind==0){
      candidates=c(getDataFile$i[j],getDataFile$e[j],getDataFile$c[j],getDataFile$p[j],getDataFile$l[j]) 
    }
    if(seed_kind==1){
      candidates=c(getDataFile$a[j],getDataFile$b[j],getDataFile$c[j],getDataFile$d[j]) 
    }
    for(round in num_seed:1){
      ### combn函数，输入一个数组，指定组合多少元素，返回组合结果
      ### 例如，candidates=[e00,c01,p02],指定组合元素数量round=2，返回一个2*3的数组，每列就是对应组合的下标
      combination=combn(candidates,round)
      for(k in 1:ncol(combination)){
        tmp_name=""
        for(m in 1:round){
          tmp_name=paste0(tmp_name,'&',combination[m,k])
        }
        last_index=nchar(tmp_name)
        tmp_name=substr(tmp_name,2,last_index)
        if(!(tmp_name %in% node_names)){
          node_names=append(node_names,tmp_name) 
        }
      }
    }
  } 
  num_node=length(node_names)
  node_scores=rep(0,length(node_names))
  
  ### node_name中蕴含了所有除叶节点之外的所有排列组合的节点名字
  ## 为每个node_name计算score
  for(j in 1:num_node){
    current_node_name=node_names[j]
    check_list=strsplit(current_node_name,'&')[[1]]
    check_length=length(check_list)
    
    node_real=0
    node_predict=0
    
    count=0
    for(k in 1:nrow(getDataFile)){
      check_result=rep(FALSE,check_length)
      # 检查e00&c01是不是当前遍历到的这行叶节点名字i00&e01&c02&p03&l4的子集
      for(m in 1:check_length){
        current_char=check_list[m]
        check_result[m]=(current_char %in% getDataFile[k,])
      }
      # 如果是，累加
      if(!(FALSE %in% check_result)){
        node_real=node_real+getDataFile$real[k]
        node_predict=node_predict+getDataFile$predict[k]
        count=count+1
      }
    }
    if(simple_score==TRUE){
      node_scores[j]=(abs(node_real-node_predict)/(node_real+node_predict))
    }
    else{
      node_scores[j]=(abs(node_real-node_predict)/(node_real+node_predict))*count*check_length
    }
  }
  # 把timestamp,node_name,score之外的列都删掉
  if(seed_kind==0){
    getDataFile=getDataFile[,-c(1,2,3,4,5,6,7,10)]
  }
  if(seed_kind==1){
    getDataFile=getDataFile[,-c(1,2,3,4,5,6,9)] 
  }
  last=nrow(getDataFile)
  for (j in 1:num_node) {
    getDataFile[last+j,1]=getDataFile$timestamp[1]
    #getDataFile[last+j,2]="score"
    getDataFile[last+j,2]=node_names[j]
    getDataFile[last+j,3]=node_scores[j]
  }
  #得到所有节点的score之后降序排列
  getDataFile=getDataFile[order(getDataFile$score,decreasing = TRUE),]
  
  # 筛选输出结果并计算recall、precision、F1
  # 注意此后getDataFile不会再改变了，如果你不关心此方法的输出，设置csv_only=TRUE
  
  if(csv_only==FALSE){
    failed_list=c()
    passed_students=c(getDataFile$name[1])
    passed_list=c(1)
    
    for (j in 2:nrow(getDataFile)) {
      current_student = strsplit(getDataFile$name[j],'&')[[1]]
      
      num_passed_students=length(passed_students)
      mark_list = rep(FALSE,num_passed_students)
      
      for (k in 1:num_passed_students) {
        current_teacher=strsplit(passed_students[k],'&')[[1]]
        
        baseline=min(length(current_student),length(current_teacher))
        if(baseline%%2==0){
          baseline=baseline/2
        }
        else{
          baseline=(baseline+1)/2
        }
        
        plagiaristic=length(intersect(current_teacher,current_student))
        
        if(plagiaristic<baseline){
          mark_list[k]=TRUE
        }
        else{
          if(abs(getDataFile$score[passed_list[k]]-getDataFile$score[j])/getDataFile$score[passed_list[k]]<correction){
            if(succinctness==TRUE){
              if(length(current_student)<length(current_teacher)){
                passed_list[k]=j
                passed_students[k]=getDataFile$name[j]
              }
            }
            else{
              if(length(current_student)>length(current_teacher)){
                passed_list[k]=j
                passed_students[k]=getDataFile$name[j]
              }
            }
          }
        }
      }
      
      if(!(FALSE %in% mark_list)){
        passed_students=append(passed_students,getDataFile$name[j])
        passed_list=append(passed_list,j)
      }
      else{
        failed_list=append(failed_list,j)
      }
    }
    num_result=min(length(passed_students),num_output)
    result_output=passed_students[1:num_result]
    
    GT_output=strsplit(resultData$root_cause_node[i],';')[[1]]
    
    current_TP=length(intersect(result_output,GT_output))
    current_FP=num_result-current_TP
    current_FN=length(GT_output)-current_TP
    
    current_recall=current_TP/(current_TP+current_FN)
    current_precision=current_TP/(current_TP+current_FP)
    current_F1=(2*current_TP)/(2*current_TP+current_FN+current_FP)
    
    TP=TP+current_TP
    FP=FP+current_FP
    FN=FN+current_FN
    
    print(result_output)
    print("current_recall---")
    print(current_recall)
    print("current_precision---")
    print(current_precision)
    print("current_F1---")
    print(current_F1)
  }
  
  if(save_result==TRUE){
    write_csv(getDataFile,save_path,append = TRUE) 
  }
}

if(csv_only==FALSE){
  total_recall=TP/(TP+FN)
  total_precision=TP/(TP+FP)
  total_F1=(2*TP)/(2*TP+FP+FN)
  
  print("total_TP---")
  print(TP)
  print("total_FP---")
  print(FP)
  print("total_FN---")
  print(FN)
  
  print("total_recall---")
  print(total_recall)
  print("total_precision---")
  print(total_precision)
  print("total_F1---")
  print(total_F1)  
}
