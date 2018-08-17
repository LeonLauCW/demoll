#Python爬取数据村尾JSON-------------------------------------------------------
# import requests
# import re
# import json
# import time
# 
# def parsejd(page, url, start):
#   
#   header = {
#     "Connection": "keep-alive",
#     "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36",
#     "Accept": "*/*",
#     "Accept-Encoding": "gzip,deflate,br",
#     "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8"
#   }
# 
# rp = requests.get(url, timeout=3, headers = header)
# rp.raise_for_status()
# rp.encoding = rp.apparent_encoding
# 
# resultp = re.search(r'{.*}', rp.text).group(0)
# 
# print(start+page,':',len(resultp))
# while len(resultp) <10000:
#   time.sleep(3)
# print(page,' :again')
# rp = requests.get(url, timeout=3, headers = header)
# resultp = re.search(r'{.*}', rp.text).group(0)
# print(start+page, ':', len(resultp))
# 
# pathp = 'data' + str(start+page) + '.json'
# filep = open(pathp, 'w', encoding = 'utf-8')
# json.dump(resultp, filep, ensure_ascii=False)
# filep.close()
# 
# def main():
#   
#   for i in range(1, 401):
#   url = 'https://sclub.jd.com/comment/productPageComments.action?callback=fetchJSON_comment98vv19453&productId=27097846678&score=0&sortType=5&page=' \
# +str(i)+ '&pageSize=10&isShadowSku=0&rid=0&fold=1'
# parsejd(i, url ,0)
# 
# for i in range(1, 400):
#   url = 'https://sclub.jd.com/comment/productPageComments.action?callback=fetchJSON_comment98vv19453&productId=16580070299&score=0&sortType=5&page=' \
# + str(i) + '&pageSize=10&isShadowSku=0&rid=0&fold=1'
# parsejd(i, url, 400)
# 
# for i in range(1, 401):
#   url = 'https://sclub.jd.com/comment/productPageComments.action?callback=fetchJSON_comment98vv19453&productId=27097293658&score=0&sortType=5&page=' \
# + str(i) + '&pageSize=10&isShadowSku=0&rid=0&fold=1'
# parsejd(i, url, 800)
# 
# main()
#-----------------------------------------------------------------------------
library(jsonlite)
getwd()
setwd('/Users/liuzhuohong/Desktop/STUDY/大二夏季学期/数据挖掘/项目')
#将json数据导入---------------------------------------------------------------

path <- 'data/json_data/data0.json' #数据路径
json_data <- read_json(path) #读取第一个json文件的数据
json_data <- fromJSON(json_data) #转为json格式
comment_data <- data.frame(referenceName = json_data$comments$referenceName, #将需要的数据放入数据框
                           creationTime = json_data$comments$creationTime,
                           content = json_data$comments$content,
                           score = json_data$comments$score,
                           stringsAsFactors = FALSE)

for(i in 1:1200){ #循环导入剩下的数据
  
  path <- 'data'
  path <- paste0(path, '/json_data/data', i, '.json')
  json_data <- read_json(path) #读取json数据
  json_data <- fromJSON(json_data) #转为json格式
  new_comment_data <- data.frame(referenceName = json_data$comments$referenceName, #新json的数据
                                 creationTime = json_data$comments$creationTime,
                                 content = json_data$comments$content,
                                 score = json_data$comments$score,
                                 stringsAsFactors = FALSE)
  comment_data <- rbind(comment_data, new_comment_data) #合并
}

# write.csv(comment_data, 'data/raw_data.csv', row.names = F) #保存初始数据

#数据预处理-------------------------------------------------------------------------------------
data <- read.csv('data/raw_data.csv', fileEncoding = 'GBK', stringsAsFactors = FALSE)
data <- data[,3:4] #只取后面两列
data <- unique(data) #清理相同数据
data$content <- gsub('[a-zA-Z0-9]', '', data$content) #去除数字和英文
data$content <- gsub('[[:punct:]]', '', data$content) #去除符号

data$content <- gsub('[京东]', '', data$content)
data$content <- gsub('[手机]', '', data$content)
data$content <- gsub('[乔布斯]', '', data$content)
data$content <- gsub('[苹果]', '', data$content)
data$content <- gsub('[买]', '', data$content)
data$content <- gsub('[说]', '', data$content)
data$content <- gsub('[自营]', '', data$content)

#分词-----------------------------------------------------------------------------
library(jiebaR)
library(jiebaRD)
cutter <- worker(type = 'tag', stop_word = 'data/stoplist.txt') #构造分词器
segmented_word <- list() #分词结果放入segmented_word
for(i in 1:nrow(data)){ #segmented_word是分词结果
  segmented_word[[i]] <- segment(data$content[i], cutter)
}

word_number <- sapply(segmented_word, length) #对segmented_word的每一个元素求长度返回到word_number向量中
id <- rep(1:length(segmented_word), word_number) #句子id号
score <- rep(data$score, word_number) #评价等级
word_type <- unlist(sapply(segmented_word, names)) #词性
clean_data <- data.frame(id = id, word = unlist(segmented_word), nature = word_type, score = score)

word_number <- sapply(split(clean_data,clean_data$id), nrow)
index <- sapply(word_number, seq_len)
index <- unlist(index)
clean_data$index <- index #在该句的位置
#-------------------------------------------------------------------------------


for i in 1:neow(data){
	print(x)
}
