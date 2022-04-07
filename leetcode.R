#Leetcode Q3

lengthOfLongestSubstring <- function(str){
    res <- 0
    max_res <- res
    memo <- list()
    for(i in 1:nchar(str)){
        char <- substr(str, i, i)
        if(char %in% names(memo)){
            res <- 1
            memo <- list()
            memo[char] = 1
        }
        else{
            memo[char] = 1
            res <- res + 1
            if(res > max_res){
                max_res <- res
            }
        }
    }
    max_res
}

examples <- c('abcabcbb', 'bbbbb', 'pwwkew', 'asdkflkasf' ,'ldkfhgsuheg', 'adshgfadkhjfg')

for(test in examples){
    print(lengthOfLongestSubstring(test))
}