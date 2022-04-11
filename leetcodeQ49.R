groupAnagrams <- function(strs){
    dic <- list()
    for(char in strs){
        temp_char <- sort_str(char)

        if(!(temp_char %in% names(dic))){
            dic[temp_char] <- list(char)
        }
        else{
            dic[[temp_char]] <- append(dic[[temp_char]], char)
        }
    }
    res = list()
    for(n in names(dic)){
        res[[length(res) + 1]] <- dic[[n]]
    }
    res
}
sort_str <- function(str){
    str <- paste(sort(strsplit(str,"")[[1]]), collapse = "")
}

test <- c("eat","tea","tan","ate","nat","bat")
print(groupAnagrams(test))