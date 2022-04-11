groupAnagrams <- function(strs){
    dic <- list()
    for(char in strs){
        temp_char <- ""
        for(c in sort_str(char)){
            temp_char <- paste(c(temp_char, c), collapse = "")
        }

        if(!(temp_char %in% names(dic))){
            dic[temp_char] <- list(char)
        }
        else{
            dic[temp_char] <- append(dic[temp_char], char)
        }
    }
    res = list()
    for(n in names(dic)){
        res[[length(res) + 1]] <- dic[n]
    }
    res
}

test <- c("eat","tea","tan","ate","nat","bat")
print(groupAnagrams(test))