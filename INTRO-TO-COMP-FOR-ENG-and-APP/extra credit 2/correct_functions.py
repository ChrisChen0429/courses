import math
import pandas as pd
def check_and_suggest_word(word):
    try:
        diction
    except:
        diction = pd.read_csv('dictionary.csv')
    suggestion = []
    def transpose(word):
        result = []
        for i in range(0,len(word)):
            wordlist = list(word)
            a = wordlist.pop(i)
            for j in range(0,len(wordlist) + 1):
                b = wordlist[:j] + list(a) + wordlist[j:]
                outcome = ''.join(b)
                result.append(outcome)
                finalresult = list(set(result))
        return finalresult
    def insertion(word):
        alphabet = [chr(i) for i in range(97,123)]
        result2 = []
        for i in alphabet:
            wordlist2 = list(word)
            for j in range(0,len(wordlist2) + 1):
                c = wordlist2[:j] + list(i) + wordlist2[j:]
                outcome2 = ''.join(c)
                result2.append(outcome2)
                finalresult2 = list(set(result2)) 
        return finalresult2
    def space(word):
         wordlist3 = list(word)
         suggestion2 = []
         for i in range(1,len(wordlist3)):
            d = wordlist3[:i] + list(' ') + wordlist3[i:]
            outcome3 = ''.join(d)
            pairsword = outcome3.split(sep = ' ')
            pair1,pair2 = pairsword[0],pairsword[1]             
            if pair1 in list(diction['Word']) and pair2 in list(diction['Word']):
               suggestion2 = suggestion2 + [pair1] + [pair2]
         return suggestion2
    def remove(word):
         result4 =[]
         for i in range(0,len(list(word))):
             wordlist4 = list(word)
             wordlist4.pop(i)
             outcome4 = ''.join(wordlist4)
             result4.append(outcome4)
         return result4
    def entrydetect(word):
        result5 = []
        for i in range(0,len(diction['User_Created'])):
            e = math.isnan(diction['User_Created'][i])
            result5.append(e)
        if sum(result5) < len(diction['User_Created']):
            return('there is an entry')
        if sum(result5) == len(diction['User_Created']):
            return('there is not any entry')
    if word in list(diction['Word']):
        return True
    if word not in list(diction['Word']):   
       for i in transpose(word) + insertion(word) + remove(word):
           if i in list(diction['Word']):
              suggestion.append(i)
    suggestion = suggestion + [space(word)]
    return suggestion,entrydetect(word)
    
    
    # first function for generate transposes
    def generate_transposes(word):
        result = []
        for i in range(0,len(word)-1):
            l = word[:i]
            c1 = word[i]
            c2 = word[i+1]
            r = word[i+2:]
            n = l + c2 + c1 + r
            result.append(n)
        return result
    
    # second function for generate insertion
    def generate_insertion(word):
        a_to_z = []
        for i in range(0, 26):
            temp = chr(ord('a')+i)
            a_to_z.append(temp)
        
        result = []
        
        for i in a_to_z:
            new1 = i+word
            new2 = word+i
            
            if (new1 in result) == False:
                result.append(new1)
            if (new2 in result) == False:
                result.append(new2)
                
            for j in range(0,len(word)-1):
                new3 = word[:j+1]+i+word[j+1:]
                if (new3 in result) == False:
                    result.append(new3)
        return result
    
    
    # thrid function for generate remove
    def generate_remove(word):
        result = list()
            
        result.append(word[0:len(word)-1])
        result.append(word[1:len(word)])
        for i in range(0,len(word)-1):
            new = word[0:i+1] + word[i+2:]
            if (new in result) == False:
                result.append(new)
        return result
    
    
    # pair of word function
    def generate_pair_of_word(word):
        length = len(word)
        result = list()
    
        for i in range(1,length):
            possible = (word[:i],word[i:])
            result.append(possible)
        return result

    # function to test whether the word spell correctly
    def correctly_spelled(w,word_list):
        correctly_spelled = False
    
        if w in word_list:
            correctly_spelled = True
        return correctly_spelled
    
    
    try:
        dictionary
    except:
        dictionary = dictionary_loader()
        
    ## Now, it's the time to use these functions
    
    w = w.lower()
    #check whether the data has already loaded
    
    # read the dictionary
    
    
    word_list = list(dictionary['Word'])
            
    whether_correct = correctly_spelled(w,word_list)
    
    # check whether the contents of the file are already loaded or not
    if whether_correct == True:
        return True
    if whether_correct == False:
        possible_word = list()
        possible_word.extend(generate_insertion(w))
        possible_word.extend(generate_remove(w))
        possible_word.extend(generate_transposes(w))

        # just in oder every word is unique
        possible_word = set(possible_word)
        
        suggestions = list()
            
        for i in possible_word:
            if correctly_spelled(i,word_list) == True:
                suggestions.append(i)
        
        # a little different for pair of word
        possible_pair = generate_pair_of_word(w)
        
        for item in possible_pair:
            if correctly_spelled(item[0],word_list) == True & correctly_spelled(item[1],word_list) == True:
                suggestions.append(item)
        
        if len(suggestions) == 0:
            print("no suggestion can give to you.")
            return suggestions
        else:
            print("here are possible word you may want to input:")
            return suggestions

def update_corrections(old_word, newword):
    try:
        diction
    except:
        diction = pd.read_csv('dictionary.csv')
    if newword in list(diction['Word']):
       for i in range(0,len(diction['Word'])):
           if newword == diction['Word'][i]:
              diction['Times_Selected'][i] = diction['Times_Selected'][i] + 1 
    else:
        newdata=pd.DataFrame([[newword,1/1086322084,'True',1,'0']],
                            columns = ['Word','Relative_Frequency','User_Created','Times_Selected','Frequency'])
        newdiction = diction.append(newdata,ignore_index = True)
    newdiction.to_csv(r'newdiction.csv')
