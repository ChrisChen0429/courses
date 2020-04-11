# Copyright 2017, 2013, 2011 Pearson Education, Inc., W.F. Punch & R.J.Enbody
# Modified by Donald F. Ferguson, Columbia University, 2018


# Import some frameworks that help us implement a web application.
from flask import Flask, request, make_response

import string
import pandas as pd
import math
# The external file that implements my check and track correction functions.
import correct_functions_new as correct_functions
import json


##############################################################################################################
# These are the two functions you will write.
# You will implement in a separate Python file and access via an import statement.

# 1. Check a dictionary to determine if word is correctly spelled.
# 2. If not, call a set of functions that generate "near by, correctly spelled words."
def check_word(word):
    return correct_function.check_and_suggest_word(word)
    
    # second function for generate insertion
def update_corrections(original_word, corrected_word):
    # Your code goes here.
    return correct_functions.update_corrections(original_word, corrected_word)
    
    
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


    
    return correct_functions.check_and_suggest_word(word)


# The user selected a correction, or entered a new correct spelling.
# We will record the correct spelling and score as a possible common correction for user.
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



# End of where your code will go.
##############################################################################################################


#
# The main running program is the Flask application engine.
# We register URL endpoint functions below.
# There are some static content directories for serving HTML, CSS, JavaScript, etc.
#
app = Flask(__name__,
            static_url_path='',
            static_folder='web/static',
            template_folder='web/templates')


#
# Implements the path /api/check
# The only web method is GET.
# The query parameters are: word=xxx, where xxx is the word to check.
#
@app.route('/api/check', methods=['GET'])
def api_check():
    print("input = ", str(request.args))
    w = request.args['word']
    print("Input word = ", w)

    result = check_word(w)

    print("check_word rsp = ", result)

    if result == True:
        rsp = json.dumps({"status" : "CORRECT"})
        response = make_response(rsp)
    else:
        rsp = json.dumps({ "status" : "INCORRECT", "original": w, "suggestions": result })
        print("rsp = ", rsp)
        response = make_response(rsp)

    response.headers['Content-Type'] = 'application/json'

    return response


#
# Implements the path /api/check
# The only web method is POST.
# The body is JSON and has two elements:
# 1. original_word: the word the user checked.
# 2. corrected_word: the word the user submitted as a correction.
#
@app.route('/api/correct', methods=['POST'])
def api_correct():

    d = request.data
    print("d = ", d)
    d = json.loads(d)


    w = d['original_word']
    c = d['corrected_word']

    #print("original = " + w)
    #print("corrected = " + c)
    result = update_corrections(w, c)

    rsp = { "msg": result }
    bar = json.dumps(rsp)
    response = make_response(bar)
    response.headers['Content-Type'] = 'application/json'
    print("returning")
    return response

if __name__ == '__main__':
    app.run(debug=True)
