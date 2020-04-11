class dictionary_nonoo(object):
    
    def __init__(self,fn,result,key_column):
        self.fn = fn
        self.result = result
    
    
    def load(self,fn,key_column):
        # Your Code goes here
        import csv
        with open(fn) as f:
            f_csv = f.readlines()
            col_name = f_csv[0].replace("\n","").split(",")
            data = {}
            for i in col_name:
                input_file = csv.DictReader(open(fn))
                this_col = []
                for row in input_file:
                    this_col.append(row[i])
                data[i] = this_col

        result = {}
        for i in data[key_column]:
            index = data[key_column].index(i)
            value = {}
            for j in data.keys():
                value[j] = data[j][index]
            result[i] = value
        return result


    # Get an entry
    def get_entry(self,k):
        # Your Code goes here
        return result['k']

    # Add a new entry
    def add_entry(self,k, v):
        # Your Code goes here
        result[k] = v
        
    # Update the value of an entry
    def set_entry(self,k, v):
        # Your Code goes here
        result[k] = v

    # Get the value associated with key c in dictionary entry associated with k
    def get_cell(self,k, c):
        # Your Code goes here
        return result[k][c]

    # Set the entry
    def set_cell(self,k, c, v):
        # Your Code goes here
        result[k][c] = v
    
    def save(self):
        with open(f,"w") as f1:
            write = csv.writer(f1)
            write.writerows(result)
    
    # Json
    def to_str(self):
        import json
        result1 = json.dumps(result)
        return result1
