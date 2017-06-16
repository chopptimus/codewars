def expanded_form(num):
    decimal_places = []
    str_num = str(num)
    expanded_str = ""
    for dp, value in enumerate(str_num):
        expanded_str += str(int(value) * 10**(len(str_num) - 1 - dp)) + " + "
        
    return expanded_str[:-3]
   
print(expanded_form(12))
