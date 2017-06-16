braces = {')': '(', ']': '[', '}': '{'}

def validBraces(string):
    if not string:
        return True

    i = 0
    first_closing = len(string)
    for brace in braces:
        j = string.find(brace)
        if j > 0 and j < first_closing:
            first_closing = j

    if first_closing == len(string):
        return False
    
    i = first_closing

    if string[i-1] != braces[string[i]]:
        return False

    if not validBraces(string[:i-1] + string[i+1:]):
        return False
    
    return True
    

print(validBraces(')'))
print(validBraces('()'))
print(validBraces('(({{[[]]}}))'))
