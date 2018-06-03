import re

def toAscii85(data):
    encoding = [encode(data[i:i + 4]) for i in range(0, len(data), 4)]
    return '<~%s~>' % ''.join(encoding)

def encode(chunk):
    if chunk == chr(0) * 4:
        return 'z'

    padded = chunk.ljust(4, chr(0))
    binary_string = ['{0:08b}'.format(ord(char)) for char in padded]
    int_chunk = int(''.join(binary_string), 2)

    encoding = ''
    for _ in range(5):
        encoding += chr(int_chunk % 85 + 33)
        int_chunk //= 85

    return encoding[-len(chunk) - 1:][::-1]

def fromAscii85(data):
    data = data.replace('z', '!!!!!')
    data = re.sub(' |\n|\t|\r', '', data)
    data = re.match('^<~(.*)~>$', data).group(1)
    decoding = [decode(data[i:i + 5]) for i in range(0, len(data), 5)]
    return ''.join(decoding)

def decode(chunk):
    padded = chunk.ljust(5, 'u')

    total = 0
    for i, char in enumerate(padded):
        total += 85**(4 - i) * (ord(char) - 33)

    codes = [total >> i & 255 for i in range(24, -1, -8)]
    decoding = ''.join([chr(code) for code in codes])

    for char in decoding[:len(chunk) - 1]:
        if ord(char) > 255:
            return ''

    return decoding[:len(chunk) - 1]
