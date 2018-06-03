def justify(text, width):
    words = text.split()

    breaks = [0]
    current_length = 0
    for i, word in enumerate(words):
        current_length += len(word) + 1

        if current_length - 1 > width:
            breaks.append(i)
            current_length = 0
    breaks.append(len(words))
    
    lines = [words[start:end] for start, end in zip(breaks, breaks[1:])]
    print(lines)

    justified = ''
    for line in lines[:-1]:
        justified += pad(line, width) + '\n'
    
    return justified + ' '.join(lines[-1])

def pad(words, width):
    total_length = 0
    for word in words:
        total_length += len(word)

    spaces_needed = width - total_length
    gaps = len(words) - 1

    spaces_per_gap = spaces_needed / gaps
    remaining_spaces = spaces_needed % gaps

    padded = ''
    for i, word in enumerate(words[:-1]):
        padded += word
        padded += ' ' * spaces_per_gap
        if i < remaining_spaces:
            padded += ' '

    return padded + words[-1]

print(justify('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque fringilla mauris nec ligula viverra, vestibulum placerat ligula aliquam. Nunc faucibus suscipit scelerisque. Donec tempus odio ut est sagittis ullamcorper. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam iaculis consequat urna. Nam tempus nunc quis sem commodo, ut sodales erat suscipit. Nunc id libero quis purus placerat elementum. Pellentesque id diam cursus, ornare risus sit amet, mollis nisl. Ut efficitur velit in justo interdum vestibulum. Vestibulum vel rutrum erat. Donec dignissim accumsan neque in ullamcorper. Nunc nunc nibh, pretium ut blandit vel, tristique vitae elit. Integer fermentum, nulla nec rhoncus aliquam, odio orci finibus velit, in euismod mauris tellus id enim.', 30))

