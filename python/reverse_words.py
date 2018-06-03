def reverse_words(str):
    chunks = str.split('  ')
    rev_chunks = []
    for chunk in chunks:
        words = chunk.split()
        words = [word[::-1] for word in words]
        rev_chunks.append(" ".join(words))
    return "  ".join(rev_chunks)
