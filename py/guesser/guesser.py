#!/usr/bin/python3

import re
from collections import OrderedDict

def main():
    while 1:
        letters = input('letters: ')
        length = input('word length: ')

        letters = set(letters)

        words = possibleWords(letters, length)
        print(words)
        prob = probabilities(words, letters)
        print(prob)

        print(OrderedDict(sorted(prob.items(), key=lambda x: x[1], reverse=True)))

def possibleWords(lettersSet, length):
    letters = ''
    for letter in lettersSet:
        letters += letter
    reg = re.compile('^[' + letters + ']{' + str(length) + '}$');
    words = []
    f = open('/usr/share/dict/words')
    for line in f:
        if reg.match(line):
            words.append(line.strip('\n'))

    return words

def probabilities(words, letters):
    count = {}
    probability = {}
    for letter in letters:
        count[letter] = 0
        probability[letter] = 0

    for letter in letters:
        for word in words:
            if word.find(letter) >= 0:
                count[letter] += 1

    for letter in letters:
        probability[letter] = count[letter]/len(words)

    return probability

        
main()
