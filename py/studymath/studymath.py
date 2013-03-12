#!/usr/bin/python3

class Iterator: # {{{
    def __init__(self):
        self.subquestion = 0
        self.question = 0
        self.storage = {}

        while self.question == 0:
            try:
                self.question = int(input('Begin at: '))
            except ValueError:
                continue


    def i(self, i):
        if i == '':
            self.question += 1
            self.subquestion = 0
            return ''
        elif i[0] == 'p':
            if self.question != 1:

                try:
                    size = len(self.storage[self.question-1])
                    self.subquestion = size
                    self.question -= 1
                except KeyError:
                    self.subquestion = 0
                    self.question -= 1

            return ''
        elif i[0] == 'd':
            if self.subquestion > 0:
                self.storage[self.question].pop()
                self.subquestion -= 1
                return ''
        elif i[0] == 'r':
            returnstring = ''
            for key,value in self.storage.items():
                returnstring += '\n' + str(key)
                
                for r in value:
                    returnstring += '\n\t%s'%r
            return(str(returnstring))
        else:
            try:
                result = int(i)

                try:
                    self.storage[self.question].append(result)
                except KeyError:
                    self.storage[self.question] = []
                    self.storage[self.question].append(result)

                self.subquestion += 1
                return ''
            except ValueError:
                return 'not valid.'
# }}}


if __name__ == "__main__":
    iterator = Iterator()

    while True:
        instr = str(iterator.question) + ':' + str(iterator.subquestion) + '> '
        print(iterator.i(input(instr)))
