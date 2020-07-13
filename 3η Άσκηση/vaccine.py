#!/usr/bin/env python
import sys
import timeit
import _pickle as cPickle
from collections import deque
from collections import Counter
from copy import deepcopy, copy

actions = ['c', 'p', 'r']
total_start = timeit.default_timer()


class State:
    def __init__(self, out_queue, vaccine_string, complement_flag, reverse_flag, unchanged_after_push_flag, input_queue_index):
        self.out_queue = out_queue
        self.vaccine_string = vaccine_string
        self.complement_flag = complement_flag
        self.reverse_flag = reverse_flag
        self.unchanged_after_push_flag = unchanged_after_push_flag
        self.input_queue_index = input_queue_index

    def deep_copy(self):
        out_queue = [x for x in self.out_queue]
        vaccine_string = self.vaccine_string
        complement_flag = self.complement_flag
        reverse_flag = self.reverse_flag
        unchanged_after_push_flag = False
        input_queue_index = self.input_queue_index
        return State(out_queue, vaccine_string, complement_flag, reverse_flag, unchanged_after_push_flag, input_queue_index)

    def deep_copy_reverse_complement(self):
        out_queue = self.out_queue
        vaccine_string = self.vaccine_string
        complement_flag = self.complement_flag
        reverse_flag = self.reverse_flag
        unchanged_after_push_flag = False
        input_queue_index = self.input_queue_index
        return State(out_queue, vaccine_string, complement_flag, reverse_flag, unchanged_after_push_flag,
                     input_queue_index)

    def complement(self):
        self.complement_flag = not self.complement_flag


    def reverse(self):
        self.reverse_flag = not self.reverse_flag


    def push(self):
        if not self.complement_flag:
            last_base = input_queue[-self.input_queue_index]

        else:
            last_base = input_queue_complement[-self.input_queue_index]
        self.input_queue_index += 1

        if not self.reverse_flag:
            if len(self.out_queue) > 0:
                if self.out_queue[-1] != last_base:
                    self.out_queue.append(last_base)
                else:
                    self.unchanged_after_push_flag = True
            else:
                self.out_queue.append(last_base)
        else:
            if len(self.out_queue) > 0:
                if self.out_queue[0] != last_base:
                    self.out_queue.insert(0, last_base)
                else:
                    self.unchanged_after_push_flag = True
            else:
                self.out_queue.insert(0, last_base)

    def take_action(self, action):
        if action == 'c':
            self.complement()
        elif action == 'p':
            self.push()
        elif action == 'r':
            self.reverse()
        self.vaccine_string += action
        return self

    def accessible_initial(self):
        temp = self.deep_copy()
        return temp.take_action('p')

    def accessible_secondary(self):
        for action in ['c', 'p']:
            if len(self.vaccine_string) > 0:
                if (action == 'c' and self.vaccine_string[-1] == 'c') or (
                        action == 'r' and self.vaccine_string[-1] == 'r') \
                        or (action == 'c' and self.vaccine_string[-1] == 'r'):  # cut excessive actions
                    continue
            temp = self.deep_copy()
            yield temp.take_action(action)

    def accessible(self):
        for action in actions:
            if len(self.vaccine_string) > 0:
                if (action == 'c' and self.vaccine_string[-1] == 'c') or (
                        action == 'r' and self.vaccine_string[-1] == 'r') \
                        or (action == 'c' and self.vaccine_string[-1] == 'r'):  # cut excessive actions
                    continue
            if action == 'p':
                temp = self.deep_copy()
            else:
                temp = self.deep_copy_reverse_complement()
            yield temp.take_action(action)

    def check_action(self):
        if len(self.out_queue) == len(set(self.out_queue)):  # check for duplicates in the temporary queue
            if input_queue_length == self.input_queue_index - 1:
                return 2  # vaccine string is completed
            return 1  # vaccine string should be developed further
        return 0  # vaccine string should be ingored/deleted

    def get_vaccine_string(self):
        return self.vaccine_string


def string_to_queue(string):  # load string into queue list
    queue = []
    for char in string:
        queue.append(char)
    return queue


def complement(queue):  # find the complement input queue, only needed once per experiment
    queue_complement = []
    for i in range(len(queue)):
        if queue[i] == "G":
            queue_complement.append("C")
        elif queue[i] == "C":
            queue_complement.append("G")
        elif queue[i] == "A":
            queue_complement.append("U")
        else:
            queue_complement.append("A")
    return queue_complement


def callback():
    init = State([], "", False, False, False, 1)
    init_p = init.accessible_initial()
    Q = deque([init_p])
    secondary_flag = True
    done = False
    while Q or not done:
        s = Q.popleft()
        internal_Q = deque()
        if secondary_flag:
            secondary_flag = not secondary_flag
            for t in s.accessible_secondary():
                if t.check_action() == 2:
                    return t
                elif t.check_action() == 1:
                    if t.unchanged_after_push_flag:
                        Q.append(t)
                        break
                    else:
                        internal_Q.append(t)
            Q += internal_Q
        else:
            for t in s.accessible():
                if t.check_action() == 2:
                    return t
                elif t.check_action() == 1:
                    if t.unchanged_after_push_flag:
                        append_flag = True
                        for o in Q:
                            if o.reverse_flag == t.reverse_flag and o.complement_flag == t.complement_flag and o.input_queue_index == t.input_queue_index and o.out_queue == t.out_queue:
                                append_flag = False
                                break
                        if append_flag:
                            Q.append(t)
                        break
                    else:
                        internal_Q.append(t)
            Q += internal_Q


with open(sys.argv[1], 'r') as my_file:  # open argument file
    lines = my_file.readlines()  # read all file lines
    N = int(lines[0])  # save the number of lines that include RNA bases
    for i in range(1, N + 1):  # loop through all lines with RNA bases
        start = timeit.default_timer()
        input_string = lines[i][:-1]  # RNA bases string
        input_queue = string_to_queue(input_string)  # import the string to the input queue
        input_queue_complement = complement(input_queue)  # find the complement input queue
        input_queue_length = len(input_queue)
        t = callback()
        end = timeit.default_timer()
        print(t.vaccine_string)
total_end = timeit.default_timer()
