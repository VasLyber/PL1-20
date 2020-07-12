#!/usr/bin/env python
import sys


def push(queue_input, queue_output, vaccine_string):  # push function implementation
    last_base = queue_input.pop()
    queue_output.append(last_base)
    vaccine_string += "p"
    return vaccine_string


def complement(queue_input, vaccine_string):  # complement function implementation
    for i in range(len(queue_input)):
        if queue_input[i] == "G":
            queue_input[i] = "C"
        elif queue_input[i] == "C":
            queue_input[i] = "G"
        elif queue_input[i] == "A":
            queue_input[i] = "U"
        else:
            queue_input[i] = "A"
    vaccine_string += "c"
    return vaccine_string


def reverse(queue_output, vaccine_string):  # reverse function implementation
    queue_output.reverse()
    vaccine_string += "r"
    return vaccine_string


def string_to_queue(input_string):  # load string into queue list
    queue_input = []
    for char in input_string:
        queue_input.append(char)
    return queue_input


def take_action(queue_input, queue_output, vaccine_string, action_id):
    if action_id == 0:
        vaccine_string = complement(queue_input, vaccine_string)
    elif action_id == 1:
        vaccine_string = push(queue_input, queue_output, vaccine_string)
    elif action_id == 2:
        vaccine_string = reverse(queue_output, vaccine_string)
    return vaccine_string


def check_completion(queue_input, queue_output):
    temp_queue_output = [i for i in queue_output]  # create deep copy of the queue
    i = 0
    while i < len(temp_queue_output) - 1:  # delete consecutive duplicates from temporary queue
        if temp_queue_output[i] == temp_queue_output[i + 1]:
            del temp_queue_output[i]
        else:
            i += 1

    if len(temp_queue_output) == len(set(temp_queue_output)):  # check for duplicates in the temporary queue
        if not queue_input:
            return 2  # vaccine string is completed
        return 1  # vaccine string should be developed further
    return 0  # vaccine string should be ingored/deleted


def initialize_vaccine_string_list(vaccine_string_list, queue_input_list, queue_output_list, queue_input_initial):
    queue_input = [x for x in queue_input_initial]
    queue_output = []
    take_action(queue_input, queue_output, "", 0)
    vaccine_string_list.append("c")
    queue_input_list.append(queue_input)
    queue_output_list.append(queue_output)

    queue_input = [x for x in queue_input_initial]
    queue_output = []
    take_action(queue_input, queue_output, "", 1)
    vaccine_string_list.append("p")
    queue_input_list.append(queue_input)
    queue_output_list.append(queue_output)

    queue_input = [x for x in queue_input_initial]
    queue_output = []
    take_action(queue_input, queue_output, "", 2)
    vaccine_string_list.append("r")
    queue_input_list.append(queue_input)
    queue_output_list.append(queue_output)


vaccine_string = ""  # initialization of output string
maximum_input_length = 100  # maximum length for each file line
maximum_vaccine_string_length = 100 * 3  # maximum length for possible vaccine, worst case is complement, reverse and
# push for a single base
with open(sys.argv[1], 'r') as my_file:  # open argument file
    lines = my_file.readlines()  # read all file lines
    N = int(lines[0])  # save the number of lines that include RNA bases
    for i in range(1, N + 1):  # loop through all lines with RNA bases
        queue_input_list = []
        queue_output_list = []
        optimal_vaccine_string = ""
        vaccine_string_list = []
        input_string = lines[i][:-1]  # RNA bases string
        queue_input_initial = string_to_queue(input_string)  # import the string to the input queue
        initialize_vaccine_string_list(vaccine_string_list, queue_input_list, queue_output_list, queue_input_initial)

        while True:
            for j in range(len(vaccine_string_list)):
                string = vaccine_string_list.pop(0)  # delete the first string len(str_list) times
                queue_input_original = queue_input_list.pop(0)
                queue_output_original = queue_output_list.pop(0)
                for action in range(0, 3):  # loop through all 3 possible actions (complement, push, reverse)
                    if (action == 0 and string[-1] == 'c') or (action == 2 and string[-1] == 'r') or \
                            (action == 0 and string[-1] == 'r'):  # cut excessive actions
                        continue
                    queue_input = [x for x in queue_input_original]
                    queue_output = [x for x in queue_output_original]
                    vaccine_string = take_action(queue_input, queue_output, string, action)  # take the desired action
                    code = check_completion(queue_input, queue_output)  # check if task is done
                    if code == 2:
                        break
                    elif code == 1:
                        vaccine_string_list.append(vaccine_string)
                        queue_input_list.append(queue_input)
                        queue_output_list.append(queue_output)
                else:
                    continue
                break
            else:
                continue
            break
        print(vaccine_string)
