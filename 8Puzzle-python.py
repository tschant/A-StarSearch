import heapq
from copy import deepcopy
import time

#this is used to reference a list within a list easily
#also used for the Manhattan Distance
coords = [[0, 0], [0, 1], [0, 2],
          [1, 0], [1, 1], [1, 2],
          [2, 0], [2, 1], [2, 2]]
#simple abstraction to check is the current state is at the goal
def goal(state, goal_state):
    if state == goal_state:
        return True
    else:
        return False
#rudimentary implementation of a hash table
#mainly used to check if a move has been explored
def hash_add(dct, lst):
    temp_bool = True
    #checks the entire hash list if the move is already in the hash
    for x in range(len(dct)):
        #the move was found in the hash table, it should be ignored
        if dct[x] == lst:
            temp_bool = False
    return [temp_bool, lst]
#print the solution that was found, displaying the action, list, and the total weight to get there
def print_solution(node):
    if node.action == "Start":
        print "Action: " + str(node.action) + " List: " + str(node.state) + \
              " Weight: " + str(node.g) + ", " + str(node.h) + ", " + str(node.f)
    else:
        #uses recursion to in order to print the start state first
        print_solution(node.prev)
        print "Action: " + str(node.action) + " List: " + str(node.state) + \
              " Weights: " + str(node.g) + ", " + str(node.h) + ", " + str(node.f)

def A(curState, goal):
    #measures the execution time
    start = time.time()
    #create a priority queue to sort the states
    pq = PriorityQueue()
    #Adds the start state to the Priority queue
    nodes = Node(curState, [], "Start", 0, man_dist(curState), man_dist(curState))
    pq.__add__(nodes, nodes.f)
    #also add the start state to the hash list
    hash.append(curState)
    #checks if the puzzle can be solved
    if solvable(curState)%2 == 0:
        #loop this section
        while True:
            if pq.__len__() > 0:
                s = pq.remove()
                #check if at the goal state
                if s.state == goal:
                    end = time.time()
                    print "Time Elapsed= " + str(end-start) + " seconds"
                    print_solution(s)
                    break
                else:
                    #find all possible moves
                    #returns as a list of moves in node form
                    list_moves = move(s)
                    for x in range(len(list_moves)):
                        #check the hash list
                        chk, lst_add = hash_add(hash, list_moves[x].state)
                        #if the move was not in the hash list, it is added to the queue and hash
                        if chk:
                            #implemnted hash table as a list function
                            hash.append(lst_add)
                            pq.__add__(list_moves[x], list_moves[x].f)
    else:
        end = time.time()
        print "Time Elapsed= " + str(end-start) + " seconds"
        print "The list " + str(curState) + " cannot be solved."
#instead of trying to solve an insolvable puzzle
#this function will determine if the current list is able to be solved
def solvable(start):
    #a flat list works better to check
    flat_start = [num for elem in start for num in elem]
    #0 does not count for this test
    flat_start = [x for x in flat_start if x is not 0]
    total = 0
    for x in range(len(flat_start)):
        for y in range(x+1, len(flat_start)):
            #for every number to the right that is
            #less than the current number add 1
            if flat_start[x] < flat_start[y]:
                total += 1
    #if the number returned is even, the puzzle is solvable
    return total
#find the manhattan distance
def man_dist(lst):
    #makes the list flat and finds the location of every number 1 to 8
    location_lst = map(lambda x: [num for elem in lst for num in elem].index(x), [1, 2, 3, 4, 5, 6, 7, 8])
    #the location of the goal numbers
    location_goal = [0, 1, 2, 3, 4, 5, 6, 7]
    total = 0
    #takes the location of the current list and uses the coordinates
    #and subtracts the goal location coord (use the abs so there will be no negative answer)
    for x in range(len(location_lst)):
        total += (abs(coords[location_lst[x]][0] - coords[location_goal[x]][0]) +
                  abs(coords[location_lst[x]][1] - coords[location_goal[x]][1]))
    return total
#swap function for the tiles
def swap(st, node, dir, x1, y1, x2, y2):
    temp = node.state[x1][y1]
    st[x1][y1] = node.state[x2][y2]
    st[x2][y2] = temp
    mandist = man_dist(st)
    return Node(st, node, [node.state[x2][y2], dir], node.g+1, mandist, node.g+mandist+1)
def move(node):
    #create an empty list
    nodes = []
    #copies the values and not a refrence to original state
    s = deepcopy(node.state)
    #finds the location fo the '0' tile
    index = [num for elem in node.state for num in elem].index(0)
    #create a short hand notation for coordinate referencing for the '0'
    coordX1 = coords[index][0]
    coordY1 = coords[index][1]
    #check if right is a valid move
    if index+1 < 9 and coordX1 == coords[index+1][0]:
        nodes.append(swap(s, node, 'L', coordX1, coordY1, coords[index+1][0], coords[index+1][1]))
        #need to recopy the original list
        s = deepcopy(node.state)
    #check if left is a valid move
    if index-1 >= 0 and coordX1 == coords[index-1][0]:
        nodes.append(swap(s, node, 'R', coordX1, coordY1, coords[index-1][0], coords[index-1][1]))
        s = deepcopy(node.state)
    #check if up is a valid move
    if index+3 < 9 and coordY1 == coords[index+3][1]:
        nodes.append(swap(s, node, 'D', coordX1, coordY1, coords[index+3][0], coords[index+3][1]))
        s = deepcopy(node.state)
    #check if down is a valid move
    if index-3 >= 0 and coordY1 == coords[index-3][1]:
        nodes.append(swap(s, node, 'U', coordX1, coordY1, coords[index-3][0], coords[index-3][1]))
    #returns the list of valid moves
    return nodes

#struct of nodes
class Node(object):
    def __init__(self, state, prev, action, g, h, f):
        self.g = g  #cost of the move (always 1 for 8-tile)
        self.h = h  #cost of using heuristic only
        self.f = f  #cost of g and h combined
        self.state = state
        self.prev = prev
        self.action = action
#Priority Queue implementation
class PriorityQueue(object):
    def __init__(self):
        self.l = []
    def __len__(self):
          return len(self.l)
    def __add__(self, node, k):
        heapq.heappush(self.l, (k, node))
    def remove(self):
        return heapq.heappop(self.l)[-1]

#UI for user to experiment with
prompt = "> "
#test lists
test1 = [[1, 2, 3],
        [4, 6, 5],
        [8, 7, 0]]
test2 = [[6, 4, 2],
         [1, 5, 3],
         [7, 0, 8]]
test3 = [[6, 4, 2],
         [8, 5, 3],
         [1, 0, 7]]
test4 = [[6, 4, 7],
         [8, 5, 0],
         [3, 2, 1]]
test5 = [[8, 0, 7],
         [6, 5, 4],
         [3, 2, 1]]
#test impossible list
#takes to long to finish without the "solvable" function
impossible = [[1, 2, 3],
               [4, 5, 6],
               [8, 7, 0]]
goallst = [[1, 2, 3],
           [4, 5, 6],
           [7, 8, 0]]
print "A* Search Algorithm: Using Manhattan Distance heuristic"
while True:
    #used to determine if move has been made, also reset after each run
    hash = []
    print "Choose or Input a list: "
    print "1. " + str(test1)
    print "2. " + str(test2)
    print "3. " + str(test3)
    print "4. " + str(test4)
    print "5. " + str(test5)
    print "6. " + str(impossible)
    #print "4. Input List"
    print "7. End"
    ans = raw_input(prompt)
    if ans == "1":
        A(test1, goallst)
    elif ans == "2":
        A(test2, goallst)
    elif ans == "3":
        A(test3, goallst)
    elif ans == "4":
        A(test4, goallst)
    elif ans == "5":
        A(test5, goallst)
    elif ans == "6":
        A(impossible, goallst)
    else:
        break
    raw_input("Press Enter to continue.")
