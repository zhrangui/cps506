immutable_array = (1, 2, 3, 4, 5, 6) #Immutability
def square(immutable): #Pure Functions
    result = map(lambda x: pow(x, 2), immutable) ##Pure Functions
    return result
print(list(square(immutable_array)))