immutable_array = (1, 2, 3, 4, 5, 6)
def square(immutable):
    result = map(lambda x: pow(x, 2), immutable)
    return result
print(list(square(immutable_array)))