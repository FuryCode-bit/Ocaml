def knapsack(W, items):
    """
    Returns the maximum value that can be put in a knapsack of capacity W
    using items with weights wt and values val, along with the sum of objects selected.
    """
    # Sort items by value-to-weight ratio in decreasing order
    items = sorted(items, key=lambda x: (x[0]/x[1], -x[1]))
    print(items)

    # Initialize variables
    max_value = 0
    sum_objects = 0

    # Iterate over the items and add them to the knapsack if their weight is less than or equal to the remaining capacity
    for i in range(len(items)):
        weight, value = items[i]
        print(f"weight: {weight}, value: {value}, max_value: {max_value} - W: {W}")
        if W >= weight:
            W -= weight
            max_value += value
            sum_objects += 1
            print(f"Sol: {weight}, value: {value}, max_value: {max_value} - W: {W}")
        elif W == 0:
            break
        else:
            # If the remaining capacity is less than the weight of the current item, pick the item that occupies the full capacity
            if W == weight:
                W -= weight
                max_value += value
                sum_objects += 1
                print(f"Sol: {weight}, value: {value}, max_value: {max_value} - W: {W}")
            else:
                continue

    return max_value, sum_objects


items = [(1, 2), (2, 3), (3, 8), (5, 12), (6, 17), (7, 17), (8, 20)]; W = 8

# items = [(1, 2), (2, 4), (4, 8)]; W = 4

# items = [(1, 1), (2, 3), (3, 5), (4, 7)]; W = 7

# items = [(1, 20), (2, 2), (3, 5), (5, 10), (10, 20)]; W = 10

# items = [(1, 1), (3, 5), (5, 9), (10, 15), (12, 20); (15, 25)], W = 15

# items = [(1, 2), (5, 10), (10, 20), (15, 30), (20, 40)]; W = 20

# items = [(1, 1), (4, 7), (8, 13), (12, 18)]; W = 12

# items = [(1, 2), (3, 7), (6, 12), (9, 16)]; W = 9

# items = [(1, 1), (3, 5), (6, 8)]; W = 6

# items = [(1, 2), (5, 8), (9, 13), (11, 18), (13, 22)]; W = 13

# items = [(2, 2), (5, 7), (8, 11), (11, 14)]; W = 11
max_value, sum_objects = knapsack(W, items)
print("Maximum value:", max_value)
print("Sum of objects selected:", sum_objects)