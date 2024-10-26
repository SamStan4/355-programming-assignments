def partition(a : list, l : int, r : int, comp) -> int:
    p = a[l]
    while l < r:
        if comp(a[l], a[r]):
            a[l], a[r] = a[r], a[l]
        if a[l] == p:
            r -= 1
        else:
            l += 1
    return l

def quick_sort_helper(a : list, l : int, r : int, comp) -> None:
    if l < r:
        pivot_idx : int = partition(
            a = a,
            l = l,
            r = r,
            comp = comp
        )
        quick_sort_helper(
            a = a,
            l = l,
            r = pivot_idx - 1,
            comp = comp
        )
        quick_sort_helper(
            a = a,
            l = pivot_idx + 1,
            r = r,
            comp = comp
        )
        
def quick_sort_ascending(a : list) -> None:
    quick_sort_helper(
        a = a,
        l = 0,
        r = len(a) - 1,
        comp = lambda x, y : x > y
    )
    
def quick_sort_descending(a : list) -> None:
    quick_sort_helper(
        a = a,
        l = 0,
        r = len(a) - 1,
        comp = lambda x, y : x < y
    )