extern putchard(char);

def foo(x)
    if x < 3 then x + 4 else x - 3;

def bar(x)
    for i = 1, i < 10, 1 in
        putchard(120);

def main()
    foo(3);
