listf100=[]
def linreg(data=listf100, w=listf100, g=listf100, dims=int):
    dot = 0.0
    c = 0
    while c<dims:
        dot+=data[c]*w[c]
        c+=1
    label = data[dims]
    dot = dot* -label
    c2=0
    while(c2<dims):
        g[c2] += dot*data[c2]
        c2+=1
def run():
    data = range(100)
    w = range(100)
    g = range(0,100)
    linreg(data, w, g, 99)
