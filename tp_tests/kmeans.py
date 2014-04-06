def kmeans(i=int, data=listi100, x=listi100, y=listi100, c=listi100, t=listi100, CENT=int):
    
    xd0 = c[0] - x[i]
    xd1 = c[1] - x[i]
    xd2 = c[2] - x[i]
    yd0 = c[3] - y[i]
    yd1 = c[4] - y[i]
    yd2 = c[5] - y[i]
    d0 = sqrt(xd0 * xd0 + yd0 * yd0)
    d1 = sqrt(xd1 * xd1 + yd1 * yd1)
    d2 = sqrt(xd2 * xd2 + yd2 * yd2)

    min = d0
    assign = 0
    if(d1<min):
        min=d1
        assign=1
    if(d2<min):
        assign=2
    t[assign]+=x[i]
    t[CENT+assign]+=y[i]
    t[2*CENT+assign]+=1


def main():
    i=100
    CENT=100
    data=range(100)
    x=range(100)
    y=range(100)
    c=range(100)
    t=range(100)
    kmeans(i, data, x, y, c, t, CENT)
