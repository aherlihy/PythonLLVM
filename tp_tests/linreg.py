#for (int i = 0; i < DATA; i++) {
#            float dot = 0;
#            for (int j = 0; j < DIMS; j++)
#                dot += d[i][j] * w[j];
#            float label = d[i][DIMS];
#            dot *= -label;
#            for (int j = 0; j < DIMS; j++)
#                g[j] += dot * d[i][j];
#}


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
def main():
#    data = range(100.0)
#    w = range(100.0)
#    g = range(0.0,100.0)
#    linreg(data, w, g, 100)
    x = 1
