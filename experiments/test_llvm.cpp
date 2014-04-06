#include <stdio.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#define DATA 10
#define DIMS 5
#define VALS 10
#define LABELS 10
#define COUNTS (LABELS + LABELS * DIMS * VALS)
#define SIZE (COUNTS * sizeof(int))
using std::cout;
using std::getline;
using std::ifstream;
using std::istringstream;
using std::string;

inline void init(string file, char **data, int *counts) {

    ifstream f(file.c_str());
    if (f.is_open()) {
        string line;
        int n;
        char c;
        for (int i = 0; i < DATA && getline(f, line); i++) {
            istringstream buff(line);
            data[i] = new char[DIMS + 1];
            for (int j = 0; j < DIMS; j++) {
                buff >> n >> c;
                data[i][j] = (char) n;
            }
            buff >> n;
            data[i][DIMS] = (char) n;
        }
        f.close();
    }
}

void naive_bayes(char *data, int *counts, int dims, int vals, int labels){
   char label = data[dims];
    ++counts[label];
    int offset = labels + label * dims * vals;
    for (int j = 0; j < dims; j++)
        ++counts[offset + j * vals + data[j]];
}


int main() {

    char **data = new char*[DATA];
    int *counts = new int[COUNTS];
    string file = "test.tmp"; 
    init(file, data, counts);
    printf("data=%i, dims=%i, vals=%i, labels=%i\n", DATA, DIMS, VALS, LABELS);
    for (int i = 0; i < DATA; i++)
        naive_bayes(data[i], counts, DIMS, VALS, LABELS);

    printf("data:%p\n", data);
}
