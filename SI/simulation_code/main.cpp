#include "system.h"
#include <fstream>
#include <string>

using namespace std;

int main(int argc, char* argv[])
{
    if (argc != 4)
    {
        cout << "ERROR - SI network program requires 3 arguments:" << endl;
        cout << "1) Network size, N (integer)" << endl;
        cout << "2) Transmission rate, b (double)" << endl;
        cout << "3) Intial number of infected, I_init (integer)" << endl;
        return 0;       
    }

    int N = stoi(argv[1]);          // Network size
    double b = stod(argv[2]);       // Transmission rate
    int I_init = stoi(argv[3]);     // Initial number of infected
    // char o = argv[4];               // Output type

    char o = 'a';       // Output type
                        // a: aggregate
                        // s: all states

    // cout << N << '\t' << b << '\t' << I_init << endl;

    ofstream outfile("./output/output.txt");
    srand((unsigned int)time(NULL));


    for (int i = 0; i < 500; i++)
    {
        // Set up graph initial state
        graph g(N);
        g.make_complete();
        g.set_state(0);
        for (int j = 0; j < I_init; j++)
        {
            g.set_state((i+j)%N,1);
        }

        // Make SI network model
        si_net m(b,g);

        // Make time-dependent system based on model 
        mysystem s(m);

        // Run Gillespie algorithm for system
        s.Gillespie(outfile,o);
    }

    return 0;
}
